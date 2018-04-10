/*
 * Copyright 2017 CETIC www.cetic.be
 * This is being developed for the TANGO Project: http://tango-project.eu
 *
 * Placer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Placer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License  for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License along with Placer.
 * If not, see http://www.gnu.org/licenses/lgpl-3.0.en.html
 *
 */

package placerT.algo

import oscar.cp.core.variables.CPIntVar
import oscar.cp.modeling.Constraints
import oscar.cp.{multiobjective, _}
import placerT.algo.hw._
import placerT.algo.sw.{CPTask, CPTransmission}
import placerT.metadata.hw._
import placerT.metadata.sw._
import placerT.metadata.{MappingProblem, _}

import scala.collection.immutable.SortedSet

case class MapperConfig(maxDiscrepancy:Int,
                        timeLimit:Int,
                        lns:Boolean,
                        lnsMaxFails:Int,
                        lnsRelaxProba:Int,
                        lnsNbRelaxations:Int,
                        lnsNbRelaxationNoImprove:Int,
                        lnsCarryOnObjForMultiHardware:Int)

object Mapper {

  def findMapping(problem: MappingProblem,config:MapperConfig): Mappings = {
    problem.constraints.objective match{
      case _:MinPareto =>
        val paretoSols = multiobjective.ListPareto[Mapping](false,false) //minimize two dimensions

        for(simpleProblem <-  problem.flattenToMonoHardwareProblems) {
          val newMappings = new Mapper(simpleProblem, config, paretoSols).mappings
          for (newMapping <- newMappings) {
            paretoSols.insert(newMapping, newMapping.objValues.toIndexedSeq)
          }
        }

        Mappings(
          timeUnit = problem.timeUnit,
          dataUnit = problem.dataUnit,
          info = problem.info,
          paretoSols.toList)

      case _:Sat =>

        for(simpleProblem <- problem.flattenToMonoHardwareProblems) {
          val newMappings = new Mapper(simpleProblem, config, multiobjective.ListPareto[Mapping](false)).mappings
          if(newMappings.nonEmpty){
            require(newMappings.size == 1)
            return Mappings(
              timeUnit = problem.timeUnit,
              dataUnit = problem.dataUnit,
              info = problem.info,
              List(newMappings.head))
          }
        }

        Mappings(
          timeUnit = problem.timeUnit,
          dataUnit = problem.dataUnit,
          info = problem.info,
          List.empty)

      case _:SimpleMappingGoal =>

        val paretoSols = multiobjective.ListPareto[Mapping](false) //minimize one dimensions

        for(simpleProblem <-  problem.flattenToMonoHardwareProblems) {
          val newMappings = new Mapper(simpleProblem, config, paretoSols).mappings
          for (newMapping <- newMappings) {
            paretoSols.insert(newMapping, newMapping.objValues.toIndexedSeq)
          }
        }

        Mappings(
          timeUnit = problem.timeUnit,
          dataUnit = problem.dataUnit,
          info = problem.info,
          paretoSols.toList)
    }
  }
}

class Mapper(val problem: MappingProblemMonoHardware,config:MapperConfig,bestSolutionsSoFar:multiobjective.ListPareto[Mapping]) extends CPModel with Constraints {

  //TODO: handle BestSoFar!!

  def addDocumented(c: Constraint, origin: => String) {
    try {
      add(c)
    } catch {
      case n: NoSolutionException =>
        throw new NoSolutionException("error on constraint " + origin + "\n" + n)
    }
  }

  val softwareModel = problem.softwareModel
  val hardwareModel = problem.hardwareModel
  var nbProcessorsHWAndVirtual: Int = Int.MinValue
  val objective = problem.constraints.objective

  val store: CPStore = this.solver

  store.silent = true

  val cpProblem = postProblem(softwareModel, hardwareModel)
  val mappings = solveMappingProblem(cpProblem, objective)

  def reportProgress(startedUpTask: String): Unit = {
    this.solver.propagate()
    if (this.solver.isFailed) throw new Error("solver trivially concluded to no solution or overflowed during latest modeling step")
    println(startedUpTask)
  }

  def postProblem(softwareModel: SoftwareModel,
                  hardwareModel: HardwareModel): CPMappingProblem = {

    val summedMaxTaskDurations =
      softwareModel.simpleProcesses.map(_.maxDuration(hardwareModel.processors, problem.properties ++ hardwareModel.properties ++ softwareModel.properties)).sum

    val summedMaxTransmissionTimes =
      softwareModel.transmissions.map(flow => hardwareModel.busses.map(bus => bus.transmissionDuration(flow.size)).max).sum

    val summedMaxSwitchingTimes = hardwareModel.processors.map(_.switchingDelay).max * softwareModel.simpleProcesses.length

    val staticMaxHorizon = summedMaxTaskDurations + summedMaxTransmissionTimes + summedMaxSwitchingTimes
    val maxHorizon = softwareModel.softwareClass.maxMakespan match {
      case Some(x) =>
        staticMaxHorizon min x
      case None => staticMaxHorizon
    }

    println("staticMaxHorizon:" + staticMaxHorizon)
    println("maxHorizon:      " + maxHorizon)

    println("nbTasks:" + softwareModel.simpleProcesses.length)
    println("nbTransmissions:" + softwareModel.transmissions.length)

    reportProgress("creating processors")

    //creating the CPPRocessors
    val cpProcessorsSimple: List[CPProcessor] = hardwareModel.processors.toList.map(
      processor => processor.processorClass match {
        case m: MultiTaskPermanentTasks => new CPPermanentTaskProcessor(processor.id, processor, processor.memSize, this)
        case m: SwitchingTask => new CPSwitchingTaskProcessor(processor.id, processor, processor.memSize, processor.switchingDelay, processor.nbCore, this)
      })

    var processorIDCounter = hardwareModel.processors.size - 1

    def nextProcessorID() = {
      processorIDCounter += 1
      processorIDCounter
    }

    val sharedImplementationIDToFlattenedAndVirtualCores: Array[List[(FlattenedImplementationConcrete, CPInstantiatedPermanentFunction)]] = {
      softwareModel.sharedPermanentFunctions.map(parametricSharedFunction => {
        val flattenedImplementations: List[FlattenedImplementationConcrete] = parametricSharedFunction.implementations.toList
        flattenedImplementations.flatMap((sharedFlattenedImplementation: FlattenedImplementationConcrete) => {
          val potentialHosts = cpProcessorsSimple.filter(cpps => sharedFlattenedImplementation.canRunOn(cpps.p))
          potentialHosts.map({ case (hostPE: CPPermanentTaskProcessor) =>
            //we do not care so much about providing a bound for the number of instances since the CP propagation will find out by itself anyway
            //todo: the max number of instance is to large, find the correct bound.
            val dedicatedSwitchingPE = new CPInstantiatedPermanentFunction(nextProcessorID(), hostPE, sharedFlattenedImplementation, hostPE.p.memSize, softwareModel.simpleProcesses.length, this)
            hostPE.accumulateExecutionConstraintOnSharedImplementation(dedicatedSwitchingPE)
            (sharedFlattenedImplementation, dedicatedSwitchingPE)
          })
        })
      })
    }

    val allCPSharedFunctions: Iterable[CPInstantiatedPermanentFunction] = sharedImplementationIDToFlattenedAndVirtualCores.flatMap(_.map(_._2))

    val cpProcessorsVirtual: List[CPProcessor] = sharedImplementationIDToFlattenedAndVirtualCores.toList.
      flatMap((l: List[(FlattenedImplementation, CPInstantiatedPermanentFunction)]) => l.map(_._2))

    val cpProcessors: Array[CPProcessor] = (cpProcessorsSimple ++ cpProcessorsVirtual).toArray
    nbProcessorsHWAndVirtual = cpProcessors.length

    reportProgress("constants about adjacency")

    //instantiating constants about adjacency
    val processorToBusAdjacencyNoSelfLoop: Set[(Int, Int)] =
      hardwareModel.busses.flatMap(bus => bus.receivingFromProcessors.map(proc => (proc.id, bus.id))).toSet

    val busToProcessorAdjacencyNoSelfLoop: Set[(Int, Int)] =
      hardwareModel.busses.flatMap(bus => bus.sendingToProcessors.map(proc => (bus.id, proc.id))).toSet

    val processorIDToProcessorIAndHostedVirtualProcessorID: Array[List[Int]] = Array.tabulate(cpProcessorsSimple.length)(List(_))
    for (virtualProcesor <- cpProcessorsVirtual) {
      processorIDToProcessorIAndHostedVirtualProcessorID(virtualProcesor.p.id) = virtualProcesor.id :: processorIDToProcessorIAndHostedVirtualProcessorID(virtualProcesor.p.id)
    }

    val processorToBusToProcessorAdjacencyNoSelfLoop: Set[(Int, Int, Int)] = hardwareModel.busses.flatMap(
      bus => {
        val receivingIDs = bus.receivingFromProcessors.flatMap(receivingFrom => processorIDToProcessorIAndHostedVirtualProcessorID(receivingFrom.id))
        val sendingIDs = bus.sendingToProcessors.flatMap(sendingTo => processorIDToProcessorIAndHostedVirtualProcessorID(sendingTo.id))

        receivingIDs.flatMap(receivingID => {
          sendingIDs.flatMap(sendingID => {
            if (receivingID != sendingID) Some((sendingID, bus.id, receivingID))
            else None
          })
        })
      }).toSet

    //TODO: shared functions
    //    val selfLoopBusses = hardwareModel.processors.toList.map(
    //      proc => new CPSelfLoopBus(proc.id + hardwareModel.busses.length, proc, proc, this))


    var lastBusID: Int = hardwareModel.busses.length - 1

    def nextBusID(): Int = {
      lastBusID += 1
      lastBusID
    }

    val selfLoopBusses = processorIDToProcessorIAndHostedVirtualProcessorID.indices.flatMap(
      procID => {
        val otherProcessorsIDs = processorIDToProcessorIAndHostedVirtualProcessorID(procID)
        otherProcessorsIDs.flatMap((otherProcID) => {
          val busA = new CPSelfLoopBus(nextBusID(), cpProcessors(procID).p, cpProcessors(otherProcID).p, this)
          if (procID == otherProcID) List(busA) else List(busA, new CPSelfLoopBus(nextBusID(), cpProcessors(otherProcID).p, cpProcessors(procID).p, this))
        }
        )
      })

    val selfLoopBussesID = SortedSet.empty[Int] ++ selfLoopBusses.map(_.id)

    //TODO: shared functions
    val processorToBusToProcessorAdjacency: Set[(Int, Int, Int)] =
      processorToBusToProcessorAdjacencyNoSelfLoop ++ selfLoopBusses.map((bus: CPSelfLoopBus) => (bus.processorFrom.id, bus.id, bus.processorTo.id))

    //    println("processorToBusToProcessorAdjacency" + processorToBusToProcessorAdjacency)

    reportProgress("creating busses")
    //creating the CPbusses
    val cpBusses: Array[CPBus] = (hardwareModel.busses.toList.map(
      bus => new CPRegularBus(bus.id, bus, this)
    ) ++ selfLoopBusses).toArray

    val cpHardwareModel = new CPHardwareModel(
      cpProcessors: Array[CPProcessor],
      cpBusses: Array[CPBus],
      processorToBusToProcessorAdjacency: Set[(Int, Int, Int)],
      selfLoopBussesID: SortedSet[Int],
      store,
      maxHorizon,
      sharedImplementationIDToFlattenedAndVirtualCores)

    reportProgress("creating tasks")

    //creating the CPTasks
    val cpTasks: Array[CPTask] = softwareModel.simpleProcesses.map(
      atomicTask => new CPTask(atomicTask.id, atomicTask, atomicTask.name, cpHardwareModel, this)
    )

    reportProgress("creating transmissions")

    //creating the CPtransmissions
    val cpTransmissions: Array[CPTransmission] = softwareModel.transmissions.map(
      (flow: Transmission) => new CPTransmission(flow.id, flow,
        cpTasks(flow.source.id), cpTasks(flow.target.id),
        cpHardwareModel,
        flow.size, flow.name, flow.timing)
    )

    //creating the width var, in case needed for modulo scheduling
    val widthVar: Option[CPIntVar] =
      softwareModel.softwareClass match {
        case i: IterativeSoftware =>
          i.maxFrameDelay match {
            case Some(d) =>
              reportProgress("creating width var")
              Some(CPIntVar(0, d))
            case _ if objective.needsWidth =>
              reportProgress("creating width var")
              Some(CPIntVar(0, Int.MaxValue))
            case _ => None
          }
        case _ =>
          require(!objective.needsWidth, "you cannot ask for width related objective if not in an interative software")
          None
      }

    var widthVarList: List[CPIntVar] = List.empty

    reportProgress("registering tasks and transmissions to processors and busses")
    //registering tasks and transmissions to processors and busses
    for (bus <- cpBusses) {
      for (transmission <- cpTransmissions) {
        bus.accumulatePotentialTransmissionOnBus(transmission)
      }
      bus.close()

      widthVar match {
        case Some(w) =>
          widthVarList = bus.timeWidth :: widthVarList
        case _ => ;
      }
    }

    reportProgress("registering execution constraints per processors and task")

    for (proc <- cpProcessors) {
      //reportProgress("registering execution constraints on processor " + proc.p.name)
      for (task <- cpTasks) {
        //reportProgress("registering execution constraints processor " + proc.p.name + " for task " + task.task.name)
        proc.accumulateExecutionConstraintsOnTask(task)
      }
      widthVar match {
        case Some(w) =>
          widthVarList = proc.timeWidth :: widthVarList //TODO: add proc.temporaryStorageWidth
        case _ => ;
      }
    }
    //processors are closed in a second round because of the shared implementations.
    for (proc <- cpProcessorsVirtual) {
      proc.close()
    }

    for (proc <- cpProcessorsSimple) {
      proc.close()
    }

    widthVar match {
      case Some(w) =>
        reportProgress("posting maxFrameDelay")
        addDocumented(maximum(widthVarList.toArray, w), "iterativeSoftware.maxFrameDelay")
      case _ => ;
    }

    reportProgress("computing makeSpan on tasks")
    val taskEnds = cpTasks.map(task => task.end)
    val makeSpan = maximum(taskEnds)

    //does not work for multi-cores becauze of rounding stuff.
    val processorLoadArrayUnderApprox = Array.tabulate(cpProcessors.length)(
      (processorID: Int) => cpProcessors(processorID) match {
        case m: CPSwitchingTaskProcessor => CPIntVar(0, maxHorizon * m.nbCores)
        case _ => CPIntVar(0, maxHorizon)
      })


    //reportProgress("redundant bin-packing constraint on workload for mono task processors")
    //this one assumes adjusted minDuration per processor
    for (processorID <- cpProcessors.indices) {
      cpProcessors(processorID) match {
        case m: CPSwitchingTaskProcessor =>
          val areTaskunningOnThisProcessor = cpTasks.map(task => task.isRunningOnProcessor(processorID))
          val switchingDelay = m.switchingDelay
          val minDurationOfTaskWhenOnThisProcessor = cpTasks.map(task => task.minTaskDurationOnProcessor(processorID) + switchingDelay)
          val processorLoadVariable = processorLoadArrayUnderApprox(processorID)
          add(binaryKnapsack(areTaskunningOnThisProcessor, minDurationOfTaskWhenOnThisProcessor, processorLoadVariable))
        //          add(processorLoadVariable <= (makeSpan + switchingDelay))
        case _ => ;
      }
    }

    //this one assumes minDuration for all task on any processor
    //it only works if all processors are CPMonoTaskProcessor
    //val processorLoadArrayUnderApprox2 = Array.tabulate(cpProcessors.length)(_ => CPIntVar(0, maxHorizon))
    //add(binPacking(cpTasks.map(_.processorID),cpTasks.map(_.taskDuration.min),processorLoadArrayUnderApprox2))
    //for(load <- processorLoadArrayUnderApprox2) add(load <== makeSpan)

    reportProgress("sharedFunction log for LNS")

    val pEToTasksOnFlexible: Array[List[CPTask]] = Array.fill(cpProcessorsSimple.length)(List.empty)
    for (cpTask <- cpTasks) {
      for (implem <- cpTask.allImplementationArray) {
        implem match {
          case r: ReferenceToSharedFlattenedImplementationConcrete =>
            val peID = r.target.p.id
            pEToTasksOnFlexible(peID) = cpTask :: pEToTasksOnFlexible(peID)
          case _ => false
        }
      }
    }

    reportProgress("redundant bin-packing constraint on usage per bus")

    for (cpBus <- cpBusses) {
      cpBus match {
        case r: CPRegularBus =>
        //TODO: not sure with the endNZ and end, which one should be used?
        //          add(r.busOccupancy <= makeSpan)
        case _ => ;
      }
    }

    reportProgress("computing total energy consumption")
    val energyForEachTask = cpTasks.map(task => task.energy)
    val backgroundPower: Int = cpProcessors.map(p => p.p.constantPower.value).sum
    val energy = sum(energyForEachTask) + makeSpan * backgroundPower

    //deadline
    softwareModel.softwareClass match {
      case OneShotSoftware(Some(deadline)) =>
        reportProgress("posting deadline")
        add(makeSpan <= deadline)
      case i: IterativeSoftware =>
        i.maxMakespan match {
          case Some(deadline) =>
            reportProgress("posting deadline")
            add(makeSpan <= deadline)
          case None => ;
        }
      case _ => ;
    }

    //powerCap
    hardwareModel.powerCap match {
      case None => ;
      case Some(cap) =>
        reportProgress("posting powerCap")
        val simpleCumulativeTasks = cpTasks.toList.map(
          task => CumulativeTask(start = task.start, duration = task.taskDuration, end = task.end, amount = task.power, explanation = "power of task " + task)
        )
        CumulativeTask.postCumulativeForSimpleCumulativeTasks(simpleCumulativeTasks, CPIntVar(cap - backgroundPower), "powerCap")
    }

    //energyCap
    hardwareModel.energyCap match {
      case None => ;
      case Some(cap) =>
        reportProgress("posting energyCap")
        addDocumented(energy <= cap, "energyCap")
    }

    reportProgress("posting mapping constraints")

    for (constraint <- (problem.constraints.cl ++ hardwareModel.constraints)) {
      constraint match {
        case _: MappingObjective =>
        //skipping objectives, they will be handled later on.
        case RunOnConstraint(processor, process, value) =>
          if (value) {
            add(cpTasks(process.id).processorID === processor.id)
          } else {
            add(cpTasks(process.id).processorID !== processor.id)
          }
        case c@CoreSharingConstraint(processes, value) =>
          val processesIDSet = SortedSet.empty[Int] ++ processes.map(_.id)
          if (value) {
            //same cores all
            if (!processes.isEmpty) {
              val processorID = cpTasks(processes.head.id).processorID
              for (process <- processes) {
                add(cpTasks(process.id).processorID === processorID)
              }

              //this improves propagation
              for (transmission <- cpTransmissions) {
                if (processesIDSet.contains(transmission.from.id) && processesIDSet.contains(transmission.to.id)) {
                  transmission.isSelfLoopTransmission.assignTrue()
                }
              }
            } else {
              println("requiring same core for an empty set of tasks?")
            }
          } else {
            //different cores all
            val processesVars = processes.map(p => cpTasks(p.id).processorID)
            addDocumented(allDifferent(processesVars), c.toString)

            //this improves propagation
            for (transmission <- cpTransmissions) {
              if (processesIDSet.contains(transmission.from.id) && processesIDSet.contains(transmission.to.id)) {
                transmission.isSelfLoopTransmission.assignFalse()
              }
            }
          }
        case x@MustBeUsedConstraint(processor, value) =>

          if (value) {
            println("posting MustBeUsedConstraint(" + processor.name + ")")
            val isRunningOnProcessor: Array[CPBoolVar] = cpTasks.map(task => task.isRunningOnProcessor(processor.id))
            add(new oscar.cp.constraints.Or(isRunningOnProcessor))
          } else {
            println("posting MustNotBeUsedConstraint(" + processor.name + ")")
            for (task <- cpTasks) {
              add(task.isRunningOnProcessor(processor.id) === 0)
            }
          }
        case SymmetricPEConstraint(processors: List[ProcessingElement], breaking) =>
          if (config.lns) {
            println("symmetry among " + processors.map(_.name) + " is disabled because using LNS")
          } else {
            //TODO: check that PE are indeed symmetric
            breaking match {
              case SymmetricPEConstraintType.Workload =>
                println("breaking symmetry among " + processors.map(_.name) + " by workload")
                val processorIDs = processors.map(_.id)
                val loadVariables = processorIDs.map(processorLoadArrayUnderApprox(_))

                def makeSorted(varList: List[CPIntVar]): Unit = {
                  varList match {
                    case a :: b :: tail =>
                      add(a <= b)
                      makeSorted(b :: tail)
                    case _ => ;
                  }
                }

                makeSorted(loadVariables)

              case SymmetricPEConstraintType.LongTask =>
                println("breaking symmetry among " + processors.map(_.name) + " by longest tasks assignment")
                assert(false, "this should not be used because it only works if the selected tasks hae no additional constraints on it, such as SamePE")
                val witnessProcessorID = processors.head.id
                val tasksPotentiallyRunningOnprocessors = cpTasks.toList.filter(task => !task.isRunningOnProcessor(witnessProcessorID).isFalse)

                def breakSymmetry(taskPotentiallyRunningOnProcessors: List[CPTask], processorIDs: List[Int]) {
                  processorIDs match {
                    case Nil => ;
                    case currentProcessorID :: tail =>
                      val longestTask = taskPotentiallyRunningOnProcessors.maxBy(_.minTaskDurationOnProcessor(witnessProcessorID))

                      for (p <- tail) {
                        add(longestTask.isRunningOnProcessor(p) === 0)
                      }
                      if (tail.nonEmpty) {
                        breakSymmetry(taskPotentiallyRunningOnProcessors.filter(_.id != longestTask.id), tail)
                      }
                  }
                }

                breakSymmetry(tasksPotentiallyRunningOnprocessors, processors.map(_.id))
            }
          }
      }
    }

    reportProgress("starting search")
    CPMappingProblem(
      problem,
      hardwareModel.name,
      allCPSharedFunctions,
      cpTasks,
      cpProcessors,
      cpBusses,
      cpTransmissions,
      makeSpan,
      energy,
      widthVar,
      processorLoadArrayUnderApprox,
      pEToTasksOnFlexible)
  }



  def solveMappingProblem(problem: CPMappingProblem, goal: MappingObjective): Iterable[Mapping] = {

    if (config.lns) {
      goal match {
        case simpleGoal: SimpleMappingGoal if !(simpleGoal.isInstanceOf[Sat]) =>
          new LNSSolver(problem: CPMappingProblem, simpleGoal: SimpleMappingGoal, config: MapperConfig, solver: CPSolver, bestSolutionsSoFar: multiobjective.ListPareto[Mapping])
            .solveMappingProblemMinimizeLNS()
        case _ =>
          throw new Error("LNS can only be used for simple mapping goals, not for " + goal)
      }
    }else{
      new PureCPSolver(cpProblem: CPMappingProblem, goal: MappingObjective, config:MapperConfig, solver:CPSolver, bestSolutionsSoFar:multiobjective.ListPareto[Mapping])
        .solveMappingProblem()
    }
  }
}
