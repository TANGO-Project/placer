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

import oscar.cp
import oscar.cp._
import oscar.cp.core.variables.CPIntVar
import oscar.cp.modeling.Constraints
import placerT.algo.hw._
import placerT.algo.sw.{CPTask, CPTransmission}
import placerT.metadata.hw._
import placerT.metadata.sw._
import placerT.metadata.{MappingProblem, _}

import scala.collection.immutable.SortedSet

object Mapper {

  def findMapping(problem: MappingProblem,maxDiscrepancy:Int=20,timeLimit:Int = Int.MaxValue): Mappings = {
    // try {
    Mappings(new Mapper(problem,maxDiscrepancy:Int,timeLimit).mapping)
    // } catch{case e:oscar.cp.core.NoSolutionException => Mappings(List.empty)}
  }
}

class Mapper(val problem: MappingProblem,maxDiscrepancy:Int,timeLimit:Int) extends CPModel with Constraints {


  def addDocumented(c: Constraint,origin: =>String){
    try {
      add(c)
    }catch{
      case n:NoSolutionException =>
        throw new NoSolutionException("error on constraint " + origin + "\n" + n)
    }
  }

  val softwareModel = problem.softwareModel
  val hardwareModel = problem.hardwareModel
  val goal = problem.goal

  val store:CPStore = this.solver

  val cpProblem = postProblem(softwareModel, hardwareModel)
  val mapping = searchMappingProblem(cpProblem, goal)

  def reportProgress(startedUpTask:String): Unit ={
    this.solver.propagate()
    if(this.solver.isFailed()) throw new Error("solver trivially concluded to no solution or overflowed during latest modeling step")
    println(startedUpTask)
  }

  def postProblem(softwareModel: SoftwareModel,
                  hardwareModel: HardwareModel): CPMappingProblem = {

    val summedMaxTaskDurations =
      softwareModel.simpleProcesses.map(_.maxDuration(hardwareModel.processors, problem.properties ++ hardwareModel.properties ++ softwareModel.properties)).sum

    val summedMaxTransmissionTimes =
      softwareModel.transmissions.map(flow => hardwareModel.busses.map(bus => bus.transmissionDuration(flow.size)).max).sum

    val summedMaxSwitchingTimes = hardwareModel.processorClasses.map(_ match {
      case m: MultiTaskPermanentTasks => 0
      case m: MonoTaskSwitchingTask => m.switchingDelay
    }).max * softwareModel.simpleProcesses.length

    val staticMaxHorizon = summedMaxTaskDurations + summedMaxTransmissionTimes + summedMaxSwitchingTimes
    val maxHorizon = softwareModel.softwareClass.maxMakespan match{
      case Some(x) =>
        staticMaxHorizon min x
      case None => staticMaxHorizon
    }

    println("staticMaxHorizon:" + staticMaxHorizon)
    println("maxHorizon:      " + maxHorizon)

    println("nbTasks:" + softwareModel.simpleProcesses.length)
    println("nbTransmissions:" + softwareModel.transmissions.length)

    reportProgress("creating tasks")

    //creating the CPTasks
    val cpTasks: Array[CPTask] = softwareModel.simpleProcesses.map(
      process => new CPTask(process.id, process, process.name, this, maxHorizon)
    )

    reportProgress("creating processors")

    //creating the CPPRocessors
    val cpProcessors = hardwareModel.processors.map(
      processor => processor.processorClass match {
        case m: MultiTaskPermanentTasks => new CPMultiTaskProcessor(processor.id, processor, processor.memSize, this)
        case m: MonoTaskSwitchingTask => new CPMonoTaskProcessor(processor.id, processor, processor.memSize, m.switchingDelay, this)
      })

    reportProgress("constants about adjacency")

    //instantiating constants about adjacency
    val processorToBusAdjacencyNoSelfLoop: Set[(Int, Int)] =
      hardwareModel.busses.flatMap(bus => bus.receivingFromProcessors.map(proc => (proc.id, bus.id))).toSet

    val busToProcessorAdjacencyNoSelfLoop: Set[(Int, Int)] =
      hardwareModel.busses.flatMap(bus => bus.sendingToProcessors.map(proc => (bus.id, proc.id))).toSet

    val processorToBusToProcessorAdjacencyNoSelfLoop: Set[(Int, Int, Int)] = hardwareModel.busses.flatMap(
      bus => bus.receivingFromProcessors.flatMap(
        receivingFrom => bus.sendingToProcessors.flatMap(
          sendingTo =>
            if (receivingFrom != sendingTo) Some((receivingFrom.id, bus.id, sendingTo.id))
            else None))).toSet

    val selfLoopBusses = hardwareModel.processors.toList.map(
      proc => new CPSelfLoopBus(proc.id + hardwareModel.busses.length, proc, this))

    val selfLoopBussesID = SortedSet.empty[Int] ++ selfLoopBusses.map(_.id)

    val processorToBusToProcessorAdjacency: Set[(Int, Int, Int)] =
      processorToBusToProcessorAdjacencyNoSelfLoop ++ selfLoopBusses.map((bus: CPSelfLoopBus) => (bus.processor.id, bus.id, bus.processor.id))


    reportProgress("creating busses")
    //creating the CPbusses
    val cpBusses: Array[CPBus] = (hardwareModel.busses.toList.map(
      bus => new CPRegularBus(bus.id, bus, this)
    ) ++ selfLoopBusses).toArray

    reportProgress("creating transmissions")
    //creating the CPtransmissions
    val cpTransmissions: Array[CPTransmission] = softwareModel.transmissions.map(
      flow => new CPTransmission(flow.id, flow,
        cpTasks(flow.source.id), cpTasks(flow.target.id),
        cpBusses,
        flow.size, flow.name, flow.timing,
        this,
        maxHorizon,
        processorToBusToProcessorAdjacency,
        selfLoopBussesID)
    )

    //creating the width var, in case needed for modulo scheduling
    val widthVar:Option[CPIntVar] =
      softwareModel.softwareClass match {
        case i:IterativeSoftware =>
          i.maxFrameDelay match {
            case Some(d) =>
              reportProgress("creating width var")
              Some(CPIntVar(0,d))
            case _ if goal.needsWidth =>
              reportProgress("creating width var")
              Some(CPIntVar(0,Int.MaxValue))
            case _ =>  None
          }
        case _ =>
          require(!goal.needsWidth,"you cannot ask for width related objective if not in an interative software")
          None
      }

    var widthVarList:List[CPIntVar] = List.empty

    reportProgress("registering tasks and transmissions to processors and busses")
    //registering tasks and transmissions to processors and busses
    for (bus <- cpBusses) {
      for (transmission <- cpTransmissions) {
        bus.accumulatePotentialTransmissionOnBus(transmission)
      }
      bus.close()

      widthVar match{
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
      widthVar match{
        case Some(w) =>
          widthVarList = proc.timeWidth :: widthVarList //TODO: add proc.temporaryStorageWidth
        case _ => ;
      }
      proc.close()
    }

    widthVar match{
      case Some(w) =>
        reportProgress("posting maxFrameDelay")
        addDocumented(maximum(widthVarList.toArray,w),"iterativeSoftware.maxFrameDelay")
      case _ => ;
    }

    reportProgress("computing makeSpanon tasks")
    val taskEnds = cpTasks.map(task => task.end)
    val makeSpan = maximum(taskEnds)

    val processorLoadArrayUnderApprox = Array.tabulate(cpProcessors.length)(_ => CPIntVar(0, maxHorizon))
    reportProgress("redundant bin-packing constraint on workload for mono task processors")

    //this one assumes adjusted minDuration per processor
    for (processorID <- cpProcessors.indices) {
      cpProcessors(processorID) match{
        case m:CPMonoTaskProcessor =>
          val areTaskunningOnThisProcessor = cpTasks.map(task => task.isRunningOnProcessor(processorID))
          val switchingDelay = m.switchingDelay
          val minDurationOfTaskWhenOnThisProcessor = cpTasks.map(task => task.minTaskDurationOnProcessor(processorID) + switchingDelay)
          val processorLoadVariable = processorLoadArrayUnderApprox(processorID)
          add(binaryKnapsack(areTaskunningOnThisProcessor, minDurationOfTaskWhenOnThisProcessor, processorLoadVariable))
          add(processorLoadVariable <== makeSpan + switchingDelay)
        case _ => ;
      }
    }

    //this one assumes minDuration for all task on any processor
    //it only works if all processors are CPMonoTaskProcessor
    //val processorLoadArrayUnderApprox2 = Array.tabulate(cpProcessors.length)(_ => CPIntVar(0, maxHorizon))
    //add(binPacking(cpTasks.map(_.processorID),cpTasks.map(_.taskDuration.min),processorLoadArrayUnderApprox2))
    //for(load <- processorLoadArrayUnderApprox2) add(load <== makeSpan)

    reportProgress("redundant bin-packing constraint on usage per bus")

    for(cpBus <- cpBusses){
      cpBus match{
        case r:CPRegularBus =>
          //TODO: not sure with the endNZ and end, which one should be used?
          add(r.busOccupancy <== makeSpan)
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
      case i:IterativeSoftware =>
        i.maxMakespan match{
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
          task => CumulativeTask(start = task.start, durationOpt = Some(task.taskDuration), end = task.end, amount = task.power, explanation = "power of task " + task)
        )
        CumulativeTask.postCumulativeForSimpleCumulativeTasks(simpleCumulativeTasks, CPIntVar(cap - backgroundPower),"powerCap")
    }

    //energyCap
    hardwareModel.energyCap match {
      case None => ;
      case Some(cap) =>
        reportProgress("posting energyCap")
        addDocumented(energy <= cap,"energyCap")
    }

    reportProgress("posting mapping constraints")

    for(constraint <- problem.constraints){
      constraint match {
        case RunOnConstraint(processor, process, value) =>
          if (value) {
            add(cpTasks(process.id).processorID === processor.id)
          } else {
            add(cpTasks(process.id).processorID !== processor.id)
          }
        case c@CoreSharingConstraint(processes, value) =>
          if (value) {
            //same cores all
            if(! processes.isEmpty){
              val processorID = cpTasks(processes.head.id).processorID
              for(process <- processes){
                add(cpTasks(process.id).processorID === processorID)
              }
            }
          } else {
            //different cores all
            val processesVars = processes.map(p => cpTasks(p.id).processorID)
            addDocumented(allDifferent(processesVars),c.toString)
          }
        case MustBeUsedConstraint(processor,value) =>
          if(value) {
            val isRunningOnProcessor: Array[CPBoolVar] = cpTasks.map(task => task.isRunningOnProcessor(processor.id))
            add(new cp.constraints.Or(isRunningOnProcessor))
          }else{
            for(task <- cpTasks){
              add(task.isRunningOnProcessor(processor.id) === 0)
            }
          }
        case SymmetricPEConstraint(processors:List[ProcessingElement],breaking) =>

          //TODO: check that PE are indeed symmetric
          breaking match {
            case SymmetricPEConstraintType.Workload =>
              println("breaking symmetry among " + processors.map(_.name)  + " by workload")
              val processorIDs = processors.map(_.id)
              val loadVariables = processorIDs.map(processorLoadArrayUnderApprox(_))

              def makeSorted(varList: List[CPIntVar]): Unit = {
                varList match {
                  case a :: b :: tail =>
                    add(a <== b)
                    makeSorted(b :: tail)
                  case _ => ;
                }
              }
              makeSorted(loadVariables)

            case SymmetricPEConstraintType.LongTask =>
              println("breaking symmetry among " + processors.map(_.name)  + " by lognest tasks assignment")

              val witnessProcessor = processors.head
              val witnessCPProcessor = cpProcessors(witnessProcessor.id)
              val witnessProcessorID = witnessProcessor.id
              val tasksPotentiallyRunningOnprocessors = cpTasks.toList.filter(task => !task.isRunningOnProcessor(witnessProcessor.id).isFalse)

              def breakSymmetry(taskPotentiallyunningOnProcessors: List[CPTask], processorIDs: List[Int]) {
                processorIDs match {
                  case Nil => ;
                  case currentProcessorID :: tail =>
                    val longestTask = taskPotentiallyunningOnProcessors.maxBy(_.minTaskDurationOnProcessor(witnessProcessorID))
                    val otherTasks =

                      for (p <- tail) {
                        add(longestTask.isRunningOnProcessor(p) === 0)
                      }
                    if (tail.nonEmpty) {
                      breakSymmetry(taskPotentiallyunningOnProcessors.filter(_.id != longestTask.id), tail)
                    }
                }
              }

              breakSymmetry(tasksPotentiallyRunningOnprocessors, processors.map(_.id))
          }
      }
    }

    reportProgress("starting search")
    CPMappingProblem(
      problem,
      hardwareModel.name,
      cpTasks,
      cpProcessors,
      cpBusses,
      cpTransmissions,
      makeSpan,
      energy,
      widthVar,
      processorLoadArrayUnderApprox)
  }

  def searchMappingProblem(problem: CPMappingProblem, goal: MappingGoal): Iterable[Mapping] = {

    def simpleVarFinder(a:SimpleMappingGoal):CPIntVar = {
      a match{
        case MinEnergy() => problem.energy
        case MinMakeSpan() => problem.makeSpan
        case MinFrame() => problem.widthVar match {
          case Some(w) => w
          case None => throw new Error("you want to minimize width, but this can only be done for iterative software")
        }
      }
    }

    val searchOnlyOne = goal match {
      case s:SimpleMappingGoal =>
        minimize(simpleVarFinder(s))
        false
      case Pareto(a,b) => solver.paretoMinimize(simpleVarFinder(a), simpleVarFinder(b))
        false
      case Sat() => true
    }

    solver.addDecisionVariables(problem.varsToSave)
    var secondLevels = 0
    var thirdLevels = 0

    search {
      //binaryFirstFail(problem.varsToDistribute)

      val allVars= (
        List.empty ++
          problem.cpTasks.flatMap(task => List(task.start,task.implementationID)) ++
          problem.cpTransmissions.flatMap(transmission => List(transmission.start, transmission.busID))
      ).toArray

      val taskSchedulingVars = List.empty ++
          problem.cpTasks.map(task => (task.start,task.taskDuration,task.end))

      val processorIDChoices = problem.cpTasks.map(task => task.processorID)
      val taskMaxDurations = problem.cpTasks.map(task => task.taskDuration.max)
      val taskMinDurations = problem.cpTasks.map(task => task.taskDuration.min)

      val taskStarts = (
        List.empty ++
          problem.cpTasks.map(task => task.start) ++
          problem.cpTransmissions.map(transmission => transmission.start)
        ).toArray

      (conflictOrderingSearch(
        processorIDChoices,
        taskMaxDurations(_),
        processorIDChoices(_).iterator.toList.maxBy(procID => problem.processorLoadArrayUnderApprox(procID).max))
        ++ oscar.algo.search.Branching({secondLevels += 1; Seq.empty}) //to know how many second levels (although I do not know how to interpret this yet)
      //  ++conflictOrderingSearch(taskStarts,minRegret(taskStarts),taskStarts(_).min)
        ++ binarySplit(taskStarts,varHeuris = (cpVar => cpVar.max - cpVar.min))
        ++ oscar.algo.search.Branching({thirdLevels += 1; println("second level");Seq.empty}) //to know how many second levels (although I do not know how to interpret this yet)
        ++ discrepancy(conflictOrderingSearch(allVars,minRegret(allVars),allVars(_).min),maxDiscrepancy))

      /*val allVars = problem.varsToDistribute.toArray
      conflictOrderingSearch(allVars,allVars(_).min,allVars(_).min)
*/
      /*
         val taskPriorities = problem.generateTaskPriorityValues.toArray.asInstanceOf[Array[CPIntVar]]
         val taskAlternatives
         (conflictOrderingSearch(taskPriorities,taskPriorities(_).min,taskPriorities(_).min)
           ++ discrepancy(conflictOrderingSearch(allVars,allVars(_).min,allVars(_).min),maxDiscrepancy))
   */

      //setTimes(startsVar, durationsVar, endsVar)
      //discrepancy(binaryFirstFail(problem.varsToDistribute),3)

    } onSolution {
      println("solution found, makeSpan=" + problem.makeSpan.value + " energy:" + problem.energy.value)

    }

    val stat = start(nSols = if(searchOnlyOne)1 else Int.MaxValue,timeLimit = timeLimit) //,maxDiscrepancy = maxDiscrepancy)

    print(stat)
    println("secondLevels:" + secondLevels)
    println("thirdLevels:" + thirdLevels)
    println

    goal match {
      case Pareto(a,b) =>
        solver.nonDominatedSolutions.sortBy(_.apply( simpleVarFinder(a))).map(cpSol => problem.getMapping(cpSol))
      case _:SimpleMappingGoal | _:Sat =>
        val lastSol = solver.lastSol
        if (lastSol.dict.isEmpty) {
          None
        } else {
          Some(problem.getMapping(lastSol))
        }
    }
  }
}
