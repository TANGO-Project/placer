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

import oscar.cp._
import oscar.cp.core.variables.CPIntVar
import oscar.cp.modeling.Constraints
import placerT.algo.hw._
import placerT.algo.sw.{CPTask, CPTransmission}
import placerT.metadata.hw._
import placerT.metadata.sw._
import placerT.metadata.{MappingProblem, _}

import scala.collection.immutable.SortedSet
import scala.collection.mutable.ArrayBuffer

case class MapperConfig(maxDiscrepancy:Int=Int.MaxValue,
                        timeLimit:Int = Int.MaxValue,
                        lns:Boolean = true,
                        lnsMaxFails:Int = 2000,
                        lnsRelaxProba:Int = 90,
                        lnsNbRelaxations:Int = 500,
                        lnsNbRelaxationNoImprove:Int = 200)


object Mapper {

  def findMapping(problem: MappingProblem,config:MapperConfig): Mappings = {
    // try {
    Mappings(new Mapper(problem,config).mapping)
    // } catch{case e:oscar.cp.core.NoSolutionException => Mappings(List.empty)}
  }
}

class Mapper(val problem: MappingProblem,config:MapperConfig) extends CPModel with Constraints {

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

  store.silent = true

  val cpProblem = postProblem(softwareModel, hardwareModel)
  val mapping = solveMappingProblem(cpProblem, goal)

  def reportProgress(startedUpTask:String): Unit ={
    this.solver.propagate()
    if(this.solver.isFailed) throw new Error("solver trivially concluded to no solution or overflowed during latest modeling step")
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

    //    println("processorToBusToProcessorAdjacency" + processorToBusToProcessorAdjacency)

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

    reportProgress("computing makeSpan on tasks")
    val taskEnds = cpTasks.map(task => task.end)
    val makeSpan = maximum(taskEnds)

    val processorLoadArrayUnderApprox = Array.tabulate(cpProcessors.length)(_ => CPIntVar(0, maxHorizon))

    //reportProgress("redundant bin-packing constraint on workload for mono task processors")
    //this one assumes adjusted minDuration per processor
    for (processorID <- cpProcessors.indices) {
      cpProcessors(processorID) match{
        case m:CPMonoTaskProcessor =>
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

    reportProgress("redundant bin-packing constraint on usage per bus")

    for(cpBus <- cpBusses){
      cpBus match{
        case r:CPRegularBus =>
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
          task => CumulativeTask(start = task.start, duration = task.taskDuration, end = task.end, amount = task.power, explanation = "power of task " + task)
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
          val processesIDSet = SortedSet.empty[Int] ++ processes.map(_.id)
          if (value) {
            //same cores all
            if(! processes.isEmpty){
              val processorID = cpTasks(processes.head.id).processorID
              for(process <- processes){
                add(cpTasks(process.id).processorID === processorID)
              }

              //this improves propagation
              for(transmission <- cpTransmissions) {
                if (processesIDSet.contains(transmission.from.id) && processesIDSet.contains(transmission.to.id)) {
                  transmission.isSelfLoopTransmission.assignTrue()
                }
              }
            }else{
              println("requiring same core for an empty set of tasks?")
            }
          } else {
            //different cores all
            val processesVars = processes.map(p => cpTasks(p.id).processorID)
            addDocumented(allDifferent(processesVars),c.toString)

            //this improves propagation
            for(transmission <- cpTransmissions) {
              if (processesIDSet.contains(transmission.from.id) && processesIDSet.contains(transmission.to.id)) {
                transmission.isSelfLoopTransmission.assignFalse()
              }
            }
          }
        case x@MustBeUsedConstraint(processor,value) =>

          if(value) {
            println("posting MustBeUsedConstraint(" + processor.name + ")")
            val isRunningOnProcessor: Array[CPBoolVar] = cpTasks.map(task => task.isRunningOnProcessor(processor.id))
            add(new oscar.cp.constraints.Or(isRunningOnProcessor))
          }else{
            println("posting MustNotBeUsedConstraint(" + processor.name + ")")
            for(task <- cpTasks){
              add(task.isRunningOnProcessor(processor.id) === 0)
            }
          }
        case SymmetricPEConstraint(processors:List[ProcessingElement],breaking) =>
          if(config.lns){
            println("symmetry among " + processors.map(_.name) + " is disabled because using LNS")
          }else {
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
                val witnessProcessorID =  processors.head.id
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
      cpTasks,
      cpProcessors,
      cpBusses,
      cpTransmissions,
      makeSpan,
      energy,
      widthVar,
      processorLoadArrayUnderApprox)
  }

  def solveMappingProblem(problem: CPMappingProblem, goal: MappingGoal): Iterable[Mapping] = {

    if(config.lns) {
      goal match {
        case s: SimpleMappingGoal =>
          return solveMappingProblemMinimizeLNS(problem: CPMappingProblem, s)
        case _ =>
          throw new Error("LNS can only be used for simple mapping goals, not for " + goal)
      }
    }

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

    val (isSearchOnlyOne,isParetoSearch,theVar) = goal match {
      case s:SimpleMappingGoal =>
        val theVar = simpleVarFinder(s)
        minimize(theVar)
        (false,false,theVar)
      case Pareto(a,b) => solver.paretoMinimize(simpleVarFinder(a), simpleVarFinder(b))
        (false,true,null)
      case Sat() => (true,false,null)
    }

    solver.addDecisionVariables(problem.varsToSave)

    var secondLevels = 0
    var thirdLevels = 0

    search {
      val allVars = problem.varsToDistribute.toArray
      if(isParetoSearch) {
        binaryFirstFail(allVars)
        //conflict search does not deliver pareto optimal results so it is not used here.
        //conflictOrderingSearch(allVars,allVars(_).min,allVars(_).min)
        //Same for split
        //splitLastConflict(allVars)
      }else{
        //binaryFirstFail(allVars)
        //splitLastConflict(allVars)
        val processorIDArray = problem.cpTasks.map(_.processorID)
        conflictOrderingSearch(processorIDArray,processorIDArray(_).min,processorIDArray(_).min) ++ conflictOrderingSearch(allVars,allVars(_).min,allVars(_).min)

        val processorIDChoices = problem.cpTasks.map(task => task.processorID)
        val taskMaxDurations = problem.cpTasks.map(task => task.taskDuration.max)

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
          ++ oscar.algo.search.Branching({thirdLevels += 1; /*println("second level");*/ Seq.empty}) //to know how many second levels (although I do not know how to interpret this yet)
          ++ discrepancy(conflictOrderingSearch(allVars,minRegret(allVars),allVars(_).min),config.maxDiscrepancy))
      }
    } onSolution {
      println("solution found, makeSpan=" + problem.makeSpan.value + " energy:" + problem.energy.value)
    }

    val stat = start(nSols = if(isSearchOnlyOne)1 else Int.MaxValue,timeLimit = config.timeLimit, maxDiscrepancy = config.maxDiscrepancy)
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



  def solveMappingProblemMinimizeLNS(problem: CPMappingProblem, simpleGoal: SimpleMappingGoal): Iterable[Mapping] = {

    def simpleVarFinder(a: SimpleMappingGoal): CPIntVar = {
      a match {
        case MinEnergy() => problem.energy
        case MinMakeSpan() => problem.makeSpan
        case MinFrame() => problem.widthVar match {
          case Some(w) => w
          case None => throw new Error("you want to minimize width, but this can only be done for iterative software")
        }
      }
    }

    val varToMinimize = simpleVarFinder(simpleGoal)

    minimize(varToMinimize)

    solver.addDecisionVariables(problem.varsToSave)

    var secondLevels = 0
    var thirdLevels = 0

    var bestSolution: Option[Mapping] = None
    var bestValue: Int = Int.MaxValue

    search {
      val allVars = problem.varsToDistribute.toArray

      //binaryFirstFail(allVars)
      //splitLastConflict(allVars)
      val processorIDArray = problem.cpTasks.map(_.processorID)
     // conflictOrderingSearch(processorIDArray, processorIDArray(_).min, processorIDArray(_).min) ++ conflictOrderingSearch(allVars, allVars(_).min, allVars(_).min)

      val processorIDChoices = problem.cpTasks.map(task => task.processorID)
      val taskMaxDurations = problem.cpTasks.map(task => task.taskDuration.max)

      val taskStarts = (
        List.empty ++
          problem.cpTasks.map(task => task.start) ++
          problem.cpTransmissions.map(transmission => transmission.start)
        ).toArray

      (conflictOrderingSearch(
        processorIDChoices,
        taskMaxDurations(_),
        processorIDChoices(_).iterator.toList.maxBy(procID => problem.processorLoadArrayUnderApprox(procID).max))
        ++ oscar.algo.search.Branching({
        secondLevels += 1; Seq.empty
      }) //to know how many second levels (although I do not know how to interpret this yet)
        //  ++conflictOrderingSearch(taskStarts,minRegret(taskStarts),taskStarts(_).min)
        ++ binarySplit(taskStarts, varHeuris = (cpVar => cpVar.max - cpVar.min))
        ++ oscar.algo.search.Branching({
        thirdLevels += 1; /*println("second level");*/ Seq.empty
      }) //to know how many second levels (although I do not know how to interpret this yet)
        ++ conflictOrderingSearch(allVars, minRegret(allVars), allVars(_).min))


    } onSolution {
      bestSolution = Some(problem.getMapping(solver.lastSol))
      bestValue = varToMinimize.value
      println("solution found, makeSpan=" + problem.makeSpan.value + " energy:" + problem.energy.value)
    }

    val stat = start(nSols = 1, timeLimit = Int.MaxValue, maxDiscrepancy = Int.MaxValue)

      println("stat of initial solution: ")
      println(stat)

    bestSolution match{
      case None => throw new Error("Placer could not find an initial placement to start LNS, problem seems to have no solution")
      case _ => ;
    }
    val samePEConstraints:Iterable[CoreSharingConstraint] = problem.mappingProblem.constraints.flatMap(
      _ match{
        case c@CoreSharingConstraint(processes, value) if value => Some(c)
        case _ => None
      }
    )

    val allProcessesInSamePEConstraints:SortedSet[Int] = SortedSet.empty[Int] ++ samePEConstraints.flatMap(_.processes.map(_.id))

    //LNS restart stuff here!
    val constraintBuffer = ArrayBuffer[Constraint]()
    val maxFails = 2000
    val relaxProba = 90
    val nRelaxations = 500
    val nbRelaxationNoImprove = 200
    var remainigRelaxationNoImprove = nbRelaxationNoImprove
    var currentRelaxation = 0
    while(currentRelaxation < nRelaxations && remainigRelaxationNoImprove > 0){
      remainigRelaxationNoImprove -= 1
      currentRelaxation = currentRelaxation +1
      println("relaxation " + currentRelaxation)
      constraintBuffer.clear()
      val stats = startSubjectTo(failureLimit = maxFails,timeLimit = config.timeLimit) {
        for ((task, pe, implem, s, d, e) <- bestSolution.get.taskMapping) {
          if(!allProcessesInSamePEConstraints.contains(task.id)) {
            if (scala.math.random * 100 > relaxProba) {
              constraintBuffer += (problem.cpTasks(task.id).processorID === pe.id)
            }
          }
        }

        for(samePEConstraint <- samePEConstraints){
          if (scala.math.random * 100 > relaxProba) {
            val witnessTaskID = samePEConstraint.processes.head.id
            val processorID = problem.cpTasks(witnessTaskID).processorID
            for(task <- samePEConstraint.processes) {
              constraintBuffer += (problem.cpTasks(task.id).processorID === processorID)
            }
          }
        }
        add(constraintBuffer)
      }

      if(stats.nSols > 0){
        remainigRelaxationNoImprove = nbRelaxationNoImprove
      }
    }

    bestSolution
  }
}
