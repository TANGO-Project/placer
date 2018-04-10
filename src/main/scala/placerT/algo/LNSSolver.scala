package placerT.algo

import oscar.cp
import oscar.cp.constraints.ParetoConstraint
import oscar.cp.core.CPSol
import oscar.cp.core.variables.CPIntVar
import oscar.cp.modeling.Constraints
import oscar.cp.{multiobjective, _}
import placerT.algo.sw.{CPTask, CPTransmission}
import placerT.metadata._

import scala.collection.immutable.SortedSet


class LNSSolver(cpProblem: CPMappingProblem, simpleGoal: SimpleMappingGoal, config:MapperConfig, solver:oscar.cp.CPSolver, bestSolutionsSoFar:multiobjective.ListPareto[Mapping]){

  implicit val solver2:oscar.cp.core.CPSolver = solver

  def solveMappingProblemMinimizeLNS(): Iterable[Mapping] = {

    def simpleVarFinder(a: SimpleMappingGoal): CPIntVar = {
      a match {
        case Sat() =>  throw new Error("LNS cannot solve sat problems")
        case MinEnergy() => cpProblem.energy
        case MinMakeSpan() => cpProblem.makeSpan
        case MinFrame() => cpProblem.widthVar match {
          case Some(w) => w
          case None => throw new Error("you want to minimize width, but this can only be done for iterative software")
        }
      }
    }

    val varToMinimize = simpleVarFinder(simpleGoal)
    val theVar = varToMinimize

    minimize(varToMinimize)(solver)

    solver.addDecisionVariables(cpProblem.varsToSave)
    solver.addDecisionVariables(theVar)

    var bestSolution: Option[Mapping] = None
    var bestValue: Int = Int.MaxValue

    search {
      val allVars = cpProblem.varsToDistribute.toArray

      //binaryFirstFail(allVars)
      //splitLastConflict(allVars)
      val processorIDArray = cpProblem.cpTasks.map(_.processorID)
      // conflictOrderingSearch(processorIDArray, processorIDArray(_).min, processorIDArray(_).min) ++ conflictOrderingSearch(allVars, allVars(_).min, allVars(_).min)

      val processorIDChoices = cpProblem.cpTasks.map(task => task.processorID)
      val taskMaxDurations = cpProblem.cpTasks.map(task => task.taskDuration.max)

      val taskStarts = (
        List.empty ++
          cpProblem.cpTasks.map(task => task.start) ++
          cpProblem.cpTransmissions.map(transmission => transmission.start)
        ).toArray

      val arrayOfNbInstancesOfSharedFunctions = cpProblem.cpSharedFunctions.map(_.nbInstances).toArray

      (conflictOrderingSearch(
        processorIDChoices,
        taskMaxDurations(_),
        processorIDChoices(_).iterator.toList.maxBy(procID => cpProblem.processorLoadArrayUnderApprox(procID).max)) //TODO: should consider the fastest implementation first!!
        ++ (if(arrayOfNbInstancesOfSharedFunctions.nonEmpty) conflictOrderingSearch(
        arrayOfNbInstancesOfSharedFunctions,
        (fnID) => arrayOfNbInstancesOfSharedFunctions(fnID).max,
        (fnID) => arrayOfNbInstancesOfSharedFunctions(fnID).min
      ) else oscar.algo.search.Branching({Seq.empty}))
        ++ binarySplit(taskStarts, varHeuris = cpVar => cpVar.max - cpVar.min)
        ++ conflictOrderingSearch(allVars, minRegret(allVars), allVars(_).min)
        )

    } onSolution {
      bestSolution = Some(cpProblem.getMapping(solver.lastSol,List(theVar)))
      bestValue = varToMinimize.value
      println("solution found, makeSpan=" + cpProblem.makeSpan.value + " energy:" + cpProblem.energy.value)
    }



    config.lnsCarryOnObjForMultiHardware match {
      case 0 =>
        val stat = start(nSols = 1, timeLimit = Int.MaxValue, maxDiscrepancy = Int.MaxValue)
        println("stat of initial solution (no carry on lns): ")
        println(stat)
      case 1 =>
        println("first attempt of initial solution with lns carry on")
        val firstAttempt = startSubjectTo(nSols = 1, timeLimit = Int.MaxValue, maxDiscrepancy = Int.MaxValue) {
          if (!bestSolutionsSoFar.isEmpty) {
            require(bestSolutionsSoFar.size == 1)
            add(ParetoConstraint(bestSolutionsSoFar, Array(false), Array(theVar)))
          }
        }

        if (firstAttempt.nSols == 0) {
          println("initial solution with lns carry on failed: ")
          println(firstAttempt)

          if(bestSolutionsSoFar.size == 1) {
            println("starting second attempt without carry on")
            val stat = start(nSols = 1, timeLimit = Int.MaxValue, maxDiscrepancy = Int.MaxValue)
            println("stat of second attempt without lns carry on: ")
            println(stat)
          }else{
            println("there is no actual value carried on, so no second attempt")
          }

        } else {
          println("initial solution with lns carry on:")
          println(firstAttempt)
        }

      case 2 =>
        val stat = startSubjectTo(nSols = 1, timeLimit = Int.MaxValue, maxDiscrepancy = Int.MaxValue) {
          if (!bestSolutionsSoFar.isEmpty) {
            require(bestSolutionsSoFar.size == 1)
            add(ParetoConstraint(bestSolutionsSoFar, Array(false), Array(theVar)))
          }
        }
      case x => "undefined value of lnsCarryOn:" + x
    }



    bestSolution match{
      case None => throw new Error("Placer could not find an initial placement to start LNS, problem seems to have no solution")
      case _ => ;
    }
    val samePEConstraints:Iterable[CoreSharingConstraint] = cpProblem.mappingProblem.constraints.cl.flatMap(
      _ match{
        case c@CoreSharingConstraint(processes, value) if value => Some(c)
        case _ => None
      }
    )

    val allProcessesInSamePEConstraints:SortedSet[Int] = SortedSet.empty[Int] ++ samePEConstraints.flatMap(_.processes.map(_.id))

    //LNS restart stuff here!

    val maxFails = config.lnsMaxFails
    val relaxProba = config.lnsRelaxProba
    val nRelaxations = config.lnsNbRelaxations
    var remainigRelaxationNoImprove = config.lnsNbRelaxationNoImprove
    var currentRelaxation = 0
    while(currentRelaxation < nRelaxations && remainigRelaxationNoImprove > 0){
      remainigRelaxationNoImprove -= 1
      currentRelaxation = currentRelaxation +1
      println("relaxation " + currentRelaxation)

      val constraintsForThisRelaxation = generateConstraintsForRelaxation(relaxProba,bestSolution.get,allProcessesInSamePEConstraints,samePEConstraints)
      val stats1 = startSubjectTo(failureLimit = maxFails/100,timeLimit = config.timeLimit) {
        //relaxation strategy (actually it is more a non-relaxation strategy)
        add(constraintsForThisRelaxation)
      }

      //we only did a small run, so if there was some soluition, carry on the search, with the same relaxation
      if(stats1.nSols > 0) {
        remainigRelaxationNoImprove = config.lnsNbRelaxationNoImprove
        val stats2 = startSubjectTo(failureLimit = maxFails, timeLimit = config.timeLimit*10) {
          add(constraintsForThisRelaxation)
        }
      }else{
        println("early stop")
      }
    }

    bestSolution
  }


  def timeOverlappingTasks(task:CPTask,mapping:Mapping):List[CPTask] = {
    cpProblem.cpTasks.toList.filter((otherTask:CPTask) =>
      taskOverlap(task,otherTask,mapping.cpSol)
    )
  }

  def taskOverlap(taskA:CPTask,taskB:CPTask,sol:CPSol):Boolean = {
    if(sol(taskA.end) < sol(taskB.start)) false
    else if (sol(taskB.end) < sol(taskA.start)) false
    else true
  }
  def transmissionOverlap(transmissionA:CPTransmission,transmissionB:CPTransmission,sol:CPSol):Boolean = {
    if(sol(transmissionA.end) < sol(transmissionB.start)) false
    else if (sol(transmissionB.end) < sol(transmissionA.start)) false
    else true
  }


  def timeOverlappingTransmissions(transmission:CPTransmission, mapping:Mapping):List[CPTransmission] = {
    cpProblem.cpTransmissions.toList.filter(
      (otherTransmission:CPTransmission) =>
        transmission != otherTransmission
          && transmissionOverlap(transmission,otherTransmission,mapping.cpSol)
    )
  }

  def transmissionToAdjacentTasks(transmission:CPTransmission):List[CPTask] = {
    List(transmission.from,transmission.to)
  }

  def taskToAdjacentTransmissions(task:CPTask):List[CPTransmission] = {
    List.empty ++ task.incomingCPTransmissions ++ task.outgoingCPTransmissions
  }

  def nextAndPrevTransmissionsOnSameSupport(transmission:CPTransmission, mapping:Mapping):List[CPTransmission] = {
    val sol = mapping.cpSol
    val onSameBus = cpProblem.cpTransmissions.filter(p => sol(p.busID) == sol(transmission.busID) && p != transmission)
    val precedings = onSameBus.filter(p => sol(p.start) < sol(transmission.start))
    val lastPreceding = if(precedings.nonEmpty){
      Some(precedings.maxBy(p => sol(p.start)))
    }else{
      None
    }

    val succeedings = onSameBus.filter(p => sol(p.start) > sol(transmission.start))
    val firstSucceeding = if(succeedings.nonEmpty) {
      Some(precedings.minBy(p => sol(p.start)))
    }else{
      None
    }
    List.empty ++ lastPreceding ++ firstSucceeding
  }


  def expandTask(task:CPTask,mapping:Mapping,requiredSize:Int):List[CPTask] = {
    var taskSet:SortedSet[Int] = SortedSet(task.id)
    while(taskSet.size < requiredSize){
      val overlappingTasks:SortedSet[Int] = taskSet.flatMap(taskID => timeOverlappingTasks(cpProblem.cpTasks(taskID),mapping).map(_.id))
      val newOverlappingTasks = overlappingTasks -- taskSet
      val linkedTasks:SortedSet[Int] = newOverlappingTasks.flatMap(taskID => {
        val adjacentTransmissions:List[CPTransmission] = taskToAdjacentTransmissions(cpProblem.cpTasks(taskID))
        val linkedTasks = adjacentTransmissions.flatMap(transmissionToAdjacentTasks)
        val timeOverlappingTrans:List[CPTransmission] = adjacentTransmissions.flatMap(t => timeOverlappingTransmissions(t,mapping))
        val tasksLinkedToTimeOverlappingTransmissions = timeOverlappingTrans.flatMap(transmissionToAdjacentTasks)
        SortedSet.empty[Int] ++ linkedTasks.map(_.id) ++ tasksLinkedToTimeOverlappingTransmissions.map(_.id)
      })
      taskSet = taskSet ++ linkedTasks
    }
    taskSet.toList.map(cpProblem.cpTasks(_))
  }

  def generateConstraintsForRelaxation(relaxProba:Int,
                                       bestSolution:Mapping,
                                       allProcessesInSamePEConstraints:SortedSet[Int],
                                       samePEConstraints:Iterable[CoreSharingConstraint]): List[Constraint] ={

    var toReturn:List[Constraint] = List.empty
    val mustRelaxArray = Array.fill(cpProblem.cpTasks.length)(false)

    //for shared function stuff
    for(listOFTaskOnSameFlexible:List[CPTask] <- cpProblem.pEToTasksOnFlexible){
      if (scala.math.random * 100 > relaxProba) {
        //must relax
        for(taskToRelaxAnyway <- listOFTaskOnSameFlexible){
          mustRelaxArray(taskToRelaxAnyway.id) = true
        }
      }
    }

    //does not operate on SamePE constraints
    for (TaskMapping(task, pe, implem, s, d, e) <- bestSolution.taskMapping) {
      if(!allProcessesInSamePEConstraints.contains(task.id) && !mustRelaxArray(task.id)) {
        if (scala.math.random * 100 > relaxProba) {
          toReturn =(cpProblem.cpTasks(task.id).processorID === pe.id) :: toReturn
        }
      }
    }

    for(samePEConstraint <- samePEConstraints){
      val noMustRelax = samePEConstraint.processes.forall(process => !mustRelaxArray(process.id))
      if (noMustRelax && scala.math.random * 100 > relaxProba) {
        val witnessTaskID = samePEConstraint.processes.head.id
        val processorID = cpProblem.cpTasks(witnessTaskID).processorID
        for(task <- samePEConstraint.processes) {
          toReturn = (cpProblem.cpTasks(task.id).processorID === processorID) :: toReturn
        }
      }
    }
    toReturn
  }
}


