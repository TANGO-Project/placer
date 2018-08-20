package placer.algo

import oscar.cp.constraints.ParetoConstraint
import oscar.cp.core.variables.CPIntVar
import oscar.cp.{multiobjective, _}
import placer.metadata._

class PureCPSolver(cpProblem: CPMappingProblem, goal: Option[MappingObjective], config:MapperConfig, solver:CPSolver, bestSolutionsSoFar:multiobjective.ListPareto[Mapping]) {

  implicit val solver2:oscar.cp.core.CPSolver = solver

  def solveMappingProblem(): Iterable[Mapping] = {

    def simpleVarFinder(a: SimpleMappingGoal): CPIntVar = {
      a match {
        case MinEnergy() => cpProblem.energy
        case MinMakeSpan() => cpProblem.makeSpan
        case MinFrame() => cpProblem.widthVar match {
          case Some(w) => w
          case None => throw new Error("you want to minimize width, but this can only be done for iterative software")
        }
      }
    }

    val (isSearchOnlyOne, isParetoSearch, theVars) = goal match {
      case None => (true, false, List.empty)

      case Some(s: SimpleMappingGoal) =>
        val theVar = simpleVarFinder(s)
        minimize(theVar)

        if (!bestSolutionsSoFar.isEmpty) {
          require(bestSolutionsSoFar.size == 1)
          try {
            add(ParetoConstraint(bestSolutionsSoFar, Array(false), Array(theVar)))
          } catch{
            case _:NoSolutionException => return None
          }
        }

        (false, false, List(theVar))
      case Some(MinPareto(a, b)) =>
        val varA = simpleVarFinder(a)
        val varB = simpleVarFinder(b)
        solver.paretoMinimize(varA, varB)

        if (!bestSolutionsSoFar.isEmpty) {
          add(ParetoConstraint(bestSolutionsSoFar, Array(false, false), Array(varA, varB)))
        }

        (false, true, List(varA, varB))
    }

    solver.addDecisionVariables(cpProblem.varsToSave)
    solver.addDecisionVariables(theVars)

    search {
      val allVars = cpProblem.varsToDistribute.toArray
      if (isParetoSearch) {
        binaryFirstFail(allVars)
        //conflict search does not deliver pareto optimal results so it is not used here.
        //conflictOrderingSearch(allVars,allVars(_).min,allVars(_).min)
        //Same for split
        //splitLastConflict(allVars)
      } else {
        //binaryFirstFail(allVars)
        //splitLastConflict(allVars)
        val processorIDArray = cpProblem.cpTasks.map(_.processorID)
        //        conflictOrderingSearch(processorIDArray,processorIDArray(_).min,processorIDArray(_).min) ++ conflictOrderingSearch(allVars,allVars(_).min,allVars(_).min)

        val processorIDChoices = cpProblem.cpTasks.map(task => task.processorID)
        val taskMaxDurations = cpProblem.cpTasks.map(task => task.taskDuration.max)

        val taskAndTransmissionStarts = (
          List.empty ++
            cpProblem.cpTasks.map(task => task.start) ++
            cpProblem.cpTransmissions.map(transmission => transmission.start)
          ).toArray

        val arrayOfNbInstancesOfSharedFunctions = cpProblem.cpSharedFunctions.map(_.nbInstances).toArray


        //basic distribution procedures
        def distributeOnTaskPlacementLessBuzyProcFirst = conflictOrderingSearch(
          processorIDChoices,
          taskMaxDurations(_),
          processorIDChoices(_).iterator.toList.maxBy(procID => cpProblem.processorLoadArrayUnderApprox(procID).max))

        def distributeOnTaskPlacementFastestImplemPlusLessBuzyProcFirst = conflictOrderingSearch(
          processorIDChoices,
          taskID => cpProblem.cpTasks(taskID).taskDuration.size,
          taskID => cpProblem.cpTasks(taskID).processorID.minBy(
            processorID => cpProblem.cpTasks(taskID).minTaskDurationOnProcessor(processorID).getOrElse(Int.MaxValue) + cpProblem.processorLoadArrayUnderApprox(processorID).max))

        def distributeOnSharedImplementationInstances =
          if(arrayOfNbInstancesOfSharedFunctions.nonEmpty)
            conflictOrderingSearch(
              arrayOfNbInstancesOfSharedFunctions,
              fnID => arrayOfNbInstancesOfSharedFunctions(fnID).max,
              fnID => arrayOfNbInstancesOfSharedFunctions(fnID).min)
          else oscar.algo.search.Branching({Seq.empty})

        def distributeOnLocalOrBusTransmission =
          conflictOrderingSearch(
            cpProblem.cpTransmissions.map(_.isSelfLoopTransmission),
            transmissionID => cpProblem.cpTransmissions(transmissionID).isSelfLoopTransmission.size,
            transmissionID => cpProblem.cpTransmissions(transmissionID).isSelfLoopTransmission.min)

        def distributeOnTransmissionRouting =
          conflictOrderingSearch(
            cpProblem.cpTransmissions.map(_.busID),
            transmissionID => -cpProblem.cpTransmissions(transmissionID).transmissionDuration.size, //the one that has the most impact on the schedule?
            transmissionID => cpProblem.cpTransmissions(transmissionID).busID.min)

        def distributeOnScheduleConflict = conflictOrderingSearch(
          taskAndTransmissionStarts,
          taskAndTransmissionID => taskAndTransmissionStarts(taskAndTransmissionID).size,
          taskAndTransmissionID => taskAndTransmissionStarts(taskAndTransmissionID).min)

        (/*distributeOnLocalOrBusTransmission
        ++ distributeOnTransmissionRouting
        ++ distributeOnTaskPlacementLessBuzyProcFirst //TODO: should consider the fastest implementation first!!
        ++ */
          /*distributeOnLocalOrBusTransmission
            ++*/ distributeOnTaskPlacementFastestImplemPlusLessBuzyProcFirst
          ++ distributeOnSharedImplementationInstances
          ++ binarySplit(taskAndTransmissionStarts, varHeuris = cpVar => cpVar.max - cpVar.min)
          ++ conflictOrderingSearch(allVars, minRegret(allVars), allVars(_).min)
          )


        /*
        (conflictOrderingSearch(
          processorIDChoices,
          taskMaxDurations(_),
          processorIDChoices(_).iterator.toList.maxBy(procID => cpProblem.processorLoadArrayUnderApprox(procID).max))
          ++ (if (arrayOfNbInstancesOfSharedFunctions.nonEmpty) conflictOrderingSearch(
          arrayOfNbInstancesOfSharedFunctions,
          (fnID) => arrayOfNbInstancesOfSharedFunctions(fnID).max,
          (fnID) => arrayOfNbInstancesOfSharedFunctions(fnID).min
        ) else oscar.algo.search.Branching({
          Seq.empty
        }))
          ++ binarySplit(taskStarts, varHeuris = (cpVar => cpVar.max - cpVar.min))
          ++ discrepancy(conflictOrderingSearch(allVars, minRegret(allVars), allVars(_).min), config.maxDiscrepancy))*/
      }
    } onSolution {
      println("solution found, makeSpan=" + cpProblem.makeSpan.value + " energy:" + cpProblem.energy.value)
    }

    val stat = start(nSols = if (isSearchOnlyOne) 1 else Int.MaxValue, timeLimit = config.timeLimit)
    print(stat)

    println

    goal match {
      case Some(MinPareto(a, b)) =>
        solver.nonDominatedSolutions.sortBy(_.apply(simpleVarFinder(a))).map(cpSol => cpProblem.getMapping(cpSol, theVars))
      case Some(_: SimpleMappingGoal) | None =>
        val lastSol = solver.lastSol
        if (lastSol.dict.isEmpty) {
          None
        } else {
          Some(cpProblem.getMapping(lastSol, theVars))
        }
    }
  }
}
