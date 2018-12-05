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

package placer.algo

import oscar.cp.constraints.ParetoConstraint
import oscar.cp.core.variables.CPIntVar
import oscar.cp.{multiobjective, _}
import placer.metadata._

class PureCPSolver(cpProblem: CPMappingProblem,
                   goal: Option[MappingObjective],
                   config:MapperConfig,
                   solver:CPSolver,
                   bestSolutionsSoFar:multiobjective.ListPareto[Mapping])
  extends SearchStrategy(cpProblem: CPMappingProblem,
    config:MapperConfig,
    solver:CPSolver){

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
      searchStrategy(isParetoSearch)
    } onSolution {
      if(config.verbose) println("solution found, makespan=" + cpProblem.makeSpan.value + " energy:" + cpProblem.energy.value)
    }

    val stat = start(nSols = if (isSearchOnlyOne) 1 else Int.MaxValue, timeLimit = config.timeLimit)
    if (config.verbose) println(stat)

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
