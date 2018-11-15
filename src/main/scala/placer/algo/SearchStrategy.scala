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

import oscar.algo.search.Branching
import oscar.cp.scheduling.search.SetTimesBranching
import oscar.cp.{CPIntVar, CPSolver, binaryFirstFail, binarySplit, conflictOrderingSearch, minRegret}

object Strategy extends Enumeration {
  val TransmissionRouting,
  TaskPlacementLessBuzyProcFirst,
  LocalOrBusTransmissionLargestFirstLocalFirst,
  LocalOrBusTransmissionLongestAdjFirstNonLocalFirst,
  TaskPlacementFastestImplemPlusLessBuzyProcFirst,
  SharedImplementationInstances,
  TaskAndTransmissionStarts,
  HEFT = Value
}

import Strategy._

class SearchStrategy(cpProblem: CPMappingProblem,
                     config:MapperConfig,
                     solver:CPSolver) {

  implicit val solver2: oscar.cp.core.CPSolver = solver

  def searchStrategy(isParetoSearch: Boolean):Branching = {
    val allVars = cpProblem.varsToDistribute.toArray
    if (isParetoSearch) {
      binaryFirstFail(allVars)
      //conflict search does not deliver pareto optimal results so it is not used here.
      //conflictOrderingSearch(allVars,allVars(_).min,allVars(_).min)
      //Same for split
      //splitLastConflict(allVars)
    } else {

      config.strategy match {
        case Some(l) =>
          instantiateStrategyFromGivenList(l: List[Strategy.Value]) ++ defaultSearchStrategy
        case None =>
          defaultSearchStrategy

      }
    }
  }

  def instantiateStrategyFromGivenList(a: List[Strategy.Value]): Branching = {
    val allVars = cpProblem.varsToDistribute.toArray
    val processorIDArray = cpProblem.cpTasks.map(_.processorID)
    val processorIDChoices = cpProblem.cpTasks.map(task => task.processorID)
    val taskMaxDurations = cpProblem.cpTasks.map(task => task.taskDuration.max)
    val taskAndTransmissionStarts = (
      List.empty ++
        cpProblem.cpTasks.map(task => task.start) ++
        cpProblem.cpTransmissions.map(transmission => transmission.start)
      ).toArray
    val taskAndTransmissionDuration = (
      List.empty ++
        cpProblem.cpTasks.map(task => task.taskDuration) ++
        cpProblem.cpTransmissions.map(transmission => transmission.transmissionDuration)
      ).toArray
    val taskAndTransmissionEnds = (
      List.empty ++
        cpProblem.cpTasks.map(task => task.end) ++
        cpProblem.cpTransmissions.map(transmission => transmission.end)
      ).toArray

    val arrayOfNbInstancesOfSharedFunctions = cpProblem.cpSharedFunctions.map(_.nbInstances).toArray

    def instantiateStrategyFromGivenListRecur(a: List[Strategy.Value]): Branching = {
      a match {
        case head :: tail => instantiateSimpleStrategy(head) ++ instantiateStrategyFromGivenListRecur(tail)
        case Nil =>
          (binarySplit(taskAndTransmissionStarts, varHeuris = cpVar => cpVar.max - cpVar.min)
            ++ conflictOrderingSearch(allVars, minRegret(allVars), allVars(_).min))
      }
    }

    def instantiateSimpleStrategy(s: Strategy.Value): Branching =
      s match {
        case TransmissionRouting =>
          conflictOrderingSearch(
            cpProblem.cpTransmissions.map(_.busID),
            transmissionID => -cpProblem.cpTransmissions(transmissionID).transmissionDuration.size, //the one that has the most impact on the schedule?
            transmissionID => cpProblem.cpTransmissions(transmissionID).busID.min)
        case TaskPlacementLessBuzyProcFirst =>
          //this one works great on Aquascan
          conflictOrderingSearch(
            processorIDChoices,
            taskMaxDurations(_),
            processorIDChoices(_).iterator.toList.maxBy(procID => cpProblem.processorLoadArrayUnderApprox(procID).max))
        case LocalOrBusTransmissionLargestFirstLocalFirst =>
          conflictOrderingSearch(
            cpProblem.cpTransmissions.map(_.isSelfLoopTransmission),
            transmissionID => -cpProblem.cpTransmissions(transmissionID).transmissionDuration.max,
            transmissionID => cpProblem.cpTransmissions(transmissionID).isSelfLoopTransmission.max)
        case TaskPlacementFastestImplemPlusLessBuzyProcFirst =>
          conflictOrderingSearch(
            processorIDChoices,
            taskID => cpProblem.cpTasks(taskID).taskDuration.size,
            taskID => cpProblem.cpTasks(taskID).processorID.minBy(
              processorID => cpProblem.cpTasks(taskID).minTaskDurationOnProcessor(processorID).getOrElse(Int.MaxValue) + cpProblem.processorLoadArrayUnderApprox(processorID).max))
        case SharedImplementationInstances =>
          if (arrayOfNbInstancesOfSharedFunctions.nonEmpty)
            conflictOrderingSearch(
              arrayOfNbInstancesOfSharedFunctions,
              fnID => arrayOfNbInstancesOfSharedFunctions(fnID).max,
              fnID => arrayOfNbInstancesOfSharedFunctions(fnID).min)
          else oscar.algo.search.Branching({
            Seq.empty
          })
        case TaskAndTransmissionStarts =>

          conflictOrderingSearch(
            taskAndTransmissionStarts,
            taskAndTransmissionID => taskAndTransmissionStarts(taskAndTransmissionID).size,
            taskAndTransmissionID => taskAndTransmissionStarts(taskAndTransmissionID).min)


          //time branching from long time ago
//          new SetTimesBranching(taskAndTransmissionStarts, taskAndTransmissionDuration, taskAndTransmissionEnds, identity) //i => -taskAndTransmissionEnds(i).min)
        case LocalOrBusTransmissionLongestAdjFirstNonLocalFirst =>
          conflictOrderingSearch(
            cpProblem.cpTransmissions.map(_.isSelfLoopTransmission),
            transmissionID => -(cpProblem.cpTransmissions(transmissionID).from.taskDuration.size
              + cpProblem.cpTransmissions(transmissionID).to.taskDuration.size
              + cpProblem.cpTransmissions(transmissionID).transmissionDuration.max),
            transmissionID => cpProblem.cpTransmissions(transmissionID).isSelfLoopTransmission.min)

        case HEFT =>
          //HEFT heuristics with backtracking

          def selectMinID[T](ts: Iterable[T], cond:T => Boolean, value:T => Int):Option[(T,Int)] = {
            var toReturn:Option[(T,Int)] = None
            var currentMin = Int.MaxValue
            var i = 0
            for(t <- ts if cond(t)){
              val m = value(t)
              if(m < currentMin) {
                toReturn = Some((t,i))
                currentMin = m
              }
              i =i+1
            }
            toReturn
          }

          val tasksEnds = cpProblem.cpTasks.map(_.end)
          import oscar.algo.search._
          Branching {
            selectMinID[CPIntVar](tasksEnds, x => !x.isBound, _.min) match {
              case None =>
                noAlternative
              case Some((end, id)) =>
                //set it to finish the earliest; select the target accordingly
                branch({
                  solver.add(end === end.min)
                  solver.add(cpProblem.cpTasks(id).processorImplementationCombo === cpProblem.cpTasks(id).processorImplementationCombo.min)
                })(solver.add(end !== end.min))

            }
          }
      }

    instantiateStrategyFromGivenListRecur(a)
  }

  def defaultSearchStrategy: Branching = {
    val allVars = cpProblem.varsToDistribute.toArray
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
      if (arrayOfNbInstancesOfSharedFunctions.nonEmpty)
        conflictOrderingSearch(
          arrayOfNbInstancesOfSharedFunctions,
          fnID => arrayOfNbInstancesOfSharedFunctions(fnID).max,
          fnID => arrayOfNbInstancesOfSharedFunctions(fnID).min)
      else oscar.algo.search.Branching({
        Seq.empty
      })

    def distributeOnLocalOrBusTransmissionLargestFirstLocalFirst =
      conflictOrderingSearch(
        cpProblem.cpTransmissions.map(_.isSelfLoopTransmission),
        transmissionID => -cpProblem.cpTransmissions(transmissionID).transmissionDuration.max,
        transmissionID => cpProblem.cpTransmissions(transmissionID).isSelfLoopTransmission.max)

    def distributeOnLocalOrBusTransmissionLongestAdjFirstNonLocalFirst =
      conflictOrderingSearch(
        cpProblem.cpTransmissions.map(_.isSelfLoopTransmission),
        transmissionID => -(cpProblem.cpTransmissions(transmissionID).from.taskDuration.size
          + cpProblem.cpTransmissions(transmissionID).to.taskDuration.size
          + cpProblem.cpTransmissions(transmissionID).transmissionDuration.max),
        transmissionID => cpProblem.cpTransmissions(transmissionID).isSelfLoopTransmission.min)

    /*
    val taskToPEAndTransmissionToBus: Array[CPIntVar] = (cpProblem.cpTasks.toList.map(_.processorID) ::: cpProblem.cpTransmissions.toList.map(_.busID)).toArray
    //def taskAndTransID(id:Int)=if(id >= cpProblem.cpTasks.length) //transmission
    //else // task

    def distributeTransmissionsAndTasks =
      conflictOrderingSearch(
        taskToPEAndTransmissionToBus,
        (id: Int) => if (id >= cpProblem.cpTasks.length) 1
        else 1,
        (id: Int) => taskToPEAndTransmissionToBus.
        transmissionID

    => cpProblem.cpTransmissions(transmissionID).isSelfLoopTransmission.min
    )*/

    //l'idée est de distribuer sur ce qui a le plus d'impact en premier, que ce soit une transmission ou le placement d'une tâche.
    //pour commencer, on va jouer sur la durée brute:
    //les tâche avec leur durée, et les transmission avec leur durées
    // on fixe la tâche au proco le moins ocupé et la transmission en local first
    // le souci des transmission, c'est qu'elles impactement les tâches, donc on peut aussi décider du placement des tâches

    //on peut ajouter les transmisions local/global et décider sur base des sommes des durées des tâches + de la transmission.

    //pour les transmissions,
    // sélectionner par impact le plus grand = delta entre le min et le max en tenant compte des durées des tâches, qui varient selon le PE
    //sélectionner quoi en priorité?


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
      //distributeOnTaskPlacementFastestImplemPlusLessBuzyProcFirst
       distributeOnTaskPlacementLessBuzyProcFirst
        ++ distributeOnLocalOrBusTransmissionLargestFirstLocalFirst
        ++ distributeOnSharedImplementationInstances
        ++ binarySplit(taskAndTransmissionStarts, varHeuris = cpVar => cpVar.max - cpVar.min)
        ++ conflictOrderingSearch(allVars, minRegret(allVars), allVars(_).min)
      )
  }
}

