/*
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

case class SimpleTask(start: CPIntVar, duration: CPIntVar, end: CPIntVar, isNeeded: CPBoolVar)

object SimpleTask extends Constraints {

  def postUnaryResourceFromSimpleTasks(simpleTasks: List[SimpleTask], switchingDelay: Int = 0) {
    val simpleTasksArray = simpleTasks.filter(!_.isNeeded.isFalse).toArray
    val startTimeArray = simpleTasksArray.map(_.start)
    val endArray = simpleTasksArray.map(_.end)
    val durationArray = simpleTasksArray.map(_.duration)
    val isNeededArray = simpleTasksArray.map(_.isNeeded)

    val cp = startTimeArray(0).store
    val naive = false
    if (naive) {
      //ça ne va pas être très efficace du tout.
      if (switchingDelay != 0) {
        for {
          i <- startTimeArray.indices
          j <- i + 1 until startTimeArray.length
        } {
          cp.add((!isNeededArray(i)) || (!isNeededArray(j)) || (endArray(j) + switchingDelay <== startTimeArray(i)) || (endArray(i) + switchingDelay <== startTimeArray(j)))
        }
      }
    } else {
      for (t <- endArray.indices) {
        endArray(t) = endArray(t) + switchingDelay
        durationArray(t) = durationArray(t) + switchingDelay
      }
    }
    cp.add(unaryResource(startTimeArray, durationArray, endArray, isNeededArray))
  }

  def resourceWidthOfUse(simpleTasks: List[SimpleTask]):CPIntVar = {
    val simpleTasksArray = simpleTasks.filter(!_.isNeeded.isFalse).toArray
    val startTimeArray = simpleTasksArray.map(_.start)
    val endTimeArray = simpleTasksArray.map(_.end)
    val isNeededArray = simpleTasksArray.map(_.isNeeded)

    val minimumStartTime:Int = (for (x <- startTimeArray) yield x.getMin).min
    val maximumEndTime:Int = (for (x <- endTimeArray) yield x.getMax).max
    val store = startTimeArray(0).store

    val startTimeArrayMaxIfNotNeeded = Array.tabulate(startTimeArray.length)(
      filteredTaskID => elementVar(IndexedSeq(CPIntVar(maximumEndTime)(store),startTimeArray(filteredTaskID)),isNeededArray(filteredTaskID)))

    val endTimeArrayMinIfNotNeeded = Array.tabulate(startTimeArray.length)(
      filteredTaskID => elementVar(IndexedSeq(CPIntVar(minimumStartTime)(store),endTimeArray(filteredTaskID)),isNeededArray(filteredTaskID)))

    maximum(endTimeArrayMinIfNotNeeded) - minimum(startTimeArrayMaxIfNotNeeded)
  }
}

object CumulativeTask extends Constraints {

  /**
   * posts a cumulative constraint and returns the wdth of the resource
   * @param cumulativeTasks a set of cumulative tasks, which require a certain amount of resource (zero means that it does not use the resource, actually)
   * @param maxResource the maximal amount of available resources
   * @return the width of the resource, that is the spacing before the same usage pattern can be repeated
   */
  def defineResourceWidth(cumulativeTasks: List[CumulativeTask], maxResource: CPIntVar):CPIntVar = {
    val relevantTasks = cumulativeTasks.filter(!_.amount.isBoundTo(0))

    val minimumStartTime:Int = (for (task <- relevantTasks) yield task.start.getMin).min
    val maximumEndTime:Int = (for (task <- relevantTasks) yield task.end.getMax).max
    val store = relevantTasks.head.start.store
    val width = CPIntVar(minimumStartTime,maximumEndTime)(store)

    val mirrorTasks = relevantTasks.map(
    {case CumulativeTask(start, duration, end, amount, explanation) =>
      CumulativeTask(start + width, duration, end + width, amount, explanation + " shifted by width")
    })

    val allRelevantTasks = relevantTasks ++ mirrorTasks

    postCumulativeForSimpleCumulativeTasks(allRelevantTasks, maxResource)

    width
  }

  /**
   * posts a cumulative constraint.
   * @param cumulativeTasks a set of cumulative tasks, which require a certain amount of resource (zero means that it does not use the resource, actually)
   * @param maxResource the maximal amount of available resources
   */

  def postCumulativeForSimpleCumulativeTasks(cumulativeTasks: List[CumulativeTask], maxResource: CPIntVar) {
    val simpleTasksArray = cumulativeTasks.filter(!_.amount.isBoundTo(0)).toArray
    if (simpleTasksArray.length != 0) {
      val startTimeArray = simpleTasksArray.map(_.start)
      val endArray = simpleTasksArray.map(_.end)
      val durationArray = simpleTasksArray.map(_.duration)
      val amountArray = simpleTasksArray.map(_.amount)
      val cp = startTimeArray(0).store

      cp.add(maxCumulativeResource(startTimeArray, durationArray, endArray, amountArray, maxResource))
    }
  }
}

case class CumulativeTask(start: CPIntVar, duration: CPIntVar, end: CPIntVar, amount: CPIntVar, explanation: String)

