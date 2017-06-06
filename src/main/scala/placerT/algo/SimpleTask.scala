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
}

object CumulativeTask extends Constraints {
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

