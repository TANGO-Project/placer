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
 * You should have received a copy of the GNU Lesser General Public License along with OscaR.
 * If not, see http://www.gnu.org/licenses/lgpl-3.0.en.html
 *
 */

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
 * You should have received a copy of the GNU Lesser General Public License along with OscaR.
 * If not, see http://www.gnu.org/licenses/lgpl-3.0.en.html
 *
 */

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
 * You should have received a copy of the GNU Lesser General Public License along with OscaR.
 * If not, see http://www.gnu.org/licenses/lgpl-3.0.en.html
 *
 */

package main.scala.placerT.algo.hw

import main.scala.placerT.algo.Mapper
import main.scala.placerT.algo.sw.CPTask
import main.scala.placerT.metadata.hw.{MultiTaskPermanentTasks, ProcessingElement}
import oscar.cp._
import oscar.cp.core.variables.CPIntVar

import scala.collection.immutable.SortedMap

/**
 * these processors are represented as bin-packing resoruces, pemanently allocated to the task, even if the only execute for a fraction of the time
 * @param id
 * @param p
 * @param memSize
 */
class CPMultiTaskProcessor(id: Int, p: ProcessingElement, memSize: Int, mapper: Mapper) extends CPProcessor(id, p, memSize, mapper) {
  require(p.processorClass.isInstanceOf[MultiTaskPermanentTasks])

  var tasksPotentiallyExecutingHere: List[CPTask] = List.empty

  val resourceToUsage: SortedMap[String, CPIntVar] = p.resources.mapValues(maxValue => CPIntVar(0, maxValue))

  override def accumulateExecutionConstraintsOnTask(task: CPTask) {
    accumulateTransmissionStorageOnTask(task)
    val isTaskExecutedHere = task.isRunningOnProcessor(id)
    if (!isTaskExecutedHere.isFalse) {
      tasksPotentiallyExecutingHere = task :: tasksPotentiallyExecutingHere
    }
  }

  override def close() {
    val x: List[(Array[CPBoolVar], SortedMap[String, Array[Int]])] =
      tasksPotentiallyExecutingHere.flatMap(t => t.buildArrayImplemAndMetricUsage(this))

    val isImplemSelectedSubArray = x.flatMap(_._1).toArray.asInstanceOf[Array[CPIntVar]]

    for ((resource, maxSize) <- p.resources) {
      val metricsForThisDimensionSubArray = x.flatMap(_._2.get(resource).get).toArray
      solver.add(weightedSum(metricsForThisDimensionSubArray, isImplemSelectedSubArray, resourceToUsage(resource)))
    }

    closeTransmissionAndComputationMemory()
  }

}