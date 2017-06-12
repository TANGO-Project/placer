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

package placerT.algo.hw

import oscar.cp
import oscar.cp._
import oscar.cp.core.variables.CPIntVar
import placerT.algo.{SimpleTask, Mapper}
import placerT.algo.sw.{CPTaskSet, CPAbstractTask, CPTask}
import placerT.metadata.hw.{MultiTaskPermanentTasks, ProcessingElement}

import scala.collection.immutable.SortedMap

/**
 * these processors are represented as bin-packing resoruces, pemanently allocated to the task, even if the only execute for a fraction of the time
 * @param id
 * @param p
 * @param memSize
 */
class CPMultiTaskProcessor(id: Int, p: ProcessingElement, memSize: Int, mapper: Mapper)
  extends CPProcessor(id, p, memSize, mapper) {
  require(p.processorClass.isInstanceOf[MultiTaskPermanentTasks])

  var tasksPotentiallyExecutingHere: List[CPAbstractTask] = List.empty

  val resourceToUsage: SortedMap[String, CPIntVar] = p.resources.mapValues(maxValue => CPIntVar(0, maxValue))

  override def accumulateExecutionConstraintsOnTask(aTask: CPAbstractTask) {
    aTask match{
      case task:CPTask =>
        accumulateTransmissionStorageOnTask(task)

        if (task.couldBeExecutingOnProcessor(id)) {
          tasksPotentiallyExecutingHere = task :: tasksPotentiallyExecutingHere
        }
      case sTask:CPTaskSet =>

    }
  }

  override def close() {

    //collects value for the bin-packing
    val x: List[(Array[CPIntVar], SortedMap[String, Array[Int]])] =
      tasksPotentiallyExecutingHere.flatMap(t => t.buildArrayImplemAndMetricUsage(this))

    val isImplemSelectedSubArray = x.flatMap(_._1).toArray

    //do not put more tasks than allowed by resources (there is no precedence here)
    for ((resource, maxSize) <- p.resources) {
      val metricsForThisDimensionSubArray = x.flatMap(_._2.get(resource).get).toArray
      solver.add(weightedSum(metricsForThisDimensionSubArray, isImplemSelectedSubArray, resourceToUsage(resource)))
    }
    //manage memory
    closeTransmissionAndComputationMemory()
  }

  override def timeWidth: cp.CPIntVar = {
    val simpleTasks = tasksPotentiallyExecutingHere.map(cpTask =>
      new SimpleTask(cpTask.start,cpTask.duration,cpTask.end,cpTask.isRunningOnProcessor(id))
    )
    SimpleTask.resourceWidthOfUse(simpleTasks)
  }
}