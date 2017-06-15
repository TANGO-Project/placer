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
import placerT.algo.{CumulativeTask, SimpleTask, Mapper}
import placerT.algo.sw.{CPTaskSet, CPAbstractTask, CPTask}
import placerT.metadata.hw.{MultiTaskPermanentTasks, ProcessingElement}
import placerT.metadata.sw.{TaskSet, AtomicTask}

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

    //precedence constraints for subtasks of taskSets
    for(aTask <- tasksPotentiallyExecutingHere){
      aTask match{
        case c:CPTask => ; //we are looking for task set only
        case s:CPTaskSet =>
          val (subTasks,implemsAndNbInstances) = s.cpTasksAndNbInstancesForMultiProcessor(this)
          for((implem,nbInstances) <- implemsAndNbInstances){
            var taskAcc:List[CumulativeTask] = List.empty
            for(subTask <- subTasks){
              if(!subTask.isRunningOnProcessor(id).isFalse && !subTask.isImplementationSelected(implem.id).isFalse) {
                val simpleTask = CumulativeTask(
                  subTask.start,
                  subTask.duration,
                  subTask.end,
                  subTask.isRunningOnProcessor(id) && subTask.isImplementationSelected(implem.id),
                  "subtask of:" + s.explanation + " implem:" + implem)
                taskAcc = simpleTask :: taskAcc
              }
            }
            if(taskAcc.isEmpty){
              add(nbInstances == 0)
            }else{
              CumulativeTask.postCumulativeForSimpleCumulativeTasks(taskAcc,nbInstances)
            }
          }
      }
    }


    //manage memory
    closeTransmissionAndComputationMemory()
  }

  override def timeWidth: cp.CPIntVar = {
    val simpleTasks = tasksPotentiallyExecutingHere.map({
        case a:CPTask => new SimpleTask(a.start,a.duration,a.end,a.isRunningOnProcessor(id))
        case s:CPTaskSet => ???
      }

    )
    SimpleTask.resourceWidthOfUse(simpleTasks)
  }
}