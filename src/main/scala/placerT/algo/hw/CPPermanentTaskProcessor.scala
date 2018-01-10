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

package placerT.algo.hw

import oscar.cp
import oscar.cp._
import oscar.cp.core.variables.CPIntVar
import placerT.algo.{Mapper, SimpleTask}
import placerT.algo.sw.CPTask
import placerT.metadata.hw.{MultiTaskPermanentTasks, ProcessingElement}
import placerT.metadata.sw.FlattenedImplementation

import scala.collection.immutable.SortedMap

/**
 * these processors are represented as bin-packing resources, permanently allocated to the task, even if the only execute for a fraction of the time
 * @param id
 * @param p
 * @param memSize
 */
class CPPermanentTaskProcessor(id: Int, p: ProcessingElement, memSize: Int, mapper: Mapper) extends CPProcessor(id, p, Some(memSize), mapper) {
  require(p.processorClass.isInstanceOf[MultiTaskPermanentTasks])

  var tasksPotentiallyExecutingHere: List[CPTask] = List.empty

  //kept in a separated list because the resources must be counted differently and multiplied by the number of instances located on this PE
  var hostedSharedImplementations: List[CPInstantiatedPermanentFunction] = List.empty

  val resourceToUsage: SortedMap[String, CPIntVar] = p.resources.mapValues(maxValue => CPIntVar(0, maxValue))

  def accumulateExecutionConstraintOnSharedImplementation(virtualProcessor:CPInstantiatedPermanentFunction){
    hostedSharedImplementations = virtualProcessor :: hostedSharedImplementations
  }

  override def accumulateExecutionConstraintsOnTask(task: CPTask) {
    accumulateTransmissionStorageOnTask(task)
    accumulateComputationMemoryOnProcessor(task)

    val isTaskExecutedHere = task.isRunningOnProcessor(id)
    if (!isTaskExecutedHere.isFalse) {
      tasksPotentiallyExecutingHere = task :: tasksPotentiallyExecutingHere
    }
  }

  override def close() {

    require(hostedSharedImplementations.forall(_.isClosed))

    //for non shared implems
    val x: List[(Array[CPBoolVar], SortedMap[String, Array[Int]])] =
      tasksPotentiallyExecutingHere.flatMap(t => t.buildArrayImplemAndMetricUsage(this))

    val isImplemSelected:List[CPIntVar] = x.flatMap(_._1)

    //for shared implems
    val y:List[(CPIntVar,SortedMap[String,Int])] = hostedSharedImplementations.map(_.nbInstanceAndResourceUsage)

    val nbInstancesOfSharedImplems = y.map(_._1)
    val requirementsForSharedImplems = y.map(_._2)

    for ((resource, maxSize) <- p.resources) {
      val requirementsForThisDimensionUsedByNonSharedImplems:List[Int] = x.flatMap(_._2.get(resource).get)
      val requirementsForThisDimensionUsedBySharedImplems:List[Int] = requirementsForSharedImplems.map(_.get(resource).getOrElse(0))
      val concatenatedRequirementsArray:Array[Int] = (requirementsForThisDimensionUsedByNonSharedImplems ++ requirementsForThisDimensionUsedBySharedImplems).toArray

      val concatenatedUsageArray:Array[CPIntVar] = (isImplemSelected ++ nbInstancesOfSharedImplems).toArray

      println("isFailed:" + solver.isFailed)
      solver.add(weightedSum(concatenatedRequirementsArray,concatenatedUsageArray,resourceToUsage(resource)))
    }

    closeTransmissionAndComputationMemory()
  }

  override def timeWidth: cp.CPIntVar = {
    val simpleTasks = tasksPotentiallyExecutingHere.map(cpTask =>
      new SimpleTask(cpTask.start,cpTask.taskDuration,cpTask.end,cpTask.isRunningOnProcessor(id))
    )
    if(simpleTasks.isEmpty) CPIntVar(0)
    else SimpleTask.resourceWidthOfUse(simpleTasks)
  }
}

