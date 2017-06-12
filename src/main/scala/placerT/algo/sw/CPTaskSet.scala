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

package placerT.algo.sw

import oscar.cp
import oscar.cp._
import oscar.cp.core.variables.CPIntVar
import placerT.algo.{CPMappingProblem, Mapper}
import placerT.algo.hw.{CPProcessor, CPMonoTaskProcessor, CPMultiTaskProcessor}
import placerT.metadata.hw.{MonoTaskSwitchingTask, MultiTaskPermanentTasks}
import placerT.metadata.sw.{FlattenedImplementation, TaskSet}

import scala.collection.immutable.SortedMap

case class CPTaskSet(taskSet:TaskSet,
                     explanation: String,
                     mapper: Mapper,
                     maxHorizon: Int,
                     multiTaskProcessors:Array[CPMultiTaskProcessor],
                     monoTaskProcessors:Array[CPMonoTaskProcessor])
  extends CPAbstractTask(mapper) {


  implicit val solver = mapper.solver

  val numberOfTasks = taskSet.numberOfTasks

  val flattenedImplems:Array[FlattenedImplementation] = taskSet.task.implementationArray

  val subTasks: Array[CPTask] = Array.tabulate(numberOfTasks)(
    id => new CPTask(taskSet.task, taskSet.task.name + "_" + id, mapper, maxHorizon)
  )

  val prototypeTask = taskSet.task

  //for each multiTaskprocessor, lists all implementation that can run on it, and defines a variable telling how many of these instance run of it in parallel
  val multiTaskProcessorAndBundles:Array[List[(FlattenedImplementation,CPIntVar)]] =
    multiTaskProcessors.map((multiTaskProcessor:CPMultiTaskProcessor) => {
      val implemsForThisTarget = flattenedImplems.toList.filter(i => i.target == multiTaskProcessor.p.processorClass)
      val numberOfInstancesInParallel = CPIntVar(0 to numberOfTasks)

      //the number of instance in parallel on a given multiTask processor should anyway be smaller or equal to the number of instances running on it, actually
      add(numberOfInstancesInParallel <= sum(subTasks.map((c:CPTask) => c.isRunningOnProcessor(multiTaskProcessor.id))))

      implemsForThisTarget.map((f:FlattenedImplementation) => (f,numberOfInstancesInParallel))
    }
  )

  override val start:CPIntVar = minimum(subTasks.map(_.start))
  override val end:CPIntVar = maximum(subTasks.map(_.end))
  override def duration: cp.CPIntVar = end - start

  //this is for multiTask processors.
  override def buildArrayImplemAndMetricUsage(target: CPMultiTaskProcessor): Option[(Array[cp.CPIntVar], SortedMap[String, Array[Int]])] = {
    val processor = target.p
    val processorClass = processor.processorClass
    prototypeTask.computingHardwareToImplementations.get(processorClass) match {
      case None => None
      case Some(Nil) => None
      case Some(implementations: List[FlattenedImplementation]) =>
        val implementationSubArray = implementations.toArray

        val howManyInstanceOfThisImplementation:Array[CPIntVar] = implementationSubArray.map(implementation => {
          val isImplementationSelected = subTasks.map(subTask => subTask.isImplementationSelected(implementation.id))
          sum(isImplementationSelected)
        })

        val dimAndSizePerImplemSubArray: List[(String, Array[Int])] = processorClass.resources.toList.map((dimension: String) =>
          (dimension, implementationSubArray.map(implementation => implementation.resourceUsage(dimension))))

        val dimToSizesPerImplemSubArrays: SortedMap[String, Array[Int]] = SortedMap.empty[String, Array[Int]] ++ dimAndSizePerImplemSubArray

        Some((howManyInstanceOfThisImplementation, dimToSizesPerImplemSubArrays))
    }
  }

  override def variablesToDistribute: Iterable[CPIntVar] = subTasks.flatMap(_.variablesToDistribute)

  override def couldBeExecutingOnProcessor(procID: Int): Boolean = ???

}
