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

import oscar.cp.core.variables.CPIntVar
import placerT.algo.sw.{CPTaskSet, CPAbstractTask, CPTask}
import placerT.algo.{Mapper, SimpleTask}
import placerT.metadata.hw.{MonoTaskSwitchingTask, ProcessingElement}

/**
 * these are represented as unary resources. furthermore, only tasks that can fit on this processor are allowed, statically
 * @param id
 * @param p
 * @param memSize
 */
class CPMonoTaskProcessor(id: Int, p: ProcessingElement, memSize: Int, switchingDelay: Int, mapper: Mapper)
  extends CPProcessor(id, p, memSize, mapper) {
  require(p.processorClass.isInstanceOf[MonoTaskSwitchingTask])

  var allSimpleTasksPotentiallyExecutingHere: List[SimpleTask] = List.empty
  var allTasksPotentiallyExecutingHere: List[CPTask] = List.empty

  override def accumulateExecutionConstraintsOnTask(aTask: CPAbstractTask) {
    aTask match{
      case task:CPTask =>
        accumulateTransmissionStorageOnTask(task)

        if (task.couldBeExecutingOnProcessor(id)) {
          //could be true, or unbound yet

          allTasksPotentiallyExecutingHere = task :: allTasksPotentiallyExecutingHere

          allSimpleTasksPotentiallyExecutingHere = SimpleTask(
            task.start,
            task.duration,
            task.end,
            task.isRunningOnProcessor(id)) :: allSimpleTasksPotentiallyExecutingHere
        }
      case s:CPTaskSet =>

    }
  }

  override def timeWidth: CPIntVar =
    SimpleTask.resourceWidthOfUse(allSimpleTasksPotentiallyExecutingHere)

  override def close() {
    SimpleTask.postUnaryResourceFromSimpleTasks(allSimpleTasksPotentiallyExecutingHere, switchingDelay)
    closeTransmissionAndComputationMemory()
  }
}
