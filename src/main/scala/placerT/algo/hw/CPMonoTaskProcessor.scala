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

import main.scala.placerT.algo.sw.CPTask
import main.scala.placerT.algo.{Mapper, SimpleTask}
import main.scala.placerT.metadata.hw.{MonoTaskSwitchingTask, ProcessingElement}

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

  override def accumulateExecutionConstraintsOnTask(task: CPTask) {
    accumulateTransmissionStorageOnTask(task)
    val isTaskExecutedHere = task.isRunningOnProcessor(id)

    if (!isTaskExecutedHere.isFalse) {
      //could be true, or unbound yet

      allTasksPotentiallyExecutingHere = task :: allTasksPotentiallyExecutingHere

      allSimpleTasksPotentiallyExecutingHere = SimpleTask(
        task.start,
        task.duration,
        task.end,
        isTaskExecutedHere) :: allSimpleTasksPotentiallyExecutingHere
    }
  }

  override def close() {
    SimpleTask.postUnaryResourceFromSimpleTasks(allSimpleTasksPotentiallyExecutingHere, switchingDelay)
    closeTransmissionAndComputationMemory()
  }
}
