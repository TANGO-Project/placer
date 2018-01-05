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

import oscar.cp.core.variables.CPIntVar
import placerT.algo.sw.CPTask
import placerT.algo.{Mapper, SimpleTask}
import placerT.metadata.hw.{MonoTaskSwitchingTask, ProcessingElement}

/**
 * these are represented as unary resources. furthermore, only tasks that can fit on this processor are allowed, statically
 * @param id the ID of the processor (unique, etc) inherited from p
 * @param p the processing element, to get more info
 * @param memSize the max mem size, taken from p
 */
class CPMonoTaskProcessor(id: Int, p: ProcessingElement, memSize: Int, val switchingDelay: Int, val nbCores:Int, mapper: Mapper)
  extends CPProcessor(id, p, memSize, mapper) {
  require(p.processorClass.isInstanceOf[MonoTaskSwitchingTask])
  require(nbCores ==1 || switchingDelay==0, "cannot have switching delay with multi cores")

  var allSimpleTasksPotentiallyExecutingHere: List[SimpleTask] = List.empty
  var allTasksPotentiallyExecutingHere: List[CPTask] = List.empty

  override def accumulateExecutionConstraintsOnTask(task: CPTask) {

    accumulateTransmissionStorageOnTask(task)
    accumulateComputationMemoryOnProcessor(task)

    val isTaskExecutedHere = task.isRunningOnProcessor(id)

    if (!isTaskExecutedHere.isFalse) {
      //could be true, or unbound yet

      allTasksPotentiallyExecutingHere = task :: allTasksPotentiallyExecutingHere

      allSimpleTasksPotentiallyExecutingHere = SimpleTask(
        task.start,
        task.taskDuration,
        task.end,
        isTaskExecutedHere) :: allSimpleTasksPotentiallyExecutingHere
    }
  }

  override def timeWidth: CPIntVar = {
    if (allSimpleTasksPotentiallyExecutingHere.isEmpty) CPIntVar(0)
    else SimpleTask.resourceWidthOfUse(allSimpleTasksPotentiallyExecutingHere)
  }

  override def close() {
    if(nbCores == 1) {
      SimpleTask.postUnaryResourceForSimpleTasks(allSimpleTasksPotentiallyExecutingHere, switchingDelay, origin = "usage of CPMonoTaskProcessor" + p.name)
    }else{
      SimpleTask.postCumulativeResourceForSimpleTasks(allSimpleTasksPotentiallyExecutingHere, CPIntVar(nbCores), origin = "usage of CPMonoTaskProcessor" + p.name)
    }
    closeTransmissionAndComputationMemory()
  }
}
