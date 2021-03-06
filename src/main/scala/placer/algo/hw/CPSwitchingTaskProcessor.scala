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


package placer.algo.hw

import oscar.cp.core.variables.CPIntVar
import placer.algo.sw.CPTask
import placer.algo.{CumulativeTask, Mapper, SimpleTask}
import placer.metadata.hw.{ProcessingElement, SwitchingTask}
import placer.metadata.sw.FlattenedImplementationConcrete

import scala.collection.immutable.SortedMap


abstract class AbstractCPSwitchingTaskProcessor(id: Int, p: ProcessingElement, memSize: Option[Int], mapper: Mapper)
  extends CPProcessor(id, p, memSize, mapper) {

  var allSimpleTasksPotentiallyExecutingHere: List[CumulativeTask] = List.empty
  var allTasksPotentiallyExecutingHere: List[CPTask] = List.empty

  override def accumulateExecutionConstraintsOnTask(task: CPTask) {

    accumulateTransmissionStorageOnTask(task)
    accumulateComputationMemoryOnProcessor(task)

    val isTaskExecutedHere = task.isRunningOnProcessor(id)

    if (!isTaskExecutedHere.isFalse) {
      //could be true, or unbound yet

      allTasksPotentiallyExecutingHere = task :: allTasksPotentiallyExecutingHere

      allSimpleTasksPotentiallyExecutingHere = CumulativeTask(
        task.start,
        task.taskDuration,
        task.end,
        isTaskExecutedHere * task.nbThreads,
        "threads of task " + task.task.name) :: allSimpleTasksPotentiallyExecutingHere
    }
  }

  override def timeWidth(verbose:Boolean): CPIntVar = {
    if (allSimpleTasksPotentiallyExecutingHere.isEmpty) CPIntVar(0)
    else SimpleTask.resourceWidthOfUse(allSimpleTasksPotentiallyExecutingHere.map((c:CumulativeTask) => SimpleTask(c.start,c.duration,c.end,c.amount.isEq(1))))
  }
}

case class CPInstantiatedPermanentFunction(override val id: Int, host:CPPermanentTaskProcessor, sharedImplementation:FlattenedImplementationConcrete, maxMemSize:Int, maxCores:Int, mapper: Mapper)
  extends AbstractCPSwitchingTaskProcessor(id: Int, host.p, None, mapper: Mapper){

  val usedMem = CPIntVar(0,maxMemSize)
  val nbInstances = CPIntVar(0,maxCores)

  //we need to check this because there are two possible closing fo this PE class
  var isClosed = false

  override def close(verbose:Boolean){
    require(!isClosed)
    if(allSimpleTasksPotentiallyExecutingHere.isEmpty) nbInstances.assign(0)
    else CumulativeTask.postCumulativeForSimpleCumulativeTasks(allSimpleTasksPotentiallyExecutingHere, nbInstances, origin = "usage of CPInstantiatedPermanentFunction" + p.name,verbose:Boolean)
    //we do not close anything about memory since this is incorporated into the hosting PE
    isClosed = true
  }

  def nbInstanceAndResourceUsage: (CPIntVar, SortedMap[String, Int]) = {
    (nbInstances,sharedImplementation.resourceUsage)
  }

  def varsToDistribute:List[CPIntVar] = List(nbInstances)

  def variablesToSave:List[CPIntVar] = List(nbInstances)

  //we do not consider these here since they are considered at the level of the hosting PE
  override def accumulateTransmissionStorageOnTask(task:CPTask,processorID:Int = id){
    host.accumulateTransmissionStorageOnTask(task,this.id)
  }
  override def accumulateComputationMemoryOnProcessor(task:CPTask,processorID:Int = id){
    host.accumulateComputationMemoryOnProcessor(task,this.id)
  }
}

/**
  * these are represented as unary resources. furthermore, only tasks that can fit on this processor are allowed, statically
  * @param id the ID of the processor (unique, etc) inherited from p
  * @param p the processing element, to get more info
  * @param memSize the max mem size, taken from p
  */
class CPSwitchingTaskProcessor(id: Int, p: ProcessingElement, memSize: Int, val switchingDelay: Int, val nbCores:Int, mapper: Mapper)
  extends AbstractCPSwitchingTaskProcessor(id: Int, p: ProcessingElement, Some(memSize), mapper: Mapper){

  require(p.processorClass.isInstanceOf[SwitchingTask])
  require(nbCores ==1 || switchingDelay==0, "cannot have switching delay with multi cores")

  override def timeWidth(verbose:Boolean): CPIntVar = {
    if (allSimpleTasksPotentiallyExecutingHere.isEmpty) CPIntVar(0)
    else SimpleTask.resourceWidthOfUse(allSimpleTasksPotentiallyExecutingHere.map((c:CumulativeTask) => SimpleTask(c.start,c.duration,c.end,c.amount.isEq(1))))
  }

  override def close(verbose:Boolean) {
    if(nbCores == 1) {
      SimpleTask.postUnaryResourceForSimpleTasks(allSimpleTasksPotentiallyExecutingHere.map((c:CumulativeTask) => SimpleTask(c.start,c.duration,c.end,c.amount.isEq(1))), switchingDelay, origin = "usage of CPSwitchingTaskProcessor" + p.name)
    }else{
      CumulativeTask.postCumulativeForSimpleCumulativeTasks(allSimpleTasksPotentiallyExecutingHere, CPIntVar(nbCores),origin = "usage of CPSwitchingTaskProcessor" + p.name,verbose)
    }
    closeTransmissionAndComputationMemory(verbose = verbose)
  }
}
