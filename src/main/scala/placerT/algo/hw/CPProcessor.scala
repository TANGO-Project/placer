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
import oscar.cp.constraints.Or
import oscar.cp.core.NoSolutionException
import oscar.cp.core.variables.CPIntVar
import placerT.algo.sw.CPTask
import placerT.algo.{CumulativeTask, Mapper}
import placerT.metadata.hw.ProcessingElement
import placerT.metadata.sw.TransmissionTiming._

/**
  * @param id
  * @param p
  * @param memSize the memory used for storing incoming and outgoing data, also used for computation memory of tasks te memory for data in and out is maintained during task execution.
  * @param mapper
  */
abstract class CPProcessor(val id: Int, val p: ProcessingElement, memSize: Option[Int], mapper: Mapper) {

  implicit val solver = mapper.solver

  def addDocumented(c: cp.core.Constraint,origin:String){
    try {
      add(c)
    }catch{
      case n:NoSolutionException =>
        val exception = new NoSolutionException("error on constraint " + origin + "\n" + n)
        //exception.setStackTrace(n.getStackTrace)
        throw exception
    }
  }

  private var temporaryStorages: List[CumulativeTask] = List.empty

  private def accumulateTemporaryStorage(from: CPIntVar, duration: CPIntVar, to: CPIntVar, amount: CPIntVar, explanation: String) {
    temporaryStorages = CumulativeTask(from, duration, to, amount, explanation) :: temporaryStorages
  }

  def accumulateExecutionConstraintsOnTask(task: CPTask)

  def accumulateComputationMemoryOnProcessor(task: CPTask,processorID:Int = id) {
    val isTaskExecutedHere = task.isRunningOnProcessor(processorID)
    if (!isTaskExecutedHere.isFalse) {
      accumulateTemporaryStorage(
        task.start,
        task.taskDuration,
        task.end,
        isTaskExecutedHere * task.computationMemory,
        "working memory of task " + task.explanation)
    }
  }

  def accumulateTransmissionStorageOnTask(task: CPTask, processorID:Int = id) {
    val isTaskExecutedHere = task.isRunningOnProcessor(processorID)

    //if transmission is a self-loop, is is constrained to occur
    if (!isTaskExecutedHere.isFalse) {
      //true or not decided yet; if false, we have nothing to do

      // the code here  ensures that the storage is maintained during the execution of the task.
      //storage of incoming transmissions
      for (incomingTransmission <- task.incomingCPTransmissions) {
        //buffer for incoming data

        accumulateTemporaryStorage(
          incomingTransmission.start,
           task.start - incomingTransmission.start,
          task.start,
          isTaskExecutedHere * incomingTransmission.size,
          "data buffer of incoming transmission " + incomingTransmission.explanation + " waiting for task " + task.explanation)

        incomingTransmission.timing match {
          case Alap => //we have to constraint the arrival time here
            addDocumented(incomingTransmission.end === (task.start),"ALAP constraint on transmission " + incomingTransmission.transmission.name)
          case Sticky =>
            addDocumented(
              new Or(Array(
                incomingTransmission.end ?=== (task.start),
                incomingTransmission.start ?=== (incomingTransmission.from.end)))
              ,"Sticky constraint on transmission " + incomingTransmission.transmission.name)

          case _ =>
            //we are on the other side, the simple constraint is enough
            //notice that it must be a <= because global timing is computed in this way!
            addDocumented(incomingTransmission.end <= task.start,"precedence constraint on incoming transmission " + incomingTransmission.transmission.name)
        }
      }

      //storage of outgoing transmissions
      for (outGoingTransmission <- task.outgoingCPTransmissions) {

        //buffer for outgoing data.
        accumulateTemporaryStorage(
          task.end,
          outGoingTransmission.end - task.end,
          outGoingTransmission.end,
          isTaskExecutedHere * outGoingTransmission.isSelfLoopTransmission.not * outGoingTransmission.size,
          "data buffer outgoing task " + task.explanation + " before transmission " + outGoingTransmission.explanation)

        outGoingTransmission.timing match {
          case Asap =>
            //we have to constraint the departure time here
            addDocumented(task.end === outGoingTransmission.start,"ASAP constraint on transmission " + outGoingTransmission.transmission.name)
          case _ =>
            //Sticky or asap
            //we are on the other side, the simple constraint is enough
            //notice that it must be a <= because global timing is computed in this way!
            addDocumented(task.end <= outGoingTransmission.start,"precedence constraint on outgoing transmission " + outGoingTransmission.transmission.name + " task.end:" + task.end + " outGoingTransmission.start:" + outGoingTransmission.start)
        }
      }
    }
  }

  def closeTransmissionAndComputationMemory(memoryVar:CPIntVar = CPIntVar(memSize.get)): Unit = {
    if (temporaryStorages.isEmpty) {
      System.err.println("WARNING: no temporary storage will ever be used on " + p.name)
    } else {
      CumulativeTask.postCumulativeForSimpleCumulativeTasks(temporaryStorages, memoryVar,"temporary storage for processor " + p.name)
    }
  }

  def temporaryStorageWidth:CPIntVar = {
    //TODO: this is redundant with the "postCumulativeForSimpleCumulativeTasks" in closeTransmissionAndComputationMemory
    if(temporaryStorages.isEmpty) CPIntVar(0)
    else CumulativeTask.defineResourceWidth(temporaryStorages,CPIntVar(memSize.get),"temporaryStorageWidth for processor " + p.name)
  }

  def timeWidth:CPIntVar

  def close()
}
