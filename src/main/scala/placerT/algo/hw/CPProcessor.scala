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

import oscar.cp._
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
abstract class CPProcessor(val id: Int, val p: ProcessingElement, memSize: Int, mapper: Mapper) {

  implicit val solver = mapper.solver

  private var temporaryStorages: List[CumulativeTask] = List.empty

  private def accumulateTemporaryStorage(from: CPIntVar, duration: CPIntVar, to: CPIntVar, amount: CPIntVar, explanation: String) {
    temporaryStorages = CumulativeTask(from, duration, to, amount, explanation) :: temporaryStorages
  }

  def accumulateExecutionConstraintsOnTask(task: CPTask)

  def accumulateComputationMemoryOnProcessor(task: CPTask) {
    val isTaskExecutedHere = task.isRunningOnProcessor(this.id)
    if (!isTaskExecutedHere.isFalse) {
      accumulateTemporaryStorage(
        task.start,
        task.duration,
        task.end,
        isTaskExecutedHere * task.computationMemory,
        "temporary storage of " + task.explanation)
    }
  }

  protected def accumulateTransmissionStorageOnTask(task: CPTask) {
    val isTaskExecutedHere = task.isRunningOnProcessor(this.id)

    if (!isTaskExecutedHere.isFalse) {
      //true or not decided yet; if false, we have nothing to do

      // the code here  ensures that the storage is maintained during the execution of the task.

      //storage of incoming transmissions
      for (incomingTransmission <- task.incomingCPTransmissions) {
        val storageStart = incomingTransmission.start
        val storageDuration = task.start - 1 - incomingTransmission.start
        val storageEnd = task.start - 1

        accumulateTemporaryStorage(
          storageStart,
          storageDuration,
          storageEnd,
          isTaskExecutedHere * incomingTransmission.size,
          "incoming data from " + incomingTransmission.explanation + " before start of " + task.explanation)

        incomingTransmission.timing match {
          case Alap => //we have to constraint the arrival time here
            add(incomingTransmission.end === (task.start - 1))
          case _ =>
            //we are on the other side, the simple constraint is enough
            add(incomingTransmission.end < task.start)
        }
      }

      //storage of outgoing transmissions
      for (outGoingTransmission <- task.outgoingCPTransmissions) {
        accumulateTemporaryStorage(
          task.end + 1,
          duration = outGoingTransmission.end - task.end - 1,
          outGoingTransmission.end,
          isTaskExecutedHere * outGoingTransmission.size,
          "outgoing data from " + task.explanation + " before transmission " + outGoingTransmission.explanation)

        outGoingTransmission.timing match {
          case Asap =>
            //we have to constraint the departure time here
            add(outGoingTransmission.start === (task.end + 1))
          case _ =>
            //we are on the other side, the simple constraint is enough
            add(task.end < outGoingTransmission.start)
        }
      }
    }
  }

  def closeTransmissionAndComputationMemory(): Unit = {
    if (temporaryStorages.isEmpty) {
      println("WARNING: no temporary storage will ever be used on " + p.name)
    } else {
      CumulativeTask.postCumulativeForSimpleCumulativeTasks(temporaryStorages, CPIntVar(memSize))
    }
  }

  def temporaryStorageWidth:CPIntVar = {
    //TODO: this is redundant with the "postCumulativeForSimpleCumulativeTasks" in closeTransmissionAndComputationMemory
    CumulativeTask.defineResourceWidth(temporaryStorages,CPIntVar(memSize))
  }

  def timeWidth:CPIntVar

  def close()
}
