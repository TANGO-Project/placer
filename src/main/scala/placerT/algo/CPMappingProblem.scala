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


package placerT.algo

import oscar.cp._
import oscar.cp.core.CPSol
import placerT.algo.hw.{CPBus, CPProcessor, CPRegularBus, CPSelfLoopBus}
import placerT.algo.sw.{CPTask, CPTransmission}
import placerT.metadata.{Mapping, MappingProblem, TaskMapping}
import placerT.metadata.hw.{Bus, ProcessingElement}
import placerT.metadata.sw.FlattenedImplementation


case class CPMappingProblem(mappingProblem: MappingProblem,
                            hardwareName: String,
                            cpTasks: Array[CPTask],
                            cpProcessors: Array[CPProcessor],
                            cpBusses: Array[CPBus],
                            cpTransmissions: Array[CPTransmission],
                            makeSpan: CPIntVar,
                            energy: CPIntVar,
                            widthVar:Option[CPIntVar],
                            processorLoadArrayUnderApprox:Array[CPIntVar]) {

  def getMapping(sol: CPSol): Mapping = {

    def proc(procID: CPIntVar): ProcessingElement = cpProcessors(sol(procID)).p
    def implem(task: CPTask): FlattenedImplementation = task.task.implementationArray(sol(task.implementationID))

    val firstTaskMapping = cpTasks.map(cpTask =>
      TaskMapping(cpTask.task, proc(cpTask.processorID), implem(cpTask),sol(cpTask.start), sol(cpTask.taskDuration), sol(cpTask.end)))

    def bus(cPTransmission: CPTransmission): Bus = cPTransmission.busses(sol(cPTransmission.busID)) match {
      case c: CPSelfLoopBus => c.selfLoopBus
      case b: CPRegularBus => b.bus
    }

    val transmissionMapping = cpTransmissions.map(trans =>
      (trans.transmission, proc(trans.from.processorID), proc(trans.to.processorID), bus(trans), sol(trans.start), trans.transmissionDuration(sol), sol(trans.end)))

    new Mapping(hardwareName, firstTaskMapping, transmissionMapping, sol(makeSpan), sol(energy),widthVar match{case None => None case Some(w) => Some(sol(w))})
  }

  def varsToDistribute: List[CPIntVar] = {
    List.empty ++
      cpTasks.flatMap(task => List(task.start,task.implementationID,task.processorID)) ++
      cpTransmissions.flatMap(_.variablesToDistribute) //  override def variablesToDistribute: Iterable[CPIntVar] = List(start, busID)
  }

  def varsToSave: List[CPIntVar] = List(makeSpan,energy) ++ cpTasks.flatMap(_.variablesToSave) ++ cpTransmissions.flatMap(_.variablesToSave) ++ widthVar

}
