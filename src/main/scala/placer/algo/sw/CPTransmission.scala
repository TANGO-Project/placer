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


package placer.algo.sw

import oscar.cp
import oscar.cp._
import oscar.cp.core.CPSol
import oscar.cp.core.variables.CPIntVar
import oscar.cp.modeling.Constraints
import placer.algo.hw.CPHardwareModel
import placer.metadata.sw.TransmissionTiming.TransmissionTiming
import placer.metadata.sw.{Transmission, TransmissionTiming}


case class CPTransmission(id: Int,
                          transmission: Transmission,
                          from: CPTask,
                          to: CPTask,
                          cpHardwareModel:CPHardwareModel,
                          size: Int,
                          explanation: String,
                          timing: TransmissionTiming)
  extends CPAbstractTask() with Constraints{

  implicit val store = cpHardwareModel.store
  val maxHorizon  = cpHardwareModel.maxHorizon
  val busses = cpHardwareModel.cpBusses
  val localLoopBusses = cpHardwareModel.selfLoopBussesID //they are included in busses
  val processorToBusToProcessorAdjacency = cpHardwareModel.processorToBusToProcessorAdjacency

  val start: CPIntVar = CPIntVar(0, maxHorizon)
  val end: CPIntVar = CPIntVar(0, maxHorizon)

  val busID: CPIntVar = CPIntVar.sparse(busses.indices)

  @deprecated("should be removed for faster stuff!")
  val isOccurringOnBus: Array[CPBoolVar] = busses.map(bus => busID isEq bus.id)

  val isSelfLoopTransmission:CPBoolVar = busID.isIn(localLoopBusses)

  val originProcessorID = from.processorID
  val destinationProcessorID = to.processorID

  store.add(table(originProcessorID, busID, destinationProcessorID, processorToBusToProcessorAdjacency))

  val busToDuration = Array.tabulate(busses.length)(busID => busses(busID).transmissionDuration(transmission.size))

  val minDuration = busToDuration.min
  val maxDuration = busToDuration.max

  val transmissionDuration: CPIntVar = CPIntVar(minDuration,maxDuration)

  store.add(element(busToDuration,busID,transmissionDuration))

  store.add(end === (start + transmissionDuration))

  from.addOutgoingTransmission(this)
  to.addIncomingTransmission(this)

  store.add(from.end <= start)
  store.add(end <= to.start)

  timing match{
    case TransmissionTiming.Free | TransmissionTiming.Sticky =>
      //if localLoop then force asap
      store.add(isSelfLoopTransmission implies (start ?=== (from.end)))
    case _ => ;
  }

  def transmissionDuration(sol:CPSol):Int = {
    busToDuration(sol(busID))
  }

  override def variablesToDistribute: Iterable[CPIntVar] = List(start, busID)

  override def variablesToSave: Iterable[cp.CPIntVar] = List(start, end, transmissionDuration, busID)
}

