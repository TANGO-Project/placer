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


package placerT.algo.sw

import oscar.cp
import oscar.cp._
import oscar.cp.core.CPSol
import oscar.cp.core.variables.CPIntVar
import oscar.cp.modeling.Constraints
import placerT.algo.Mapper
import placerT.algo.hw.CPBus
import placerT.metadata.sw.TransmissionTiming.TransmissionTiming
import placerT.metadata.sw.{TransmissionTiming, Transmission}
import placerT.metadata.sw.TransmissionTiming.TransmissionTiming

case class CPTransmission(id: Int,
                          transmission: Transmission,
                          from: CPTask,
                          to: CPTask,
                          busses: Array[CPBus],
                          size: Int,
                          explanation: String,
                          timing: TransmissionTiming,
                          mapper: Mapper,
                          maxHorizon: Int,
                          processorToBusToProcessorAdjacency: Iterable[(Int, Int, Int)],
                          localLoopBusses:Set[Int])
  extends CPAbstractTask(mapper) with Constraints{

  //  println("instantiating transmission " + transmission.name)
  // println("from occuring where:" + from.occuringOnProcDebugInfo)
  //  println("to occuring where:" + to.occuringOnProcDebugInfo)

  implicit val solver = mapper.solver

  val start: CPIntVar = CPIntVar(0, maxHorizon)
  val end: CPIntVar = CPIntVar(0, maxHorizon)

  val busID: CPIntVar = CPIntVar.sparse(busses.indices)
  val isOccurringOnBus: Array[CPBoolVar] = busses.map(bus => busID isEq bus.id)

  def occuringOnBussesDebugInfo1:String = "occuringOnBusses:[" + isOccurringOnBus.mkString(",") + "]"

  val originProcessorID = from.processorID
  val destinationProcessorID = to.processorID

  add(table(originProcessorID, busID, destinationProcessorID, processorToBusToProcessorAdjacency))

  val busAndDuration = busses.toList.map(bus => (bus.id, bus.transmissionDuration(transmission.size)))
  val busAndDurationNZ = busAndDuration.filter(_._2!=0)

  val possibleDurationsNZ = busAndDurationNZ.map(_._2)
  val stubValueForDurationNZ = possibleDurationsNZ.min

  val busWithTransmissionNZ = busAndDurationNZ.map(_._1).toSet
  val busWithTransmissionZ = busAndDuration.filter(_._2==0).map(_._1).toSet

  val busAndDurationNZWithStub = busAndDurationNZ.toList ::: busWithTransmissionZ.toList.map(bus => (bus,stubValueForDurationNZ))

  val transmissionDurationNZ2: CPIntVar = CPIntVar(possibleDurationsNZ)

  add(table(busID, transmissionDurationNZ2, busAndDurationNZWithStub))

  add(or(List(busID isIn busWithTransmissionZ,end isEq (start + transmissionDurationNZ2))))
  add(or(List(busID isIn busWithTransmissionNZ,end isEq start)))

  //This is to be used to represent usage of regular busses
  val endNZ: CPIntVar = start + transmissionDurationNZ2

  from.addOutgoingTransmission(this)
  to.addIncomingTransmission(this)

  //these are redundant, since the timing is also constrained by the storage task on both side of the transmission
  add(from.end <= start)
  add(end <= to.start)  //TODO: there must be an equal here, or it fails.

  timing match{
    case TransmissionTiming.Free | TransmissionTiming.Sticky =>
      //if localLoop then force asap
      add((busID isIn localLoopBusses) implies (start ?=== (from.end+1)))
    case _ => ;
  }

  def transmissionDuration(sol:CPSol):Int = {
    val selectedBusID = sol(busID)
    if(busWithTransmissionZ contains selectedBusID) 0
    else sol(transmissionDurationNZ2)
  }

  override def variablesToDistribute: Iterable[CPIntVar] = List(start, busID)

  override def variablesToSave: Iterable[cp.CPIntVar] = List(start, end, transmissionDurationNZ2, busID)
}