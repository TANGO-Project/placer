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
import placerT.algo.sw.CPTransmission
import placerT.algo.{Mapper, SimpleTask}
import placerT.metadata.hw.{Bus, ProcessingElement, SelfLoopBus}



abstract class CPBus(val id: Int, mapper: Mapper) {

  def accumulatePotentialTransmissionOnBus(transmission: CPTransmission)

  def transmissionDuration(size: Int): Int

  def close()

  private var width:Option[CPIntVar] = None

  protected def buildTimeWidth:CPIntVar

  final def timeWidth:CPIntVar = {
    width match{
      case Some(w) => w
      case None =>
        val toReturn = buildTimeWidth
        width = Some(toReturn)
        toReturn
    }
  }
}

case class CPRegularBus(override val id: Int, bus: Bus, mapper: Mapper) extends CPBus(id: Int, mapper: Mapper) {

  private var allSimpleTasksPotentiallyExecutingHere: List[SimpleTask] = List.empty

  override def accumulatePotentialTransmissionOnBus(transmission: CPTransmission) {
    val isTransmissionOccurringHere = transmission.isOccurringOnBus(id)

    if (!isTransmissionOccurringHere.isFalse) {
      //could be true, or unbound yet
      allSimpleTasksPotentiallyExecutingHere = SimpleTask(
        transmission.start,
        transmission.transmissionDurationNZ2,
        transmission.endNZ,
        isTransmissionOccurringHere) :: allSimpleTasksPotentiallyExecutingHere
    }
  }

  override def transmissionDuration(size: Int): Int = bus.latency + size * bus.timeUnitPerBit

  override def close() {
    if (allSimpleTasksPotentiallyExecutingHere.isEmpty) {
      println("WARNING: no transmission will fit on bus " + bus.name)
    } else {
      SimpleTask.postUnaryResourceFromSimpleTasks(allSimpleTasksPotentiallyExecutingHere)
    }
  }

  def buildTimeWidth:CPIntVar = {
    if(allSimpleTasksPotentiallyExecutingHere.isEmpty) CPIntVar(0)(mapper.store)
    else SimpleTask.resourceWidthOfUse(allSimpleTasksPotentiallyExecutingHere)
  }

}

case class CPSelfLoopBus(override val id: Int, processor: ProcessingElement, mapper: Mapper) extends CPBus(id: Int, mapper: Mapper) {

  override def transmissionDuration(size: Int): Int = 0

  override def accumulatePotentialTransmissionOnBus(transmission: CPTransmission) {}

  override def close() {}

  def selfLoopBus = SelfLoopBus(processor)

  override def buildTimeWidth: CPIntVar = CPIntVar(0)(mapper.store)


}