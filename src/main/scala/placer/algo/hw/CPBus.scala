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
import oscar.cp.modeling.Constraints
import placer.algo.sw.CPTransmission
import placer.algo.{Mapper, SimpleTask}
import placer.metadata.hw.{Bus, ProcessingElement, SelfLoopBus}

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

  val isSelfLoop:Boolean
  val name:String
}

case class CPRegularBus(override val id: Int, bus: Bus, mapper: Mapper) extends CPBus(id: Int, mapper: Mapper) with Constraints {

  val isSelfLoop:Boolean = false
  val name= bus.name

  require(bus.timeUnitPerDataUnit>=0)
  private var allSimpleTasksPotentiallyExecutingHere: List[SimpleTask] = List.empty

  override def accumulatePotentialTransmissionOnBus(transmission: CPTransmission) {
    val isTransmissionOccurringHere = transmission.isOccurringOnBus(id)

    if (!isTransmissionOccurringHere.isFalse) {
      //could be true, or unbound yet
      allSimpleTasksPotentiallyExecutingHere = SimpleTask(
        transmission.start,
        transmission.transmissionDuration,
        transmission.end,
        isTransmissionOccurringHere) :: allSimpleTasksPotentiallyExecutingHere
    }
  }

  override def transmissionDuration(size: Int): Int = bus.latency + size * bus.timeUnitPerDataUnit

  override def close() {
    if (allSimpleTasksPotentiallyExecutingHere.isEmpty) {
      println("WARNING: no transmission will fit on bus " + bus.name)
    } else {
      SimpleTask.postUnaryResourceForSimpleTasks(allSimpleTasksPotentiallyExecutingHere,origin="bus " + bus.name)
    }
  }

  def buildTimeWidth:CPIntVar = {
    if(allSimpleTasksPotentiallyExecutingHere.isEmpty) CPIntVar(0)(mapper.store)
    else SimpleTask.resourceWidthOfUse(allSimpleTasksPotentiallyExecutingHere)
  }

  def busOccupancy:CPIntVar = {
    sum(allSimpleTasksPotentiallyExecutingHere.map(task => mul(task.duration,task.isNeeded)))
  }
}

case class CPSelfLoopBus(override val id: Int, processorFrom: ProcessingElement, processorTo: ProcessingElement, mapper: Mapper) extends CPBus(id: Int, mapper: Mapper) {

  val isSelfLoop:Boolean = true

  val name = "SelfLoopBus on " + processorFrom.name + (if (processorFrom == processorTo) "" else (" and " + processorTo.name))

  override def transmissionDuration(size: Int): Int = 0

  override def accumulatePotentialTransmissionOnBus(transmission: CPTransmission) {}

  override def close() {}

  def selfLoopBus = SelfLoopBus(processorFrom,processorTo)

  override def buildTimeWidth: CPIntVar = CPIntVar(0)(mapper.store)
}