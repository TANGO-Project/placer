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

import main.scala.placerT.algo.sw.CPTransmission
import main.scala.placerT.algo.{Mapper, SimpleTask}
import main.scala.placerT.metadata.hw.{Bus, ProcessingElement, SelfLoopBus}



abstract class CPBus(val id: Int, mapper: Mapper) {

  def accumulatePotentialTransmissionOnBus(transmission: CPTransmission)

  def transmissionDuration(size: Int): Int

  def close()
}

case class CPRegularBus(override val id: Int, bus: Bus, mapper: Mapper) extends CPBus(id: Int, mapper: Mapper) {

  private var allSimpleTasksPotentiallyExecutingHere: List[SimpleTask] = List.empty

  override def accumulatePotentialTransmissionOnBus(transmission: CPTransmission) {
    val isTransmissionOccurringHere = transmission.isOccurringOnBus(id)

    if (!isTransmissionOccurringHere.isFalse) {
      //could be true, or unbound yet
      allSimpleTasksPotentiallyExecutingHere = SimpleTask(
        transmission.start,
        transmission.duration,
        transmission.end,
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
}

case class CPSelfLoopBus(override val id: Int, processor: ProcessingElement, mapper: Mapper) extends CPBus(id: Int, mapper: Mapper) {

  override def transmissionDuration(size: Int): Int = 0

  override def accumulatePotentialTransmissionOnBus(transmission: CPTransmission) {}

  override def close() {}

  def selfLoopBus = SelfLoopBus(processor)
}