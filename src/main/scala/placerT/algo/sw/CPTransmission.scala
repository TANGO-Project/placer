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

package placerT.algo.sw

import oscar.cp._
import oscar.cp.core.variables.CPIntVar
import placerT.algo.Mapper
import placerT.algo.hw.CPBus
import placerT.metadata.sw.Transmission
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
                          processorToBusToProcessorAdjacency: Iterable[(Int, Int, Int)])
  extends CPAbstractTask(mapper) {
  implicit val solver = mapper.solver

  val start: CPIntVar = CPIntVar(0, maxHorizon)
  val end: CPIntVar = CPIntVar(0, maxHorizon)

  val busID: CPIntVar = CPIntVar.sparse(busses.indices)
  val isOccurringOnBus: Array[CPBoolVar] = busses.map(bus => busID isEq bus.id)

  val originProcessorID = from.processorID
  val destinationProcessorID = to.processorID

  add(table(originProcessorID, busID, destinationProcessorID, processorToBusToProcessorAdjacency))

  val busAndDuration = busses.toList.map(bus => (bus.id, bus.transmissionDuration(transmission.size)))
  val possibleDurations = busAndDuration.map(_._2)
  val duration: CPIntVar = CPIntVar.sparse(possibleDurations)
  add(table(busID, duration, busAndDuration))

  add(end isEq (start + duration))

  from.addOutgoingTransmission(this)
  to.addIncomingTransmission(this)

  //these are redundant, since the timing is also constrained by the storage task on both side of the transmission
  add(from.end < start)
  add(end < to.start)

  override def variablesToDistribute: Iterable[CPIntVar] = List(start, end, duration, busID)
}