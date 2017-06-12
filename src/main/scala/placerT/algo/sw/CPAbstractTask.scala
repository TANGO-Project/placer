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
 * You should have received a copy of the GNU Lesser General Public License along with Placer.
 * If not, see http://www.gnu.org/licenses/lgpl-3.0.en.html
 *
 */


package placerT.algo.sw

import oscar.cp.core.variables.CPIntVar
import placerT.algo.Mapper
import placerT.algo.hw.CPMultiTaskProcessor

import scala.collection.immutable.SortedMap

abstract class CPAbstractTask(mapper: Mapper) {
  def start: CPIntVar

  def end: CPIntVar

  def duration: CPIntVar

  def variablesToDistribute: Iterable[CPIntVar]

  def buildArrayImplemAndMetricUsage(target: CPMultiTaskProcessor): Option[(Array[CPIntVar], SortedMap[String, Array[Int]])]

  def couldBeExecutingOnProcessor(procID:Int):Boolean
}