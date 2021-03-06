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

import oscar.cp.CPStore
import placer.metadata.sw.FlattenedImplementationConcrete

import scala.collection.immutable.SortedSet

case class CPHardwareModel(cpProcessors:Array[CPProcessor],
                           cpBusses: Array[CPBus],
                           processorToBusToProcessorAdjacency: Set[(Int, Int, Int)],
                           selfLoopBussesID:SortedSet[Int],
                           store:CPStore,
                           maxHorizon:Int,
                           sharedImplementationIDToFlattenedAndVirtualCores:Array[List[(FlattenedImplementationConcrete,CPInstantiatedPermanentFunction)]]) {

}
