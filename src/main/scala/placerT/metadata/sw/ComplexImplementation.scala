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
 * You should have received a copy of the GNU Lesser General Public License along with placer.
 * If not, see http://www.gnu.org/licenses/lgpl-3.0.en.html
 *
 */

package placerT.metadata.sw

import placerT.metadata.{Formula, Indiced}
import placerT.metadata.hw.ProcessingElementClass

import scala.collection.immutable.SortedMap

case class MultiTreadImplementation(name: String,
                                    threadPrototype: FlattenedImplementation,
                                    numberOfThreads:Int,
                                    dialogs: List[Dialog]) extends Indiced {

}

/**
 * a Dialog is a message exchange between two atomic tasks during their execution.
 * They occur throughout task execution,
 */
case class Dialog(a:Int,b:Int,numberOfMessages:Int,sizeOfMessages:Int){

}


case class FlattenedImplementation(name: String,
                                   target: ProcessingElementClass,
                                   resourceUsage: SortedMap[String, Int],
                                   computationMemory: Int,
                                   duration: Formula,
                                   originImplementation: ParametricImplementation = null,
                                   parameterValues: SortedMap[String, Int] = null) extends Indiced {