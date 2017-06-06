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

package main.scala.placerT.algo

import main.scala.placerT.algo.hw.{CPBus, CPProcessor, CPRegularBus, CPSelfLoopBus}
import main.scala.placerT.algo.sw.{CPTask, CPTransmission}
import main.scala.placerT.metadata.Mapping
import main.scala.placerT.metadata.hw.{Bus, ProcessingElement}
import main.scala.placerT.metadata.sw.FlattenedImplementation
import oscar.cp._
import oscar.cp.core.CPSol


case class CPMappingProblem(hardwareName: String,
                            cpTasks: Array[CPTask],
                            cpProcessors: Array[CPProcessor],
                            cpBusses: Array[CPBus],
                            cpTransmissions: Array[CPTransmission],
                            makeSpan: CPIntVar,
                            energy: CPIntVar) {

  def getMapping(sol: CPSol): Mapping = {

    def proc(procID: CPIntVar): ProcessingElement = cpProcessors(sol(procID)).p
    def implem(task: CPTask): FlattenedImplementation = task.task.implementationArray(sol(task.implementationID))

    val taskMapping = cpTasks.map(cpTask => (cpTask.task, proc(cpTask.processorID), implem(cpTask),
      sol(cpTask.start), sol(cpTask.duration), sol(cpTask.end)))

    def bus(cPTransmission: CPTransmission): Bus = cPTransmission.busses(sol(cPTransmission.busID)) match {
      case c: CPSelfLoopBus => c.selfLoopBus
      case b: CPRegularBus => b.bus
    }

    val transmissionMapping = cpTransmissions.map(trans =>
      (trans.transmission, proc(trans.from.processorID), proc(trans.to.processorID), bus(trans), sol(trans.start), sol(trans.duration), sol(trans.end)))

    new Mapping(hardwareName, taskMapping, transmissionMapping, sol(makeSpan), sol(energy))
  }

  def varsToDistribute: List[CPIntVar] = {
    List.empty ++
      cpTasks.flatMap(_.variablesToDistribute) ++
      cpTransmissions.flatMap(_.variablesToDistribute)
  }

  def varsToSave: List[CPIntVar] = makeSpan :: energy :: varsToDistribute

}
