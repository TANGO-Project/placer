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

import oscar.cp
import oscar.cp._
import oscar.cp.core.variables.CPIntVar
import placerT.algo.Mapper
import placerT.algo.hw.CPProcessor
import placerT.metadata.Formula
import placerT.metadata.sw.{AtomicTask, FlattenedImplementation}

import scala.collection.immutable.SortedMap

case class CPTask(id: Int,
                  task: AtomicTask,
                  explanation: String,
                  mapper: Mapper,
                  maxHorizon: Int)
  extends CPAbstractTask(mapper) {
  implicit val solver = mapper.solver

  val start: CPIntVar = CPIntVar(0, maxHorizon)
  val end: CPIntVar = CPIntVar(0, maxHorizon)

  val processorID: CPIntVar = CPIntVar.sparse(mapper.hardwareModel.processors.indices)
  val implementationID: CPIntVar = CPIntVar.sparse(task.implementationArray.indices)

  val isRunningOnProcessor: Array[CPBoolVar] = mapper.hardwareModel.processors.map(processor => processorID isEq processor.id)
  val isImplementationSelected: Array[CPBoolVar] = task.implementationArray.map(implementation => implementationID isEq implementation.id)

  for (processor <- mapper.hardwareModel.processors) {
    if (!task.canRunOn(processor)) {
      isRunningOnProcessor.apply(processor.id).variable.assignFalse()
    }
  }

  val implemAndProcessorAndDurationAndEnergyAndPower: Iterable[(Int, Int, Int, Int, Int)] =
    task.implementationArray.flatMap(
      implem => mapper.hardwareModel.processors.toList.
        filter(p => p.processorClass equals implem.target).
        map(p => {
          val durationPI = implem.duration(p, mapper.hardwareModel.properties)
          val power = Formula.eval(p.powerModelForTask, mapper.hardwareModel.properties ++ p.processorClass.zeroResources ++ implem.resourceUsage)
          (implem.id, p.id, durationPI, durationPI * power, power)
        }))

  val possibleDurations = implemAndProcessorAndDurationAndEnergyAndPower.map(_._3)
  val duration: CPIntVar = CPIntVar.sparse(possibleDurations)

  val possibleEnergies = implemAndProcessorAndDurationAndEnergyAndPower.map(_._4)
  val energy: CPIntVar = CPIntVar.sparse(possibleEnergies)

  val possiblePowers = implemAndProcessorAndDurationAndEnergyAndPower.map(_._5)
  val power: CPIntVar = CPIntVar.sparse(possiblePowers)

  add(table(implementationID, processorID, duration, energy, power, implemAndProcessorAndDurationAndEnergyAndPower))
  add(end == (start + duration))

  def addIncomingTransmission(cPTransmission: CPTransmission): Unit = {
    incomingCPTransmissions = cPTransmission :: incomingCPTransmissions
  }

  def addOutgoingTransmission(cPTransmission: CPTransmission) {
    outgoingCPTransmissions = cPTransmission :: outgoingCPTransmissions
  }

  var incomingCPTransmissions: List[CPTransmission] = List.empty
  var outgoingCPTransmissions: List[CPTransmission] = List.empty

  val computationMemoryAndImplementation = task.implementationArray.map(i => (i.computationMemory, i.id))
  val computationMemories = computationMemoryAndImplementation.map(_._1)
  val computationMemory = CPIntVar.sparse(computationMemories)
  add(table(computationMemory, implementationID, computationMemoryAndImplementation))

  /**
   * given a target, we want an array, and a map from metric to array. all arays range on implementations
   * the first array contains CPBooVar telling if the implementation is selected
   * The map maps metrics to arrays that contains Int telling the size of the metric the implementation at this indice, from the first array)
   *
   * @param target
   */
  def buildArrayImplemAndMetricUsage(target: CPProcessor): Option[(Array[CPBoolVar], SortedMap[String, Array[Int]])] = {
    val processor = target.p
    val processorClass = processor.processorClass
    task.computingHardwareToImplementations.get(processorClass) match {
      case None => None
      case Some(Nil) => None
      case Some(implementations: List[FlattenedImplementation]) =>
        val implementationSubArray = implementations.toArray
        val isThisImpementationSelectedSubArray = implementationSubArray.map(implementation => isImplementationSelected(implementation.id))

        val dimAndSizePerImplemSubArray: List[(String, Array[Int])] = processorClass.resources.toList.map((dimension: String) =>
          (dimension, implementationSubArray.map(implementation => implementation.resourceUsage(dimension))))

        val dimToSizesPerImplemSubArrays: SortedMap[String, Array[Int]] = SortedMap.empty[String, Array[Int]] ++ dimAndSizePerImplemSubArray

        Some((isThisImpementationSelectedSubArray, dimToSizesPerImplemSubArrays))
    }
  }

  override def variablesToDistribute: Iterable[cp.CPIntVar] = List(start, end, duration, implementationID, processorID)
}
