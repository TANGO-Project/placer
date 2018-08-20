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


package placer.algo.sw

import oscar.cp
import oscar.cp._
import oscar.cp.core.variables.CPIntVar
import placer.algo.Mapper
import placer.algo.hw.{CPHardwareModel, CPInstantiatedPermanentFunction, CPPermanentTaskProcessor}
import placer.metadata.sw._
import placer.metadata.{Formula, IndiceMaker}

import scala.collection.immutable.SortedMap

case class CPTask(id: Int,
                  task: AtomicTask,
                  explanation: String,
                  cpHardwareModel:CPHardwareModel,
                  mapper:Mapper)
  extends CPAbstractTask() with IndiceMaker {

  val flattenedReferenceToImplementations = task.sharedImplementations.flatMap(sharedImplem => {
    val implemAndVCoreList:List[(FlattenedImplementationConcrete,CPInstantiatedPermanentFunction)] = cpHardwareModel.sharedImplementationIDToFlattenedAndVirtualCores(sharedImplem.p.id)
    implemAndVCoreList.map({ case (flattenedImplem, vCore) =>
      ReferenceToSharedFlattenedImplementationConcrete(flattenedImplem, vCore)
    })})

  val allImplementationArray:Array[FlattenedImplementation] = (task.implementations.flatMap(_.implementations) ++ flattenedReferenceToImplementations).toArray
  setIndices(allImplementationArray)

  implicit val store:CPStore = cpHardwareModel.store
  val maxHorizon = cpHardwareModel.maxHorizon
  val cpProcessors = cpHardwareModel.cpProcessors

  def occuringOnProcDebugInfo = task.name + " running on proc:[" +  isRunningOnProcessor.mkString(",") + "]"

  val start: CPIntVar = CPIntVar(0, maxHorizon)
  val end: CPIntVar = CPIntVar(0, maxHorizon)

  val processorID: CPIntVar = CPIntVar.sparse(cpProcessors.indices)
  val implementationID: CPIntVar = CPIntVar.sparse(allImplementationArray.indices)

  val isRunningOnProcessor: Array[CPBoolVar] = cpProcessors.map(processor => processorID isEq processor.id)
  val isImplementationSelected: Array[CPBoolVar] = allImplementationArray.map(implementation => implementationID isEq implementation.id)

  //TODO: this is probably useless sgiven the table constraint here below.
  /*
  for (processor <- cpProcessors) {
    for(i <- allImplementationArray){
      if(!i.canRunOn(processor.p)){
        isRunningOnProcessor.apply(processor.id).variable.assignFalse()
      }
    }
  }
*/

  var indice = 0
  object ImplemAndProcessorAndDurationAndEnergyAndPower{
    def apply(implem:Int,processor:Int,duration:Int,energy:Int,power:Int,nbThreads:Int):ImplemAndProcessorAndDurationAndEnergyAndPower = {
      val curentIndice = indice
      indice = indice+1
      ImplemAndProcessorAndDurationAndEnergyAndPower(implem:Int,processor:Int,duration:Int,energy:Int,power:Int,curentIndice,nbThreads)
    }
  }

  case class ImplemAndProcessorAndDurationAndEnergyAndPower(implem:Int,
                                                            processor:Int,
                                                            duration:Int,
                                                            energy:Int,
                                                            power:Int,
                                                            indice:Int,
                                                            nbThreads:Int){
    if(duration==0)
      System.err.println("WARNING: duration==0 for task " + task.name + " with implementation " + allImplementationArray(implem).name + " running on " + cpProcessors(processor).p.name)
  }

  val implemAndProcessorAndDurationAndEnergyAndPower: Iterable[ImplemAndProcessorAndDurationAndEnergyAndPower] =
    allImplementationArray.flatMap((implem:FlattenedImplementation) => {

      implem match {
        case sharedImplem: ReferenceToSharedFlattenedImplementationConcrete =>
          val targetedProcessors = cpProcessors.toList.filter(p => sharedImplem.target == p)

          targetedProcessors.map(p => {
            val durationPI = sharedImplem.f.duration(p.p, mapper.problem.properties ++ mapper.hardwareModel.properties ++ mapper.softwareModel.properties)
            require(durationPI >= 0, "duration of implementation " + sharedImplem.name + " on processor " + p.p.name + " is negative:" + durationPI)
            val power = Formula.eval(
              p.p.powerModelForTask,
              mapper.problem.properties ++
                mapper.hardwareModel.properties ++
                mapper.softwareModel.properties ++
                p.p.processorClass.zeroResources ++
                sharedImplem.f.resourceUsage)

            require(power >= 0, "power of implementation " + sharedImplem.name + " on processor " + p.p.name + " is negative:" + power)
            val energy = durationPI * power
            require(energy >= 0, "energy usage of implementation " + sharedImplem.name + " on processor " + p.p.name + " is negative:" + energy)

            ImplemAndProcessorAndDurationAndEnergyAndPower(
              implem = sharedImplem.id,
              processor = p.id,
              duration = durationPI,
              energy = energy,
              power = power,
              nbThreads = 1 //since they run on FPGA anyway
            )
          })

        case concreteImplem: FlattenedImplementationConcrete =>
          val targetedProcessors = cpProcessors.toList.filter(p => concreteImplem.canRunOn(p.p))

          targetedProcessors.map(p => {
            val durationPI = concreteImplem.duration(p.p, mapper.problem.properties ++ mapper.hardwareModel.properties ++ mapper.softwareModel.properties)
            require(durationPI >= 0, "duration of implementation " + concreteImplem.name + " on processor " + p.p.name + " is negative:" + durationPI)
            val power = Formula.eval(p.p.powerModelForTask,
              mapper.problem.properties ++
                mapper.hardwareModel.properties ++
                mapper.softwareModel.properties ++
                p.p.processorClass.zeroResources ++
                concreteImplem.resourceUsage)

            require(power >= 0, "power of implementation " + concreteImplem.name + " on processor " + p.p.name + " is negative:" + power)
            val energy = durationPI * power
            require(energy >= 0, "energy usage of implementation " + concreteImplem.name + " on processor " + p.p.name + " is negative:" + energy)

            ImplemAndProcessorAndDurationAndEnergyAndPower(
              implem = implem.id,
              processor = p.id,
              duration = durationPI,
              energy = energy,
              power = power,
              nbThreads = concreteImplem.nbThreads
            )
          })
      }}
    )

  def couplesToArray(a:Iterable[(Int,Int)]):Array[Int] = {
    val toReturn = Array.fill(a.size)(Int.MinValue)
    for((indice,value) <- a){
      toReturn(indice) = value
    }
    toReturn
  }

  private val processorImplementationCombo = CPIntVar(0,indice-1)

  //duration
  private val possibleDurations = implemAndProcessorAndDurationAndEnergyAndPower.map(_.duration)
  val minDuration = possibleDurations.min
  val maxDuration = possibleDurations.max

  private val indiceToDuration = couplesToArray(implemAndProcessorAndDurationAndEnergyAndPower.map(x => (x.indice,x.duration)))
  val taskDuration: CPIntVar = CPIntVar(minDuration,maxDuration)

  store.add(element(indiceToDuration,processorImplementationCombo,taskDuration))

  store.add(end === (start + taskDuration))

  //energy
  private val possibleEnergies = implemAndProcessorAndDurationAndEnergyAndPower.map(_.energy)
  val minEnergy = possibleEnergies.min
  val maxEnergy = possibleEnergies.max

  private val indiceToEnergy = couplesToArray(implemAndProcessorAndDurationAndEnergyAndPower.map(x => (x.indice,x.energy)))
  val energy: CPIntVar = CPIntVar(minEnergy,maxEnergy)

  store.add(element(indiceToEnergy,processorImplementationCombo,energy))

  //power
  val possiblePower = implemAndProcessorAndDurationAndEnergyAndPower.map(_.power)
  val minPower = possiblePower.min
  val maxPower = possiblePower.max

  private val indiceToPower = couplesToArray(implemAndProcessorAndDurationAndEnergyAndPower.map(x => (x.indice,x.power)))
  val power: CPIntVar = CPIntVar(minPower,maxPower)

  store.add(element(indiceToPower,processorImplementationCombo,power))


  //nbThreads
  val possibleNbTreads = implemAndProcessorAndDurationAndEnergyAndPower.map(_.nbThreads)
  val minThread = possibleNbTreads.min
  val maxThread = possibleNbTreads.max
  private val indiceToThreads = couplesToArray(implemAndProcessorAndDurationAndEnergyAndPower.map(x => (x.indice,x.nbThreads)))
  val nbThreads: CPIntVar = CPIntVar(minThread,maxThread)

  store.add(element(indiceToThreads,processorImplementationCombo,nbThreads))

  //all other stuff, with a table constraint
  private val implemAndProcessorAndIndice = implemAndProcessorAndDurationAndEnergyAndPower.map(x => (x.implem,x.processor,x.indice))

  store.add(table(implementationID, processorID, processorImplementationCombo,implemAndProcessorAndIndice))

  private val possibleProcessorAndDuration = implemAndProcessorAndDurationAndEnergyAndPower.map(x => (x.processor,x.duration))
  private val possibleProcessorAndWorkload = implemAndProcessorAndDurationAndEnergyAndPower.map(x => (x.processor,x.duration*x.nbThreads))

  private val possibleProcessorToMinDuration = possibleProcessorAndDuration.groupBy(_._1).mapValues((possibles:Iterable[(Int,Int)]) => possibles.map(_._2).min)
  private val possibleProcessorToMinWorkload = possibleProcessorAndWorkload.groupBy(_._1).mapValues((possibles:Iterable[(Int,Int)]) => possibles.map(_._2).min)

  def minTaskDurationOnProcessor(processorID:Int):Option[Int] = {
    possibleProcessorToMinDuration.get(processorID)
  }

  def minWorkloadOnProcessor(processorID:Int):Option[Int] = {
    possibleProcessorToMinWorkload.get(processorID)
  }


  def addIncomingTransmission(cPTransmission: CPTransmission): Unit = {
    if(cPTransmission.timing == TransmissionTiming.Alap){
      val otherAsap = incomingCPTransmissions.filter(_.timing == TransmissionTiming.Alap)
      if(otherAsap.nonEmpty) {
        System.err.println("Several Alap transmissions incoming a task can cause no solution; task: " + this.task.name + ":" + (cPTransmission :: otherAsap).map(_.transmission.name))
      }
    }
    incomingCPTransmissions = cPTransmission :: incomingCPTransmissions
  }

  def addOutgoingTransmission(cPTransmission: CPTransmission) {
    if(cPTransmission.timing == TransmissionTiming.Asap){
      val otherAsap = outgoingCPTransmissions.filter(_.timing == TransmissionTiming.Asap)
      if(otherAsap.nonEmpty) {
        System.err.println("Several Asap transmissions outgoing a task can cause no solution; task: " + this.task.name + ":" + (cPTransmission :: otherAsap).map(_.transmission.name))
      }
    }
    outgoingCPTransmissions = cPTransmission :: outgoingCPTransmissions
  }

  var incomingCPTransmissions: List[CPTransmission] = List.empty
  var outgoingCPTransmissions: List[CPTransmission] = List.empty

  val computationMemoryAndImplementation = allImplementationArray.map(i => (i.computationMemory, i.id))
  val computationMemories = computationMemoryAndImplementation.map(_._1)
  val computationMemory = CPIntVar.sparse(computationMemories)
  store.add(table(computationMemory, implementationID, computationMemoryAndImplementation))

  /**
    * given a target, we want an array, and a map from metric to array. all arrays range on implementations
    * the first array contains CPBooVar telling if the implementation is selected
    * The map maps metrics to arrays that contains Int telling the size of the metric the implementation at this indice, from the first array)
    *
    * @param target
    */
  def buildArrayImplemAndMetricUsage(target: CPPermanentTaskProcessor): Option[(Array[CPBoolVar], SortedMap[String, Array[Int]])] = {
    val processor = target.p
    val processorClass = processor.processorClass
    val isThisProcessorSelected:CPBoolVar = isRunningOnProcessor(target.id)

    val selectedImplementations:List[FlattenedImplementationConcrete] = allImplementationArray.toList.flatMap(p =>
      p match{
        case f:FlattenedImplementationConcrete if f.canRunOn(target.p) => Some(f)
        case _ => None
      })

    val implementationSubArray = selectedImplementations.toArray
    val isThisImplementationSelectedSubArray:Array[CPBoolVar] = implementationSubArray.map(
      implementation => isThisProcessorSelected && isImplementationSelected(implementation.id))

    val dimAndSizePerImplemSubArray: List[(String, Array[Int])] = processorClass.resources.toList.map((dimension: String) =>
      (dimension, implementationSubArray.map(implementation => implementation.resourceUsage(dimension))))

    val dimToSizesPerImplemSubArrays: SortedMap[String, Array[Int]] = SortedMap.empty[String, Array[Int]] ++ dimAndSizePerImplemSubArray

    Some((isThisImplementationSelectedSubArray, dimToSizesPerImplemSubArrays))
  }

  override def variablesToDistribute: Iterable[cp.CPIntVar] = List(start, /*end, taskDuration,*/ implementationID /*, processorID*/)
  def variablesToSave: Iterable[cp.CPIntVar] = List(start, end, taskDuration, implementationID, processorID)
}
