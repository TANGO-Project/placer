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


package placerT.io

import net.liftweb.json.DefaultFormats
import net.liftweb.json.JsonAST.JValue
import placerT.metadata.hw._
import placerT.metadata.sw.TransmissionTiming._
import placerT.metadata.sw._
import placerT.metadata._

import scala.collection.immutable.{SortedMap, SortedSet}

object Extractor {
  implicit val formats = DefaultFormats

  // Brings in default date formats etc.
  def extractProblem(jValue: JValue): MappingProblem = {
    jValue.extract[EMappingProblem].extract
  }
}

case class EMappingProblem(softwareModel: ESoftwareModel,
                           hardwareModel: EHardwareModel,
                           goal: EGoal) {

  def extract = {
    val hw = hardwareModel.extract
    val sw = softwareModel.extract(hw)

     MappingProblem(sw, hw, goal.extract)
    }
}

case class EGoal(simpleObjective:Option[String],multiObjective:Option[EPareto]){
  def extract:MappingGoal = {
    (simpleObjective,multiObjective) match{
      case (None,None) => throw new Error("no mapping goal defined")
      case (Some(s),None) => EGoal.extractSimple(s)
      case (None,Some(p)) => p.extract
      case _ => throw new Error("mapping goal defined twice")
    }

  }
}

object EGoal{
  def extractSimple(name:String):SimpleMappingGoal = {
    name match{
      case "minEnergy" => MinEnergy()
      case "minMakeSpan" => MinMakeSpan()
      case "minFrame" => MinFrame()
      case _ => throw new Error("unknown simple mapping goal:" + name)
    }
  }
}

case class EPareto(a:String,b:String){
  def extract:Pareto = Pareto(EGoal.extractSimple(a), EGoal.extractSimple(b))
}

case class ESoftwareModel(simpleProcesses: Array[EAtomicTask],
                          transmissions: Array[ETransmission],
                          softwareClass: ESoftwareClass) {
  def extract(hw: HardwareModel) = {
    val proc = simpleProcesses.map(_.extract(hw))
    SoftwareModel(
      proc,
      transmissions.map(_.extract(proc)),
      softwareClass.extract)
  }
}

case class ESoftwareClass(oneShotSoftware: Option[EOneShotSoftware],
                          iterativeSoftware:Option[EITerativeSoftware]) {
  def extract = (oneShotSoftware,iterativeSoftware) match {
    case (Some(s),None) => s.extract
    case (None,Some(i)) => i.extract
    case (None,None) => throw new Error("software class not defined")
    case _ =>  throw new Error("software class defined twice")
  }
}

case class EOneShotSoftware(maxDelay: Option[Int]) {
  def extract = OneShotSoftware(maxDelay)
}

case class EITerativeSoftware(maxMakespan:Option[Int],maxFrameDelay:Option[Int]){
  def extract = IterativeSoftware(maxMakespan,maxFrameDelay)
}

case class EAtomicTask(name: String,
                       implementations: List[EParametricImplementation]) {
  def extract(hw: HardwareModel) = {
    AtomicTask(implementations.map(_.extract(hw)),
      name)
  }
}

case class ENameValue(name: String, value: Int) {
  def toCouple: (String, Int) = (name, value)
}

case class EParametricImplementation(name: String,
                                     target: String,
                                     resourceUsage: List[ENameFormula],
                                     computationMemory: String,
                                     duration: String,
                                     parameters: List[ENameValues]) {
  val parsedComputationMemory = FormulaParser(computationMemory)
  val parsedDuration = FormulaParser(duration)

  def extract(hw: HardwareModel) =
    ParametricImplementation(name,
      hw.processorClasses.find(_.name equals target).get,
      SortedMap.empty[String, Formula] ++ resourceUsage.map(_.extract),
      parsedComputationMemory,
      parsedDuration,
      SortedMap.empty[String, Iterable[Int]] ++ parameters.map(_.extract))
}

case class ENameValues(name: String, values: List[Int]) {
  def extract = (name, values)
}

case class ENameFormula(name: String, formula: String) {
  val parsedFormula = FormulaParser(formula)

  def extract: (String, Formula) = (name, parsedFormula)
}

case class ETransmission(source: String,
                         target: String,
                         size: Int,
                         timing: String,
                         name: String) {
  def extract(a: Array[AtomicTask]) =
    Transmission(
      a.find(t => t.name equals source).get,
      a.find(t => t.name equals target).get,
      size,
      timing match {
        case "Asap" => Asap
        case "Alap" => Alap
        case "Free" => Free
      },
      name)
}

case class EProcessingElementClass(multiTaskPermanentTasks: Option[EMultiTaskPermanentTasks], monoTaskSwitchingTask: Option[EMonoTaskSwitchingTask]) {
  def extract: ProcessingElementClass = {
    (multiTaskPermanentTasks, monoTaskSwitchingTask) match {
      case (None, None) => throw new Error("Processing element not specified")
      case (Some(a), None) => a.extract
      case (None, Some(a)) => a.extract
      case (Some(_), Some(_)) => throw new Error("Processing element specified twice")
    }
  }
}

case class EMultiTaskPermanentTasks(name: String, resources: List[String], properties: List[String]) {
  def extract: MultiTaskPermanentTasks = MultiTaskPermanentTasks(name, SortedSet.empty[String] ++ resources, SortedSet.empty[String] ++ properties)
}

case class EMonoTaskSwitchingTask(name: String, resources: List[String], properties: List[String], switchingDelay: Int) {
  def extract: MonoTaskSwitchingTask = MonoTaskSwitchingTask(name, SortedSet.empty[String] ++ resources, SortedSet.empty[String] ++ properties, switchingDelay)
}

case class EProcessingElement(processorClass: String,
                              resources: List[ENameValue],
                              properties: List[ENameValue],
                              name: String,
                              memSize: Int,
                              powerModel: String) {
  val parsedPowerModel = FormulaParser(powerModel)

  def extract(pc: Array[ProcessingElementClass]): ProcessingElement = ProcessingElement(
    pc.find(_.name equals processorClass).get,
    SortedMap.empty[String, Int] ++ resources.map(_.toCouple),
    SortedMap.empty[String, Int] ++ properties.map(_.toCouple),
    name,
    memSize,
    parsedPowerModel
  )
}

case class EBus(halfDuplexBus: Option[EHalfDuplexBus], singleWayBus: Option[ESingleWayBus]) {
  def extract(p: Array[ProcessingElement]): Bus = {
    (halfDuplexBus, singleWayBus) match {
      case (None, None) => throw new Error("Bus element not specified")
      case (Some(a), None) => a.extract(p)
      case (None, Some(a)) => a.extract(p)
      case (Some(_), Some(_)) => throw new Error("Bus element specified twice")
    }
  }
}

case class EHalfDuplexBus(relatedProcessors: List[String],
                          timeUnitPerBit: Int,
                          latency: Int,
                          name: String) {
  def extract(p: Array[ProcessingElement]) = HalfDuplexBus(
    relatedProcessors.map(name => p.find(_.name equals name).get),
    timeUnitPerBit,
    latency,
    name)
}

case class ESingleWayBus(from: List[String],
                         to: List[String],
                         timeUnitPerBit: Int,
                         latency: Int,
                         name: String) {
  def extract(p: Array[ProcessingElement]) = SingleWayBus(
    from.map(name => p.find(_.name equals name).get),
    to.map(name => p.find(_.name equals name).get),
    timeUnitPerBit,
    latency,
    name)
}

case class EHardwareModel(name: String,
                          classes: Array[EProcessingElementClass],
                          processors: Array[EProcessingElement],
                          busses: Array[EBus],
                          properties: List[ENameValue],
                          powerCap: Option[Int],
                          energyCap: Option[Int]) {
  def extract = {
    val pc = classes.map(_.extract)
    val p = processors.map(_.extract(pc))
    val b = busses.map(_.extract(p))
    HardwareModel(
      name,
      pc,
      p,
      b,
      SortedMap.empty[String, Int] ++ properties.map(_.toCouple),
      powerCap,
      energyCap)
  }
}