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
  def extractProblem(jValue:JValue,fileName:String,verbose:Boolean = true): MappingProblem = {
    jValue.extract[EMappingProblem].extract(verbose,fileName)
  }
}

case class EMappingProblem(timeUnit:String,
                           dataUnit:String,
                           info:Option[String],
                           processingElementClasses:Array[EProcessingElementClass],
                           softwareModel: ESoftwareModel,
                           hardwareModel: EHardwareModel,
                           constraints:List[EMappingConstraint],
                           properties: List[ENameValue] = List.empty,
                           goal:EGoal) {

  require(processingElementClasses.nonEmpty,"no procesing element class specified in input file")
  def extract(verbose:Boolean,fileName:String) = {
    val cl = processingElementClasses.map(_.extract)
    Checker.checkDuplicates(cl.map(_.name),"processing element class")

    val hw = hardwareModel.extract(cl)
    val sw = softwareModel.extract(hw,verbose)

    MappingProblem(
      timeUnit,
      dataUnit,
      info match{case None => fileName; case Some(i) => i},
      SortedMap.empty[String, Int] ++ properties.map(_.toCouple),
      cl,
      sw,
      hw,
      constraints.map(_.extract(hw,sw)),
      goal.extract)
  }
}

case class EMappingConstraint(runOn:Option[ERunOn],
                              notRunOn:Option[ERunOn],
                              samePE:Option[List[String]],
                              notSamePE:Option[List[String]],
                              mustBeUsed:Option[String],
                              mustNotBeUsed:Option[String],
                              symmetricPE:Option[List[String]]){
  def extract(hw:HardwareModel,sw:SoftwareModel):MappingConstraint = {

    def extractRunOn(c:ERunOn,value:Boolean):MappingConstraint = {
      val processor = hw.processors.find(p => p.name equals c.processingElement) match{
        case Some(x) => x
        case None => throw new Error("cannot find processor" + c.processingElement + " used in mappingConstraint " + c)}

      val process = sw.simpleProcesses.find(p => p.name equals c.task) match{
        case Some(x) => x
        case None => throw new Error("cannot find process " + c.task + " used in mappingConstraint " + c)}

      RunOnConstraint(processor,process,value)
    }

    def extractSameCore(c:List[String],value:Boolean):MappingConstraint = {
      val processes = c.map(process => sw.simpleProcesses.find(p => p.name equals process) match{
        case Some(x) => x
        case None => throw new Error("cannot find process " + process + " used in mappingConstraint " + c)})

      CoreSharingConstraint(processes,value)
    }

    def extractMustBeUsed(processorName:String,value:Boolean):MustBeUsedConstraint = {
      val processor = hw.processors.find(p => p.name equals processorName) match{
        case Some(x) => x
        case None => throw new Error("cannot find processor" + processorName + " used in mappingConstraint")}
      MustBeUsedConstraint(processor,value)
    }

    def extractPESymmetry(symmetricPENames:List[String]):SymmetricPEConstraint = {
      val processors = symmetricPENames.map(processorName => hw.processors.find(p => p.name equals processorName) match{
        case Some(x) => x
        case None => throw new Error("cannot find processor" + processorName + " used in mappingConstraint")})

      SymmetricPEConstraint(processors)
    }

    (runOn,notRunOn,samePE,notSamePE,mustBeUsed,mustNotBeUsed,symmetricPE) match {
      case (Some(s),None,None,None,None,None,None) =>
        extractRunOn(s,true)
      case (None,Some(s),None,None,None,None,None) =>
        extractRunOn(s,false)
      case (None,None,Some(s),None,None,None,None) =>
        extractSameCore(s,true)
      case (None,None,None,Some(s),None,None,None) =>
        extractSameCore(s,false)
      case (None,None,None,None,Some(s),None,None) =>
        extractMustBeUsed(s,true)
      case (None,None,None,None,None,Some(s),None) =>
        extractMustBeUsed(s,false)
      case (None,None,None,None,None,None,Some(s)) =>
        extractPESymmetry(s)

      case (_) => throw new Error("erroneous mapping constraint (multiple def or empty def): " + this)
    }
  }
}

case class ERunOn(task:String,processingElement:String)

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
  def extractSimple(name:String):MappingGoal = {
    name match{
      case "minEnergy" => MinEnergy()
      case "minMakespan" => MinMakeSpan()
      case "minFrame" => MinFrame()
      case "sat" => Sat()
      case _ => throw new Error("unknown simple mapping goal:" + name)
    }
  }

  def extractSimpleExpectSimple(name:String):SimpleMappingGoal = {
    extractSimple(name:String) match{
      case s:SimpleMappingGoal => s
      case x => throw new Error("expected simple mappinggoal, got " + x)
    }
  }
}

case class EPareto(a:String,b:String){
  def extract:Pareto = Pareto(EGoal.extractSimpleExpectSimple(a), EGoal.extractSimpleExpectSimple(b))
}

case class ESoftwareModel(simpleProcesses: Array[EAtomicTask],
                          transmissions: Array[ETransmission],
                          properties:List[ENameValue] = List.empty,
                          softwareClass: ESoftwareClass) {
  def extract(hw: HardwareModel,verbose:Boolean) = {

    Checker.checkDuplicates(simpleProcesses.map(_.name),"task")
    Checker.checkDuplicates(transmissions.map(_.name),"transmission")

    val proc = simpleProcesses.map(task => task.extract(hw))
    SoftwareModel(
      proc,
      transmissions.map(_.extract(proc)),
      softwareClass.extract,
      SortedMap.empty[String,Int] ++ properties.map(_.toCouple),
      verbose)
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

case class EOneShotSoftware(maxMakespan: Option[Int]) {
  def extract = OneShotSoftware(maxMakespan)
}

case class EITerativeSoftware(maxMakespan:Option[Int],maxFrameDelay:Option[Int]){
  def extract = IterativeSoftware(maxMakespan,maxFrameDelay)
}

case class EAtomicTask(name: String,
                       implementations: List[EParametricImplementation]) {
  Checker.checkDuplicates(implementations.map(_.name),"implementation of task " + name)

  def extract(hw: HardwareModel) = {
    val translatedImplems = implementations.map(_.extract(hw))
    AtomicTask(translatedImplems, name)
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

  val parsedResources = SortedMap.empty[String, Formula] ++ resourceUsage.map(_.extract)

  def extract(hw: HardwareModel) = {
    val targetClass = hw.processorClasses.find(_.name equals target) match {
      case Some(x) => x;
      case None => throw new Error("cannot find processor class " + target + " used in implementation " + name)
    }
    require(targetClass.resources equals parsedResources.keySet,"error in declared resources of " + name)
    ParametricImplementation(
      name,
      targetClass,
      parsedResources,
      parsedComputationMemory,
      parsedDuration,
      SortedMap.empty[String, Iterable[Int]] ++ parameters.map(_.extract))
  }
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
      a.find(t => t.name equals source) match{case Some(x) => x ; case None => throw new Error("cannot find process" + source + " used in transmission " + name)},
      a.find(t => t.name equals target) match{case Some(x) => x ; case None => throw new Error("cannot find process " + target + " used in transmission " + name)},
      size,
      timing match {
        case "Asap" => Asap
        case "Alap" => Alap
        case "Free" => Free
        case "Sticky" => Sticky
        case _ => throw new Error("undefined timing constraint " + timing + " in transmission " + name)
      },
      name)
}

case class EProcessingElementClass(multiTaskPermanentTasks: Option[EMultiTaskPermanentTasks], monoTaskSwitchingTask: Option[EMonoTaskSwitchingTask]) {
  def extract: ProcessingElementClass = {
    (multiTaskPermanentTasks, monoTaskSwitchingTask) match {
      case (None, None) => throw new Error("empty definition of processing element class")
      case (Some(a), None) => a.extract
      case (None, Some(a)) => a.extract
      case (Some(m), Some(_)) => throw new Error("Double definition of processing element class:" + m.name)
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
    pc.find(_.name equals processorClass)match{
      case None => throw new Error("cannot find processing element class " + processorClass + " used in processing element: " + name)
      case Some(x) => x
    },
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
      case (None, None) => throw new Error("Empty bus definition")
      case (Some(a), None) => a.extract(p)
      case (None, Some(a)) => a.extract(p)
      case (Some(h), Some(_)) => throw new Error("Double definition of bus:" + h.name)
    }
  }
}

case class EHalfDuplexBus(relatedProcessors: List[String],
                          timeUnitPerDataUnit: Int,
                          latency: Int,
                          name: String) {
  def extract(p: Array[ProcessingElement]) = HalfDuplexBus(
    relatedProcessors.map(name => p.find(_.name equals name)
    match{
      case Some(x) => x ;
      case None => throw new Error("cannot find processing element " + name + " used in bus " + this.name + " existing PE:" + p.map(_.name).toList)}),
    timeUnitPerDataUnit,
    latency,
    name)
}

case class ESingleWayBus(from: List[String],
                         to: List[String],
                         timeUnitPerDataUnit: Int,
                         latency: Int,
                         name: String) {
  def extract(p: Array[ProcessingElement]) = SingleWayBus(
    from.map(name => p.find(_.name equals name) match{case Some(x) => x ; case None => throw new Error("cannot find processing element " + name + " used in bus " + this.name)}),
    to.map(name => p.find(_.name equals name) match{case Some(x) => x ; case None => throw new Error("cannot find processing element " + name + " used in bus " + this.name)}),
    timeUnitPerDataUnit,
    latency,
    name)
}

case class EHardwareModel(name: String,
                          processingElements: Array[EProcessingElement],
                          busses: Array[EBus],
                          properties: List[ENameValue],
                          powerCap: Option[Int],
                          energyCap: Option[Int]) {
  def extract(pc:Array[ProcessingElementClass]) = {
    val p = processingElements.map(_.extract(pc))
    Checker.checkDuplicates(p.map(_.name),"processing element")
    val b = busses.map(_.extract(p))
    Checker.checkDuplicates(b.map(_.name),"bus")
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

object Checker{
  def checkDuplicates(strings:Iterable[String],conceptClass:String = "definition"): Unit ={
    var stringSet = SortedSet.empty[String]
    for(s <- strings){
      if(stringSet contains s) throw new Error("duplicate " + conceptClass + ":" + s)
      stringSet += s
    }
  }
}