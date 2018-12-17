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

package placer.io

import net.liftweb.json.DefaultFormats
import net.liftweb.json.JsonAST.JValue
import placer.metadata
import placer.metadata._
import placer.metadata.hw.{ProcessingElementClass, _}
import placer.metadata.sw.SoftwareClass.SoftwareClass
import placer.metadata.sw.TransmissionTiming._
import placer.metadata.sw.{SoftwareClass, _}

import scala.collection.immutable.{SortedMap, SortedSet}

object Extractor {
  implicit val formats = DefaultFormats
  def extractProblem(jValue:JValue,verbose:Boolean = true): MappingProblem = {
    jValue.extract[EMappingProblemHeaderOnly] //just to check the header before performing the extract
    jValue.extract[EMappingProblem].extract(verbose)
  }
}

case class EMappingProblemHeaderOnly(jsonFormat:String){
  val currentFormat = "PlacerBeta5"
  require(jsonFormat equals currentFormat,"expected " + currentFormat + " format fo input JSon, got " + jsonFormat)
}

case class EMappingProblem(timeUnit:String,
                           dataUnit:String,
                           energyUnit:Option[String],
                           info:Option[String],
                           jsonFormat:String,
                           processingElementClasses:Array[EProcessingElementClass],
                           softwareModel:ESoftwareModel,
                           hardwareModel:Option[EHardwareModel],
                           hardwareModels:List[EHardwareModel],
                           properties:List[ENameValue],
                           constraintsAndObjectives:List[EMappingConstraint]) extends IndiceMaker {

  require(processingElementClasses.nonEmpty,"no processing element class specified in input file")
  require(hardwareModel.isDefined != hardwareModels.nonEmpty,"you must have either hardwareModel or hardwareModels defined")

  def extract(verbose:Boolean):MappingProblem = {
    val cl = processingElementClasses.map(_.extract)
    setIndices(cl)
    Checker.checkDuplicates(cl.map(_.name),"processing element class")

    val sw = softwareModel.extract(cl,verbose)

    val hws = (hardwareModel,hardwareModels) match{
      case (None,l) if l.nonEmpty => l.map(_.extract(cl,sw))
      case (Some(m),Nil) => List(m.extract(cl,sw))
      case _ => throw new Error("you must have either hardwareModel or hardwareModels defined and not both")
    }

    val cls = new ConstraintList(constraintsAndObjectives.map(_.extract(if(hws.size == 1) Some(hws.head.processors) else None,sw)))

    MappingProblem(
      timeUnit,
      dataUnit,
      energyUnit,
      info match{case None => ""; case Some(i) => i},
      SortedMap.empty[String, Int] ++ properties.map(_.toCouple),
      cl,
      sw,
      hws,
      cls)
  }
}


case class EMappingConstraint(runOn:Option[ERunOn],
                              notRunOn:Option[ERunOn],
                              samePE:Option[List[String]],
                              notSamePE:Option[List[String]],
                              mustBeUsed:Option[String],
                              mustNotBeUsed:Option[String],
                              symmetricPE:Option[List[String]],
                              breaking:Option[String],
                              symmetricTasks:Option[List[String]],
                              simpleObjective:Option[String],
                              multiObjective:Option[EPareto],
                              powerCap:Option[Int],
                              energyCap:Option[Int],
                              maxMakespan:Option[Int],
                              maxWidth:Option[Int],
                              startTime:Option[ETaskTime],
                              forbidHardware:Option[List[String]]){
  def extract(hwOpt:Option[Iterable[ProcessingElement]],sw:SoftwareModel):MappingConstraint = {

    def extractRunOn(c:ERunOn,value:Boolean):MappingConstraint = {
      hwOpt match{
        case None => throw new Error("runOn and notRunOn constraints must be defined in hardware section if multiple hardware used")
        case Some(hw) =>
          val processor = hw.find(p => p.name equals c.processingElement) match{
            case Some(x) => x
            case None => throw new Error("cannot find processor" + c.processingElement + " used in mappingConstraint " + c)}

          val process = sw.simpleProcesses.find(p => p.name equals c.task) match{
            case Some(x) => x
            case None => throw new Error("cannot find process " + c.task + " used in mappingConstraint " + c)}

          RunOnConstraint(processor,process,value)
      }
    }

    def extractSameCore(c:List[String],value:Boolean):MappingConstraint = {
      val processes = c.map(process => sw.simpleProcesses.find(p => p.name equals process) match{
        case Some(x) => x
        case None => throw new Error("cannot find process " + process + " used in mappingConstraint " + c)})

      CoreSharingConstraint(processes,value)
    }

    def extractMustBeUsed(processorName:String,value:Boolean):MustBeUsedConstraint = {
      hwOpt match {
        case None => throw new Error("mustBeUsed constraints must be defined in hardware section if multiple hardware used")
        case Some(hw) =>
          val processor = hw.find(p => p.name equals processorName) match {
            case Some(x) => x
            case None => throw new Error("cannot find processor" + processorName + " used in mappingConstraint")
          }
          MustBeUsedConstraint(processor, value)
      }
    }

    def extractPESymmetry(symmetricPENames:List[String], breaking:Option[String]):SymmetricPEConstraint = {
      hwOpt match {
        case None => throw new Error("symmetricPE constraints must be defined in hardware section if multiple hardware used")
        case Some(hw) =>
          val processors = symmetricPENames.map(processorName => hw.find(p => p.name equals processorName) match {
            case Some(x) => x
            case None => throw new Error("cannot find processor" + processorName + " used in mappingConstraint")
          })

          SymmetricPEConstraint(processors,extractBreaking(breaking))
      }
    }

    def extractStartTime(startTime:ETaskTime):StartTimeConstraint = {
      val task = sw.simpleProcesses.find(p => p.name equals startTime.task) match{
        case Some(x) => x
        case None => throw new Error("cannot find task " + startTime.task + " used in mappingConstraint " + startTime)}

      StartTimeConstraint(task,startTime.time)
    }

    def extractBreaking(breaking:Option[String]):SymmetricPEConstraintType.Value = {
      breaking match{
        case None => SymmetricPEConstraintType.Workload
        case Some("longTask") => SymmetricPEConstraintType.LongTask
        case Some("workload") => SymmetricPEConstraintType.Workload
        case _ => throw new Error("unknown breaking for task symmetry constraint:" + breaking)
      }
    }


    def extractSymmetricTasksConstraint(tasks:List[String]) : SymmetricTasksConstraint = {
      val processes = tasks.map(task => sw.simpleProcesses.find(p => p.name equals task) match {
        case Some(x) => x
        case None => throw new Error("cannot find task " + task + " used in mappingConstraint SymmetricTasks(" + tasks.mkString(",") + ")")
      })
      SymmetricTasksConstraint(processes)
    }

    var toReturn:MappingConstraint = null
    def acc(c:MappingConstraint): Unit ={
      require(toReturn == null,"only one constraint can be specified at a time")
      toReturn = c
    }

    runOn match{
      case Some(s) =>
        acc(extractRunOn(s,true))
      case _ => ;
    }
    notRunOn match{
      case Some(s) =>
        acc(extractRunOn(s,false))
      case _ => ;
    }
    samePE match{
      case Some(s) =>
        acc( extractSameCore(s,true))
      case _ => ;
    }
    notSamePE match{
      case Some(s) =>
        acc(extractSameCore(s,false))
      case _ => ;
    }
    mustBeUsed match{
      case Some(s) =>
        acc(extractMustBeUsed(s,true))
      case _ => ;
    }
    mustNotBeUsed match{
      case Some(s) =>
        acc(extractMustBeUsed(s,false))
      case _ => ;
    }
    symmetricPE match{
      case Some(s) =>
        acc(extractPESymmetry(s, breaking))
      case _ => ;
    }
    symmetricTasks match{
      case Some(taskNames) =>
        acc(extractSymmetricTasksConstraint(taskNames))
      case _ => ;
    }
    simpleObjective match{
      case Some(s) =>
        acc(EGoal.extractSimple(s))
      case _ => ;
    }
    multiObjective match{
      case Some(s) =>
        acc(s.extract)
      case _ => ;
    }
    powerCap match{
      case Some(maxPower) =>
        acc(PowerCap(maxPower))
      case _ => ;
    }
    energyCap match{
      case Some(maxEnergy) =>
        acc(EnergyCap(maxEnergy:Int))
      case _ => ;
    }
    maxMakespan match{
      case Some(maxMakeSpan) =>
        acc(MaxMakespan(maxMakeSpan))
      case _ => ;
    }
    maxWidth match{
      case Some(maxDelay) =>
        acc(WidthCap(maxDelay:Int))
      case _ => ;
    }
    startTime match{
      case Some(ts) =>
        acc(extractStartTime(ts))
      case _ => ;
    }

    forbidHardware match{
      case Some(hardwares) =>
        acc(ForbidHardwarePlatform(SortedSet.empty[String] ++ hardwares))
      case _ => ;
    }
    require(toReturn != null, "empty constraint found; maybe you misspelled something?")

    toReturn
  }
}

case class ETaskTime(task:String,time:Int)
case class ERunOn(task:String,processingElement:String)

object EGoal{
  def extractSimple(name:String):SimpleMappingGoal = {
    name match{
      case "minEnergy" => MinEnergy()
      case "minMakespan" => MinMakeSpan()
      case "minFrame" => MinFrame()
      case _ => throw new Error("unknown simple mapping goal:" + name + "   expected: minEnergy, minMakespan, minFrame")
    }
  }
}

case class EPareto(a:String,b:String){
  def extract:MinPareto = MinPareto(EGoal.extractSimple(a), EGoal.extractSimple(b))
}

case class ESoftwareModel(sharedPermanentFunctions:List[EParametricImplementation],
                          tasks: Array[EAtomicTask],
                          transmissions: Array[ETransmission],
                          properties:List[ENameValue] = List.empty,
                          softwareClass: String) {
  def extract(cl:Array[ProcessingElementClass],verbose:Boolean) = {

    Checker.checkDuplicates(tasks.map(_.name),"task")
    Checker.checkDuplicates(transmissions.map(_.name),"transmission")

    val standardImplems = sharedPermanentFunctions.map(_.extract(cl)).toArray

    val proc = tasks.map(task => task.extract(cl,standardImplems))
    SoftwareModel(
      standardImplems,
      proc,
      transmissions.map(_.extract(proc)),
      ESoftwareClass.extract(softwareClass),
      SortedMap.empty[String,Int] ++ properties.map(_.toCouple),
      verbose)
  }
}
import placer.metadata.sw.SoftwareClass._

object ESoftwareClass {

  def extract(softwareClass: String): SoftwareClass = {
    softwareClass match {
      case "oneShotSoftware" => SoftwareClass.OneShotSoftware
      case "iterativeSoftware" => SoftwareClass.IterativeSoftware
      case _ => throw new Error("unknown softwareClass:" + softwareClass)
    }
  }
}

case class EAtomicTask(name: String,
                       implementations: List[EParametricImplementation],
                       sharedImplementation:List[String]) {
  Checker.checkDuplicates(implementations.map(_.name),"implementation of task " + name)
  require(implementations.nonEmpty,"task " + name + " has no implementation")

  def extract(cl:Array[ProcessingElementClass],standardImplems:Array[ParametricImplementation]) = {
    val translatedImplems = implementations.map(implementation => implementation.extract(cl))

    def extractReferenceImplem(sharedImplementation:String):ReferenceToSharedParametricImplementation = {
      standardImplems.find(_.name equals sharedImplementation) match {
        case Some(x) => ReferenceToSharedParametricImplementation(x);
        case None => throw new Error("cannot find shared implementation " + sharedImplementation)
      }
    }

    val translatedSharedImplementation = sharedImplementation.map(si => extractReferenceImplem(si))

    AtomicTask(translatedImplems, translatedSharedImplementation, name)
  }
}

case class ENameValue(name: String, value: Int) {
  def toCouple: (String, Int) = (name, value)
}

case class EParametricImplementation(name: String,
                                     target: String,
                                     nbThreads:Option[String],
                                     resourceUsage: List[ENameFormula],
                                     computationMemory: String = "0",
                                     duration: String,
                                     overrideEnergy:Option[String],
                                     parameters: List[ENameValues]){
  val parsedComputationMemory = FormulaParser(computationMemory)
  val parsedDuration = FormulaParser(duration)

  val parsedResources = SortedMap.empty[String, Formula] ++ resourceUsage.map(_.extract)
  val parsedNbThreads = nbThreads match{case None => Const(1); case Some(f) => FormulaParser(f)}

  val parsedOverrideEnergy = overrideEnergy match{case None => None case Some(e) => Some(FormulaParser(e))}

  def extract(cl:Array[ProcessingElementClass]) = {
    val targetClass = cl.find(_.name equals target) match {
      case Some(x) => x;
      case None => throw new Error("cannot find processor class " + target + " used in implementation " + name)
    }

    require(targetClass.resources equals parsedResources.keySet,"error in declared resources of " + name)

    ParametricImplementation(
      name,
      targetClass,
      parsedNbThreads,
      parsedResources,
      parsedComputationMemory,
      parsedDuration,
      parsedOverrideEnergy,
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

case class EProcessingElementClass(multiTaskPermanentTasks: Option[EMultiTaskPermanentTasks], switchingTask: Option[ESwitchingTask]) {
  def extract: ProcessingElementClass = {
    (multiTaskPermanentTasks, switchingTask) match {
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

case class ESwitchingTask(name: String, resources: List[String], properties: List[String]) {
  def extract: SwitchingTask = SwitchingTask(name, SortedSet.empty[String] ++ resources, SortedSet.empty[String] ++ properties)
}

case class EProcessingElement(processorClass: String,
                              resources: List[ENameValue],
                              properties: List[ENameValue],
                              name: String,
                              memSize: Int = 0,
                              multiCore:Option[Int],
                              powerModel: String = "0",
                              switchingDelay: Option[Int]) {

  val parsedPowerModel = FormulaParser(powerModel)

  def extract(pc: Array[ProcessingElementClass]): ProcessingElement = {
    val peClass = pc.find(_.name equals processorClass)match{
      case None => throw new Error("cannot find processing element class " + processorClass + " used in processing element" + name)
      case Some(x) => x
    }
    val nbCore = multiCore match{
      case None => 1
      case Some(n) =>
        require(peClass.isInstanceOf[SwitchingTask],"multi core model can only be declared for MonoTaskSwitchingTask PE")
        n
    }

    val switchingDelayParsed = switchingDelay match{
      case None => 0
      case Some(delay) =>
        require(peClass.isInstanceOf[SwitchingTask],"switchingDelay can only be specified for MonoTaskSwitchingTask PE")
        delay
    }
    ProcessingElement(
      peClass,
      SortedMap.empty[String, Int] ++ resources.map(_.toCouple),
      SortedMap.empty[String, Int] ++ properties.map(_.toCouple),
      name,
      memSize,
      parsedPowerModel,
      nbCore,
      switchingDelayParsed
    )
  }
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
                          latency: Int = 0,
                          name: String) {
  require(relatedProcessors.size >= 1,"bus " + name + " should be linked to at least one PE")

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
                         latency: Int = 0,
                         name: String) {
  require(from.size >= 1,"bus " + name + " should have at least one from PE")
  require(to.size >= 1,"bus " + name + " should have at least one to PE")

  def extract(p: Array[ProcessingElement]) = SingleWayBus(
    from.map(name => p.find(_.name equals name) match{
      case Some(x) => x ;
      case None => throw new Error("cannot find processing element " + name + " used in bus " + this.name)}),

    to.map(name => p.find(_.name equals name) match{
      case Some(x) => x ;
      case None => throw new Error("cannot find processing element " + name + " used in bus " + this.name)}),

    timeUnitPerDataUnit,
    latency,
    name)
}

case class EHardwareModel(name: String,
                          processingElements: Array[EProcessingElement],
                          busses: Array[EBus],
                          properties: List[ENameValue],
                          constraints:List[EMappingConstraint]) {
  require(processingElements.nonEmpty,"no processing element declared")

  def extract(pc:Array[ProcessingElementClass],sw:SoftwareModel) = {
    val p = processingElements.map(_.extract(pc))
    Checker.checkDuplicates(p.map(_.name),"processing element")
    val b = busses.map(_.extract(p))
    Checker.checkDuplicates(b.map(_.name),"bus")
    HardwareModel(
      name,
      pc,
      p,
      b,
      new ConstraintList(constraints.map(_.extract(Some(p),sw))),
      SortedMap.empty[String, Int] ++ properties.map(_.toCouple))
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