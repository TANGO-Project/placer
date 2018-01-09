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



package placerT.metadata.hw


//general problem about monitorability: what if some other task must wait for another one to finish
//and they are not on the same processor, and not linked by any transmission?

import placerT.io.JSonHelper
import placerT.metadata._
import placerT.metadata.sw.ParametricImplementation

import scala.collection.immutable.{SortedMap, SortedSet}


/**
  * @param name
  * @param resources
  * @param properties
  */
abstract class ProcessingElementClass(val name: String,
                                      val resources: SortedSet[String],
                                      val properties: SortedSet[String])
  extends Indiced with Ordered[ProcessingElementClass] {

  override def compare(that: ProcessingElementClass): Int = this.name compare that.name

  override def toString: String = name + " resources:{" + resources.mkString(",") + "}" + " properties:{" + properties.mkString(",") + "}"

  def checkPowerModel(powerModel: Formula)

  val zeroResources: SortedMap[String, Int] = SortedMap.empty[String, Int] ++ resources.map(d => (d, 0))

  def allAttributes: SortedSet[String] = resources ++ properties

  def enrichWithBuzy(formula: Formula): Formula = formula

  def toJSon: String
}


/**
  * like a FPGA: can perform many tasks at the same time, but cannot be re-fitted with other tasks (approximation)
  * we do not have switching in this case because switching can be performed only when all tasks are competed,
  * and it is very difficult to represent this in the optimization model
  */
//bin-packing non-timed (since permanent use), but task still have duration
case class MultiTaskPermanentTasks(override val name: String,
                                   override val resources: SortedSet[String],
                                   override val properties: SortedSet[String])
  extends ProcessingElementClass(name: String, resources: SortedSet[String], properties: SortedSet[String]) {
  def checkPowerModel(powerModel: Formula) {
    //must be linear with respect to metric usage
    require(Formula.isLinear(powerModel), "power model must be linear wrt dimensions for multi-task switching model:" + powerModel.prettyPrint())
  }

  override def toString: String = "MultiTaskPermanentTasks(" + super.toString + ")"

  override def toJSon: String = {
    "{" +
      JSonHelper.complex("multiTaskPermanentTasks", "{" +
        JSonHelper.string("name", name) + "," +
        JSonHelper.strings("resources", resources) + "," +
        JSonHelper.strings("properties", properties) +
        "}") + "}"
  }
}

/**
  * can only perform a single task at a time, and there is a delay to switch to another task.
  * during this delay, it does nothing
  */
// unaryResource(starts: Array[CPIntVar], durations: Array[CPIntVar], ends: Array[CPIntVar], required: Array[CPBoolVar])
case class SwitchingTask(override val name: String, override val resources: SortedSet[String], override val properties: SortedSet[String])
  extends ProcessingElementClass(name: String, resources: SortedSet[String], properties: SortedSet[String]) {

  def checkPowerModel(powerModel: Formula) {}

  override def toString: String = "switchingTask(" + super.toString + ")"

  override def allAttributes: SortedSet[String] = super.allAttributes + "buzy"

  override def enrichWithBuzy(formula: Formula): Formula = Formula.simplifyConstants(formula, SortedMap("buzy" -> 1))

  override def toJSon: String = {
    "{" +
      JSonHelper.complex("switchingTask", "{" +
        JSonHelper.string("name", name) + "," +
        JSonHelper.strings("resources", resources) + "," +
        JSonHelper.strings("properties", properties) + "," +
        "}") + "}"
  }
}

/**
  * @param processorClass
  * @param resources
  * @param name
  * @param memSize
  * @param powerModel is the power that the processor consumes. dimensions are set to zero when nothing executes. must be linear for multiTask processors
  */
case class ProcessingElement(processorClass: ProcessingElementClass,
                             resources: SortedMap[String, Int],
                             properties: SortedMap[String, Int],
                             name: String,
                             memSize: Int,
                             powerModel: Formula,
                             nbCore:Int,
                             switchingDelay:Int) //expressed in term of resource usage and features
  extends Indiced() with Ordered[ProcessingElement] {

  require(switchingDelay ==0 || processorClass.isInstanceOf[SwitchingTask],"switching delay can only be declared for switching task PE")
  require(nbCore == 1 || processorClass.isInstanceOf[SwitchingTask],"multi cores can only be declared for switching task PE")

  val simplifiedPowerModel = Formula.simplifyConstants(powerModel, properties)
  val (constantPower: Const, powerModelForTaskTmp: Formula) = Formula.splitConstant(simplifiedPowerModel)

  val powerModelForTask = processorClass.enrichWithBuzy(powerModelForTaskTmp)

  processorClass.checkPowerModel(simplifiedPowerModel)
  require(resources.keySet subsetOf processorClass.resources,
    "unknown resources specified in " + name + ": " + (resources.keySet -- processorClass.resources).mkString(","))
  require(processorClass.resources subsetOf resources.keySet,
    "missing resources in " + name + ": " + (processorClass.resources -- resources.keySet).mkString(","))

  require(properties.keySet subsetOf processorClass.properties,
    "unknown properties specified in " + name + ": " + (properties.keySet -- processorClass.properties).mkString(","))
  require(processorClass.properties subsetOf properties.keySet,
    "missing properties in " + name + ": " + (processorClass.properties -- properties.keySet).mkString(","))

  require(powerModel.terms subsetOf processorClass.allAttributes,
    "unknown resources specified in powermodel of " + name + ": " +
      (powerModel.terms -- processorClass.allAttributes).mkString(","))

  override def toString: String = "ProcessingElement(" + name + " " +
    processorClass.name + " resources:{" + resources.toList.map({ case (a, b) => a + ":" + b }).mkString(" ") + "} " +
    "properties:{" + properties.toList.map({ case (a, b) => a + ":" + b }).mkString(" ") + "}" +
    " localMem:" + memSize +
    " powerModel:" + powerModel.prettyPrint() + ")"

  override def compare(that: ProcessingElement): Int = {
    require(this.id != -1)
    require(that.id != -1)
    this.id compare that.id
  }

  def toJSon: String = {
    "{" +
      JSonHelper.string("processorClass", processorClass.name) + "," +
      JSonHelper.string("name", name) + "," +
      JSonHelper.int("memSize", memSize) + "," +
      JSonHelper.multiples("resources", resources.toList.map({ case (r, a) => "{" + JSonHelper.string("name", r) + "," + JSonHelper.int("value", a) + "}" })) + "," +
      JSonHelper.multiples("properties", properties.toList.map({ case (p, a) => "{" + JSonHelper.string("name", p) + "," + JSonHelper.int("value", a) + "}" })) + "," +
      JSonHelper.string("powerModel", powerModel.prettyPrint()) + "," +
      JSonHelper.int("nbCores", nbCore) + "," +
      JSonHelper.int("switchingDelay", switchingDelay) + "}"
  }

  def symmetricTo(that:ProcessingElement):Boolean = {
    ((this.processorClass == that.processorClass) &&
      (this.resources equals that.resources) &&
      (this.properties equals that.properties) &&
      (this.memSize == that.memSize) &&
      (this.powerModel equals that.powerModel))
  }
}


//communication share  busses.
//we can choose: either round-robin scheme for the whole bus, or prioritized
//prioritized = transmissions are not split
//round-robin bus = transmissions cut into chunks, and chuncks transmitted into round-robin scheme (chunnk size = param of the bus)


//do we share communications or not??
//i propose to split all communication into chunks that are actual tasks, and prioritize on chunks
//bus are therefore unaryResource
sealed abstract class Bus(val name: String, val timeUnitPerDataUnit: Int, val latency: Int) extends Indiced() {
  require(timeUnitPerDataUnit >= 0, "creating bus " + name + " with unauthorized timeUnitPerDataUnit:" + timeUnitPerDataUnit)
  require(latency >= 0, "creating bus " + name + " with unauthorized delay:" + latency)

  def close()

  def canReceiveFlowFrom(p: ProcessingElement): Boolean

  def canSentFlowTo(p: ProcessingElement): Boolean

  def receivingFromProcessors: Set[ProcessingElement]

  def sendingToProcessors: Set[ProcessingElement]

  /**
    * compute the duration for transmissing a certain amount of data.
    * delay + size * timeUnitPerBit
    * @param size
    * @return
    */
  def transmissionDuration(size: Int): Int = latency + size * timeUnitPerDataUnit

  def toJSon: String
}

case class HalfDuplexBus(relatedProcessors: List[ProcessingElement],
                         override val timeUnitPerDataUnit: Int,
                         override val latency: Int,
                         override val name: String)
  extends Bus(name, timeUnitPerDataUnit, latency) {
  require(timeUnitPerDataUnit >= 0, "creating bus " + name + " with unauthorized timeUnitPerDataUnit:" + timeUnitPerDataUnit + "; should be >= 1")

  override def toString: String = "HalfDuplexBus(" +
    name + " processors:{" + relatedProcessors.map(_.name).mkString(",") + "} timeUnitPerDataUnit:" +
    timeUnitPerDataUnit + " latency:" + latency + ")"

  var relatedProcessorSet: SortedSet[ProcessingElement] = null

  def close(): Unit = {
    relatedProcessorSet = SortedSet.empty[ProcessingElement] ++ relatedProcessors
  }

  override def canReceiveFlowFrom(p: ProcessingElement): Boolean = relatedProcessorSet.contains(p)

  override def canSentFlowTo(p: ProcessingElement): Boolean = relatedProcessorSet.contains(p)

  override def receivingFromProcessors: Set[ProcessingElement] = relatedProcessorSet

  override def sendingToProcessors: Set[ProcessingElement] = relatedProcessorSet

  def toJSon: String = {
    "{" + JSonHelper.complex("halfDuplexBus", "{" +
      JSonHelper.string("name", name) + "," +
      JSonHelper.strings("relatedProcessors", relatedProcessors.map(_.name)) + "," +
      JSonHelper.int("timeUnitPerDataUnit", timeUnitPerDataUnit) + "," +
      JSonHelper.int("latency", latency) + "}") + "}"
  }
}

case class SingleWayBus(private val from: List[ProcessingElement],
                        to: List[ProcessingElement],
                        override val timeUnitPerDataUnit: Int,
                        override val latency: Int,
                        override val name: String)
  extends Bus(name, timeUnitPerDataUnit, latency) {
  require(timeUnitPerDataUnit > 0, "creating bus " + name + " with unauthorized timeUnitPerDataUnit:" + timeUnitPerDataUnit + "; should be >= 1")

  override def toString: String = "SingleWayBus(" + name +
    " from:{" + from.map(_.name).mkString(",") + "} to:{" +
    to.map(_.name).mkString(",") + "} timeUnitPerDataUnit:" +
    timeUnitPerDataUnit + " delay:" + latency + ")"

  var fromProcessorSet: SortedSet[ProcessingElement] = null
  var toProcessorSet: SortedSet[ProcessingElement] = null

  def close(): Unit = {
    fromProcessorSet = SortedSet.empty[ProcessingElement] ++ from
    toProcessorSet = SortedSet.empty[ProcessingElement] ++ to
  }

  override def canReceiveFlowFrom(p: ProcessingElement): Boolean = fromProcessorSet.contains(p)

  override def canSentFlowTo(p: ProcessingElement): Boolean = toProcessorSet.contains(p)

  override def receivingFromProcessors: Set[ProcessingElement] = fromProcessorSet

  override def sendingToProcessors: Set[ProcessingElement] = toProcessorSet

  def toJSon: String = {
    "{" + JSonHelper.complex("singleWayBus", "{" +
      JSonHelper.string("name", name) + "," +
      JSonHelper.strings("from", from.map(_.name)) + "," +
      JSonHelper.strings("to", to.map(_.name)) + "," +
      JSonHelper.int("timeUnitPerDataUnit", timeUnitPerDataUnit) + "," +
      JSonHelper.int("latency", latency) + "}") + "}"
  }
}


case class SelfLoopBus(proc: ProcessingElement) extends Bus("local loop on " + proc.name, 0, 0) {

  override def toString: String = "local loop on " + proc.name

  override def close() {}

  override def sendingToProcessors: Set[ProcessingElement] = Set(proc)

  override def canSentFlowTo(p: ProcessingElement): Boolean = p == proc

  override def receivingFromProcessors: Set[ProcessingElement] = Set(proc)

  override def canReceiveFlowFrom(p: ProcessingElement): Boolean = p == proc

  def toJSon: String = throw new Error("not supposed to be JSon serialized")
}

case class HardwareModel(name: String,
                         processorClasses: Array[ProcessingElementClass],
                         processors: Array[ProcessingElement],
                         busses: Array[Bus],
                         properties: SortedMap[String, Int] = SortedMap.empty,
                         powerCap: Option[Int] = None,
                         energyCap: Option[Int] = None) extends IndiceMaker {
  setIndices(processors)
  setIndices(busses)
  setIndices(processorClasses)
  busses.foreach(_.close())

  override def toString: String = "HardwareModel(\n" +
    "\tname:" + name + "\n" +
    (powerCap match {
      case None => "";
      case Some(cap) => "powerCap:" + cap + "\n"
    }) +
    (energyCap match {
      case None => "";
      case Some(cap) => "\tenergyCap:" + cap + "\n"
    }) +
    "\tprocessingElementClasses:{\n\t\t" + processorClasses.mkString("\n\t\t") + "}\n" +
    "\tprocessingElements:{\n\t\t" + processors.mkString("\n\t\t") + "} \n" +
    "\tbusses:{\n\t\t" + busses.mkString("\n\t\t") + "}\n" +
    "\tproperties:{" + properties.toList.map({ case (propertyName, value) => propertyName + ":" + value }).mkString(",") + "}\n" +
    ")"

  def toJSon: String = "{" +
    JSonHelper.string("name", name) + "," +
    (powerCap match {
      case None => "";
      case Some(cap) => """"powerCap":""" + cap + ","
    }) +
    (energyCap match {
      case None => "";
      case Some(cap) => """"energyCap":""" + cap + ","
    }) +
    JSonHelper.multiples("processingElementClasses", processorClasses.map(_.toJSon)) + "," +
    JSonHelper.multiples("processingElements", processors.map(_.toJSon)) + "," +
    JSonHelper.multiples("busses", busses.map(_.toJSon)) + "," +
    JSonHelper.multiples("properties", properties.toList.map({ case (propertyName, value) => JSonHelper.nameIntValue(propertyName, value) })) +
    "}"
}
