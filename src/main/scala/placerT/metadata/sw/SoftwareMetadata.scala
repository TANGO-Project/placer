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

import placerT.io.JSonHelper
import placerT.metadata._
import placerT.metadata.hw._

import scala.collection.immutable.SortedMap




/**
 * @param target the PE class that can run it
 * @param resourceUsage the usage of resoruce, can only mention resources delared in the PE class
 * @param computationMemory is the memory needed toperform the computation.
 *                          We consider that the memory tat stores input and output data is allocated for the duration of the task.
 * @param duration
 */
case class FlattenedImplementation(name: String,
                                   target: ProcessingElementClass,
                                   resourceUsage: SortedMap[String, Int],
                                   computationMemory: Int,
                                   duration: Formula,
                                   originImplementation: ParametricImplementation = null,
                                   parameterValues: SortedMap[String, Int] = null) extends Indiced {

  require(resourceUsage.keySet subsetOf target.resources, "unknown resources specified in implementation " + target + ": " + (resourceUsage.keySet -- target.resources).mkString(","))
  require(duration.terms subsetOf target.properties, "duration is defined based on unknown features in implementation " + target + ": " + (duration.terms -- target.properties).mkString(","))

  override def toString: String = {
    "Implementation(" + name +
      " target:" + target.name +
      " resourceUsage{" + (("mem", computationMemory) :: resourceUsage.toList).map({ case (a, b) => a + ":" + b }).mkString(",") + "}" +
      " duration:" + duration.prettyPrint() +
      " parameters:{" + parameterValues.toList.map(nv => nv._1 + ":" + nv._2).mkString(",") + "}" + ")"
  }

  def duration(proc: ProcessingElement, properties: SortedMap[String, Int]): Int = {
    require(proc.processorClass == target, "cannot evaluate WCET on processor class " + proc + " for implementation for " + target)
    val tmp = Formula.eval(duration, properties ++ proc.properties)
    require(tmp >= 0, "got a negative runtime (" + tmp + ")for implementation " + this.name + " on " + proc.name)
    tmp
  }

  def description: String = name + "(" + parameterValues.toList.map(nv => nv._1 + ":" + nv._2).mkString(",") + ")"

  def toJSon: String = "{" +
    JSonHelper.string("name", name) + "," +
    JSonHelper.string("target", target.name) + "," +
    JSonHelper.multiples("resourceUsage", resourceUsage.map(ru => "{" + JSonHelper.string("name", ru._1) + "," + JSonHelper.int("value", ru._2) + "}")) + "," +
    JSonHelper.int("computationMemory", computationMemory) + "," +
    JSonHelper.string("duration", duration.prettyPrint()) + "}"

  def toParam: ParametricImplementation = ParametricImplementation(name: String,
    target: ProcessingElementClass,
    resourceUsage = resourceUsage.mapValues(v => Const(v)),
    computationMemory = Const(computationMemory),
    duration = duration,
    parameters = SortedMap.empty)
}


/**
 * @param target
 * @param resourceUsage
 * @param computationMemory is the memory needed toperform the computation. We consider that the memory tat stores input and output data is allocated for the duration of the task.
 * @param duration
 */
case class ParametricImplementation(name: String,
                                    target: ProcessingElementClass,
                                    resourceUsage: SortedMap[String, Formula],
                                    computationMemory: Formula,
                                    duration: Formula,
                                    parameters: SortedMap[String, Iterable[Int]]) {

  def implementations: Iterable[FlattenedImplementation] = {
    parameterize(parameters.toList, SortedMap.empty[String, Int])
  }

  private def parameterize(freeParameters: List[(String, Iterable[Int])], setParameters: SortedMap[String, Int]): Iterable[FlattenedImplementation] = {
    def resolve(f: Formula): Int = Formula.simplifyConstants(f, setParameters) match {
      case Const(c) => c
      case _ => throw new Error("cannot simplify formula to constant")
    }
    def simplify(f: Formula): Formula = Formula.simplifyConstants(f, setParameters)
    freeParameters match {
      case Nil =>
        Some(FlattenedImplementation(name,
          target,
          resourceUsage.mapValues(resolve),
          resolve(computationMemory),
          simplify(duration),
          this,
          setParameters))
      case (param, values) :: tail =>
        values.flatMap(value => parameterize(tail, setParameters + ((param, value))))
    }
  }

  override def toString: String = {
    "ParametricImplementation(" + name + " target:" + target.name +
      " parameters:{" + parameters.toList.map({ case (parameterName, values) => parameterName + ":{" + values.mkString(",") + "}" }).mkString(",") + "}" +
      " resourceUsage{" + (("mem", computationMemory) :: resourceUsage.toList).map({ case (a, b: Formula) => a + ":" + b.prettyPrint() }).mkString(",") + "} duration:" + duration.prettyPrint() + ")"
  }

  def toJSon: String = "{" +
    JSonHelper.string("name", name) + "," +
    JSonHelper.string("target", target.name) + "," +
    JSonHelper.multiples("parameters", parameters.map(paramAmount => "{" + JSonHelper.string("name", paramAmount._1) + "," + JSonHelper.multiples("values", paramAmount._2.map("" + _)) + "}")) + "," +
    JSonHelper.multiples("resourceUsage", resourceUsage.map(ru => "{" + JSonHelper.string("name", ru._1) + "," + JSonHelper.string("formula", ru._2.prettyPrint()) + "}")) + "," +
    JSonHelper.string("computationMemory", computationMemory.prettyPrint()) + "," +
    JSonHelper.string("duration", duration.prettyPrint()) + "}"
}


case class AtomicTask(implementations: List[ParametricImplementation],
                      name: String) extends Indiced() with IndiceMaker {

  val implementationArray: Array[FlattenedImplementation] = implementations.flatMap(_.implementations).toArray
  setIndices(implementationArray)
  val computingHardwareToImplementations: Map[ProcessingElementClass, List[FlattenedImplementation]] = implementationArray.toList.groupBy(_.target)

  //  println("\n" + implementationArray.toList.mkString("\n") + "\n")

  override def toString: String = "Task(" + name + " implems:{" + implementations.mkString("") + "})"

  def multiString: List[String] = ("Task(" + name) :: "\t implems:{" :: implementations.map(i => "\t\t" + i.toString)

  def canRunOn(p: ProcessingElement): Boolean = {
    computingHardwareToImplementations.get(p.processorClass) match {
      case None => return false
      case Some(Nil) => return false
      case Some(implementationList) =>
        for (implementation <- implementationList) {
          val metrics = implementation.resourceUsage
          if (metrics.forall({ case (dim, req) => p.resources.get(dim).head >= req })) {
            return true
          }
        }
    }
    true
  }

  def maxDuration(procs: Iterable[ProcessingElement], properties: SortedMap[String, Int]): Int = {
    var maxDur = 0
    for (proc <- procs) {
      for (implem <- computingHardwareToImplementations.getOrElse(proc.processorClass, List.empty)) {
        val dur = implem.duration(proc, properties)
        if (dur > maxDur) maxDur = dur
      }
    }
    maxDur
  }

  def toJSon: String = "{" +
    JSonHelper.string("name", name) + "," +
    JSonHelper.multiples("implementations", implementations.map(_.toJSon)) +
    "}"
}

object TransmissionTiming extends Enumeration {
  type TransmissionTiming = Value
  val Asap, Alap, Free = Value
}

import placerT.metadata.sw.TransmissionTiming._

//on peut avoir des flux vers les process atomiques, auquel cas, c'est additioné par process sur le même HW; on peutaussi avoir des communication vers le Groupe,
// auquel cas, chaque process a besoin de la totalité du flux; c'st donc partagé si deux process du groupe sont sur le même HW

//transmission uses the amount of memory on source until terminated, and uses amount of memory on destination until consuming task started
case class Transmission(source: AtomicTask,
                        target: AtomicTask,
                        size: Int,
                        timing: TransmissionTiming,
                        name: String) extends Indiced() {
  override def toString: String = "Transmission(" + name + " " + source.name + "->" + target.name + " size:" + size + " timing:" + timing + ")"

  def shortString: String = "Transmission(" + name + " " + source.name + "->" + target.name + ")"

  def toJSon: String = "{" +
    JSonHelper.string("name", name) + "," +
    JSonHelper.string("source", source.name) + "," +
    JSonHelper.string("target", target.name) + "," +
    JSonHelper.int("size", size) + "," +
    JSonHelper.string("timing", timing.toString) + "}"
}

/**
 *
 * @param simpleProcesses the processes
 * @param transmissions the transmissions between processes
 * @param softwareClass the class of software, and its time requirements
 */
case class SoftwareModel(simpleProcesses: Array[AtomicTask],
                         tasksSets:Array[TaskSet],
                         transmissions: Array[Transmission],
                         softwareClass: SoftwareClass) extends IndiceMaker {
  setIndices(simpleProcesses)
  setIndices(transmissions)

  require(transmissions.forall(f => f.source.id != -1 && f.target.id != -1), "some transmissions refer to non-registered tasks")

  override def toString: String = "SoftwareModel(\n" +
    "\ttasks:[\n\t\t" + simpleProcesses.flatMap(_.multiString).mkString("\n\t\t") + "] \n" +
    "\ttransmissions:[\n\t\t" + transmissions.mkString("\n\t\t") + "\n" +
    "\t" + softwareClass + "])"

  def toJSon: String = "{" +
    JSonHelper.multiples("simpleProcesses", simpleProcesses.map(_.toJSon)) + "," +
    JSonHelper.multiples("transmissions", transmissions.map(_.toJSon)) + "," +
    JSonHelper.complex("softwareClass", softwareClass.toJSon) +
    "}"
}

abstract sealed class SoftwareClass() {
  def toJSon: String
}

case class OneShotSoftware(maxDelayRequirement: Option[Int]) extends SoftwareClass() {
  override def toString: String = "OneShotSoftware(maxDelayRequirement:" + maxDelayRequirement + ")"

  override def toJSon: String = "{" + JSonHelper.complex("oneShotSoftware", "{" + (maxDelayRequirement match {
    case None => "";
    case Some(t: Int) => JSonHelper.int("maxDelay", t)
  }) + "}") + "}"
}

case class IterativeSoftware(maxMakespan:Option[Int],maxFrameDelay:Option[Int]) extends SoftwareClass(){
  override def toJSon: String = "{" + JSonHelper.complex("iterativeSoftware","{" +
    (maxMakespan match{case None => "" ; case Some(t:Int) => JSonHelper.int("maxMakespan",t) + ","}) +
    (maxFrameDelay match{case None => "" ; case Some(t:Int) => JSonHelper.int("maxFrameDelay",t)}) +
    "}") + "}"

  override def toString: String = "IterativeSoftware(maxMakespan:" + maxMakespan + " maxFrameDelay:" + maxFrameDelay + ")"
}
