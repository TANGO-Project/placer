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
 * You should have received a copy of the GNU Lesser General Public License along with placer.
 * If not, see http://www.gnu.org/licenses/lgpl-3.0.en.html
 *
 */

package placer.metadata.sw

import placer.algo.hw.CPInstantiatedPermanentFunction
import placer.io.JSonHelper
import placer.metadata._
import placer.metadata.hw._

import scala.collection.immutable.SortedMap



abstract sealed class FlattenedImplementation() extends Indiced{

  def description: String

  def computationMemory:Int

  def name:String

  def parameterValues: SortedMap[String, Int]
}


case class ReferenceToSharedFlattenedImplementationConcrete(f:FlattenedImplementationConcrete,target:CPInstantiatedPermanentFunction) extends FlattenedImplementation{
  require(f.target.isInstanceOf[MultiTaskPermanentTasks],"reference to standard implementation should only refer to MultiTaskPermanentTasks")

  override def description: String = "shared:" + f.description

  def name = f.name

  override def computationMemory:Int = f.computationMemory

  override def parameterValues: SortedMap[String, Int] = f.parameterValues
}


/**
  * @param target the PE class that can run it
  * @param resourceUsage the usage of resource, can only mention resources declared in the PE class
  * @param computationMemory is the memory needed to perform the computation.
  *                          We consider that the memory tat stores input and output data is allocated for the duration of the task.
  * @param duration the duration of the implementation when executed on the specified target
  */
case class FlattenedImplementationConcrete(name: String,
                                           target: ProcessingElementClass,
                                           nbThreads:Int,
                                           resourceUsage: SortedMap[String, Int],
                                           computationMemory: Int,
                                           duration: Formula,
                                           overrideEnergy:Option[Int],
                                           originImplementation: ParametricImplementation = null,
                                           parameterValues: SortedMap[String, Int] = null) extends FlattenedImplementation {

  require(resourceUsage.keySet subsetOf target.resources, "unknown resources specified in implementation " + target + ": " + (resourceUsage.keySet -- target.resources).mkString(","))
  require(duration.terms subsetOf target.properties, "duration is defined based on unknown features in implementation " + target + ": " + (duration.terms -- target.properties).mkString(","))
  require(target match{case _:SwitchingTask => true; case _ => nbThreads == 1},"mult thread can only be specified for implem running on switchig task PE")

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

  def canRunOn(p: ProcessingElement): Boolean = {
    p.processorClass == this.target && resourceUsage.forall({case (dim, req) => p.resources.get(dim).head >= req})
  }

  def description: String = name + "(" + parameterValues.toList.map(nv => nv._1 + ":" + nv._2).mkString(",") + ")"

  def toParam: ParametricImplementation = ParametricImplementation(name: String,
    target: ProcessingElementClass,
    nbThreads,
    resourceUsage = resourceUsage.mapValues(v => Const(v)),
    computationMemory = Const(computationMemory),
    duration = duration,
    overrideEnergy = overrideEnergy match{case Some(e) => Some("" + e); case None => None},
    parameters = SortedMap.empty)
}


abstract sealed class Implementation extends Indiced() {

}

case class ReferenceToSharedParametricImplementation(p:ParametricImplementation) extends Implementation{
  require(p.target.isInstanceOf[MultiTaskPermanentTasks],"reference to standard implementation should only refer to MultiTaskPermanentTasks")

  def sharedFlattenedImplementations: Iterable[FlattenedImplementationConcrete] = {
    p.implementations
  }
}

/**
  * @param target
  * @param resourceUsage
  * @param computationMemory is the memory needed toperform the computation. We consider that the memory tat stores input and output data is allocated for the duration of the task.
  * @param duration the duration of the implementation when executed on the specified target
  */
case class ParametricImplementation(name: String,
                                    target: ProcessingElementClass,
                                    nbThreads:Formula,
                                    resourceUsage: SortedMap[String, Formula],
                                    computationMemory: Formula,
                                    duration: Formula,
                                    overrideEnergy:Option[Formula],
                                    parameters: SortedMap[String, Iterable[Int]]) extends Implementation {

  def implementations:Iterable[FlattenedImplementationConcrete] = {
    parameterize(parameters.toList, SortedMap.empty[String, Int])
  }

  private def parameterize(freeParameters: List[(String, Iterable[Int])], setParameters: SortedMap[String, Int]): Iterable[FlattenedImplementationConcrete] = {
    def resolve(f: Formula): Int = Formula.simplifyConstants(f, setParameters) match {
      case Const(c) => c
      case _ => throw new Error("cannot simplify formula to constant")
    }
    def simplify(f: Formula): Formula = Formula.simplifyConstants(f, setParameters)
    freeParameters match {
      case Nil =>
        Some(FlattenedImplementationConcrete(name,
          target,
          simplify(nbThreads) match {
            case Const(c) => c
            case x =>
              throw new Error("cannot simplify nbThreads based on parameters of implementation, remaining: " + x.prettyPrint())},
          resourceUsage.mapValues(resolve),
          resolve(computationMemory),
          simplify(duration),
          overrideEnergy match{
            case None => None
            case Some(e) => simplify(e) match {
              case Const(c) => Some(c)
              case x =>
                throw new Error("cannot simplify overrideEnergy based on parameters of implementation, remaining: " + x.prettyPrint())}},
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

}

case class AtomicTask(implementations: List[ParametricImplementation],
                      sharedImplementations:List[ReferenceToSharedParametricImplementation],
                      name: String) extends Indiced() with IndiceMaker {

  //  println("\n" + implementationArray.toList.mkString("\n") + "\n")

  override def toString: String = "Task(" + name + " implems:{" + implementations.mkString("") + "} sharedImplems:{" + sharedImplementations.mkString(",") + "})"

  def multiString: List[String] = ("Task(" + name) :: "\t implems:{" :: (implementations.map(i => "\t\t" + i.toString) ::: List("\t }")) ::: ("\t sharedImplems:{" :: sharedImplementations.map(i => "\t\t" + i.p.name) ::: List("\t }"))

  def maxDuration(procs: Iterable[ProcessingElement], properties: SortedMap[String, Int]): Int = {
    var maxDur = 0
    for (proc <- procs) {
      for (implem <- implementations.flatMap(_.implementations)) {
        if (implem.canRunOn(proc)) {
          val dur = implem.duration(proc, properties)
          if (dur > maxDur) maxDur = dur
        }
      }
    }

    //we make a second loop with proc and implem swapped because we generate the flattened implementations, actually
    for (sharedImplemRef <- sharedImplementations){
      for(implem <- sharedImplemRef.sharedFlattenedImplementations){
        for (proc <- procs) {
          if (implem.canRunOn(proc)) {
            val dur = implem.duration(proc, properties)
            if (dur > maxDur) maxDur = dur
          }
        }
      }
    }

    maxDur
  }
}

object TransmissionTiming extends Enumeration {
  type TransmissionTiming = Value
  val Asap, Alap, Free, Sticky = Value
}

import placer.metadata.sw.TransmissionTiming._


//on peut avoir des flux vers les process atomiques, auquel cas, c'est additioné par process sur le même HW; on peutaussi avoir des communication vers le Groupe,
// auquel cas, chaque process a besoin de la totalité du flux; c'st donc partagé si deux process du groupe sont sur le même HW

//transmission uses the amount of memory on source until terminated, and uses amount of memory on destination until consuming task started
case class Transmission(source: AtomicTask,
                        target: AtomicTask,
                        size: Int,
                        timing: TransmissionTiming,
                        name: String) extends Indiced() {
  override def toString: String = "Transmission(" + name + " " + source.name + "->" + target.name + " size:" + size + " timing:" + timing + ")"

  def precedes(that:Transmission):Boolean = this.target == that.source

  def shortString: String = "Transmission(" + name + " " + source.name + "->" + target.name + ")"

  def toJSon: String = "{" +
    JSonHelper.string("name", name) + "," +
    JSonHelper.string("source", source.name) + "," +
    JSonHelper.string("target", target.name) + "," +
    JSonHelper.int("size", size) + "," +
    JSonHelper.string("timing", timing.toString) + "}"
}

object SoftwareClass extends Enumeration {
  type SoftwareClass = Value
  val OneShotSoftware, IterativeSoftware = Value
}

import placer.metadata.sw.SoftwareClass._

/**
  *
  * @param simpleProcesses the processes
  * @param transmissions the transmissions between processes
  * @param softwareClass the class of software, and its time requirements
  */
case class SoftwareModel(sharedPermanentFunctions:Array[ParametricImplementation],
                         simpleProcesses: Array[AtomicTask],
                         transmissions: Array[Transmission],
                         softwareClass: SoftwareClass,
                         properties:SortedMap[String,Int],
                         verbose:Boolean = true) extends IndiceMaker {
  setIndices(simpleProcesses)
  setIndices(transmissions)
  setIndices(sharedPermanentFunctions)

  val nbTransmissions = transmissions.length

  if(verbose) println("checking for cycles in software model...")
  checkCycle
  if(verbose) println("no cycle detected")

  require(transmissions.forall(f => f.source.id != -1 && f.target.id != -1), "some transmissions refer to non-registered tasks")

  override def toString: String = "SoftwareModel(\n" +
    "\tsharedPermanentFunctions:[\n\t\t" + sharedPermanentFunctions.map(_.toString).mkString("\n\t\t") + "] \n" +
    "\ttasks:[\n\t\t" + simpleProcesses.flatMap(_.multiString).mkString("\n\t\t") + "] \n" +
    "\ttransmissions:[\n\t\t" + transmissions.mkString("\n\t\t") + "\n" +
    "\t" + softwareClass + "])"

  def checkCycle{
    val transmissionsNoPredecessors = transmissions.filter(t1 => !transmissions.exists(t2 => t2 precedes t1))
    val isReached = Array.fill(nbTransmissions)(false)
    for(t <- transmissionsNoPredecessors){
      isReached(t.id) = true
      checkCycleFrom(t)
      isReached(t.id) = false
    }

    def checkCycleFrom(t:Transmission): Unit ={
      for(next <- transmissions if t precedes next){
        if(isReached(next.id)){
          System.err.println("cycle in software model starting at " + next.name)
          System.exit(-1)
        }
        isReached(next.id) = true
        checkCycleFrom(next)
        isReached(next.id) = false
      }
    }
  }
}
