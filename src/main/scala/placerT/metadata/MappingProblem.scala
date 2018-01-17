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


package placerT.metadata

import placerT.io.JSonHelper
import placerT.metadata.hw.{HardwareModel, ProcessingElement, ProcessingElementClass}
import placerT.metadata.sw.{AtomicTask, SoftwareModel}

import scala.collection.immutable.SortedMap

/**
 * defines a mapping problem
 * @param softwareModel the model of the sw
 * @param hardwareModel the model of the hw (will evolve to list of hardware models)
 * @param goal the objective of the mapping
 */
case class MappingProblem(timeUnit:String,
                          dataUnit:String,
                          info:String,
                          properties:SortedMap[String,Int],
                          processorClasses: Array[ProcessingElementClass],
                          softwareModel: SoftwareModel,
                          hardwareModels: List[HardwareModel],
                          constraints:List[MappingConstraint],
                          goal: MappingGoal) {

  def toJSon: String = "{" +
    JSonHelper.string("timeUnit",timeUnit) + "," +
    JSonHelper.string("dataUnit",dataUnit) + "," +
    JSonHelper.complex("softwareModel", softwareModel.toJSon) + "," +
    JSonHelper.complex("hardwareModel", hardwareModel.toJSon) + "," +
    JSonHelper.complex("goal", goal.toJSon) + "}"
}

abstract sealed class MappingConstraint
case class RunOnConstraint(processor:ProcessingElement,
                             process:AtomicTask,
                             value:Boolean) extends MappingConstraint{
  override def toString: String = {
    (if (value) "MustRunOn(" else "MustNotRunOn(") + process.name + "," + processor.name + ")"
  }
}
case class CoreSharingConstraint(processes:List[AtomicTask],
                                 value:Boolean) extends MappingConstraint{
  override def toString: String = {
    (if (value) "SameCore(" else "DifferentCores(") + processes.map(_.name) + ")"
  }
}
case class MustBeUsedConstraint(processor:ProcessingElement,value:Boolean) extends MappingConstraint {
  override def toString: String = (if(value) "MustBeUsed(" else "MustNotBeUsed(") + processor.name + ")"
}

case class SymmetricPEConstraint(processors:List[ProcessingElement],breaking:SymmetricPEConstraintType.Value = SymmetricPEConstraintType.Workload) extends MappingConstraint {

  require(processors.size > 1,"SymmetricPEConstraint cannot be specified with fewer that two processing elements")
  val witnessPE = processors.head
  for(p <- processors.tail){
    require(witnessPE symmetricTo p, "different processing elements specified in SymmetricPEConstraint:" + witnessPE.name + " and " + p.name)
  }

  override def toString: String = "SymmetricPEConstraint(" + processors.map(_.name) + ")"
}

object SymmetricPEConstraintType extends Enumeration {
  val Workload,LongTask = Value
}