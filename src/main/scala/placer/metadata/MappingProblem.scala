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


package placer.metadata

import placer.metadata.hw.{HardwareModel, ProcessingElementClass}
import placer.metadata.sw.SoftwareModel

import scala.collection.immutable.SortedMap

/**
  * defines a mapping problem
  * @param softwareModel the model of the sw
  * @param hardwareModels the model of the hw (will evolve to list of hardware models)
  */
case class MappingProblem(timeUnit:String,
                          dataUnit:String,
                          energyUnit:Option[String],
                          info:String,
                          properties:SortedMap[String,Int],
                          processorClasses: Array[ProcessingElementClass],
                          softwareModel: SoftwareModel,
                          hardwareModels: List[HardwareModel],
                          constraints:ConstraintList){
  def flattenToMonoHardwareProblems:List[MappingProblemMonoHardware] = {
    hardwareModels.map(
      hardwareModel =>
        MappingProblemMonoHardware(timeUnit:String,
          dataUnit:String,
          energyUnit,
          info:String,
          properties:SortedMap[String,Int],
          processorClasses: Array[ProcessingElementClass],
          softwareModel: SoftwareModel,
          hardwareModel: HardwareModel,
          constraints)
    )
  }
}

case class MappingProblemMonoHardware(timeUnit:String,
                                      dataUnit:String,
                                      energyUnit:Option[String],
                                      info:String,
                                      properties:SortedMap[String,Int],
                                      processorClasses: Array[ProcessingElementClass],
                                      softwareModel: SoftwareModel,
                                      hardwareModel: HardwareModel,
                                      constraints:ConstraintList)
