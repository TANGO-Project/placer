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


package placerT

import net.liftweb.json._
import placerT.algo.{Mapper, MapperConfig}
import placerT.io.Extractor
import placerT.metadata.hw._
import placerT.metadata.sw.TransmissionTiming._
import placerT.metadata.sw._
import placerT.metadata._

import scala.collection.immutable.{SortedMap, SortedSet}

object Example2 extends App {

  val problem = Example2Data.problem
  println(problem)

  val mappingSet = Mapper.findMapping(problem,MapperConfig())

  println(mappingSet)

}

object GenerateJSon extends App {

  val problem = Example2Data.problem
  println(problem)
  println(problem.toJSon)
  val json = prettyRender(parse(problem.toJSon))

  println(json)

  val parsed = parse(json)
  val extracted = Extractor.extractProblem(parsed,"example2.scala")
  println(extracted)
}

object Example2Data {

  //hardware metamodel
  val cpu = MonoTaskSwitchingTask("cpu", SortedSet.empty, SortedSet("mflops", "mips"), 1)
  val fpga = MultiTaskPermanentTasks("fpga", SortedSet("kgate", "multiplier"), SortedSet("frequency"))
  val gpgpu = MonoTaskSwitchingTask("gpgpu", SortedSet("core"), SortedSet.empty, 1)

  //hardware model
  val processorA = ProcessingElement(cpu,
    SortedMap.empty, //resources
    SortedMap("mflops" -> 10, "mips" -> 1000), //properties
    "procA",
    100,
    powerModel = Dim("buzy") * Dim("mflops") * 30 + 5)
  val processorB = ProcessingElement(gpgpu,
    SortedMap("core" -> 110), //resources
    SortedMap.empty, //properties
    "GPGPU1",
    200,
    powerModel = Dim("core") * 5 + 10)
  val processorC = ProcessingElement(cpu,
    SortedMap.empty, //resources
    SortedMap("mflops" -> 6, "mips" -> 100), //properties
    "procB",
    100,
    powerModel = Dim("buzy") * Dim("mflops") * 20 + 1)
  val processorD = ProcessingElement(fpga,
    SortedMap("kgate" -> 110, "multiplier" -> 500), //resources
    SortedMap("frequency" -> 100), //properties
    "FPGA1",
    100,
    powerModel = Dim("frequency") * Dim("frequency") * (Dim("kgate") + Dim("multiplier") * 20) / 1000 + 2)

  val globalBus = HalfDuplexBus(
    List(processorA, processorB, processorC, processorD),
    3,
    1,
    "globalBus")
  val globalBus2 = HalfDuplexBus(
    List(processorA, processorB, processorC, processorD),
    4,
    1,
    "globalBus2")
  val busAB = HalfDuplexBus(
    List(processorA, processorB),
    1,
    1,
    "busAtoGPGPU")
  val busBC = HalfDuplexBus(
    List(processorB, processorC),
    2,
    1,
    "busBToGPGPU")
  val busAD = HalfDuplexBus(
    List(processorA, processorD),
    1,
    3,
    "busAToFPGA")

  val hardwareModel = HardwareModel("ExampleHardware2",
    Array(cpu, gpgpu, fpga),
    Array(processorA, processorB, processorC, processorD),
    Array(globalBus, busAB, busBC, busAD, globalBus2),
    SortedMap("hardwareSize" -> 4),
    powerCap = Some(700),
    energyCap = Some(500000))


  // software model
  val inputting = AtomicTask(
    List(
      FlattenedImplementation("cpu_standard", cpu, SortedMap.empty, 10, 100 / Dim("mflops") + 1000 / Dim("mips")).toParam,
      FlattenedImplementation("fpga_standard", fpga, SortedMap("kgate" -> 100, "multiplier" -> 30), 100, 500 / Dim("frequency")).toParam),
    "inputting")
  val decoding = AtomicTask(
    List(
      FlattenedImplementation("PI_cpu_supplier1", cpu, SortedMap.empty, 100, 10 / Dim("mflops") + 100000 / Dim("mips")).toParam,
      FlattenedImplementation("PI_cpu_supplier2", cpu, SortedMap.empty, 10, 100 / Dim("mflops") + 1000 / Dim("mips")).toParam,
      FlattenedImplementation("fpga_standard", fpga, SortedMap("kgate" -> 100, "multiplier" -> 20), 10, 1000 / Dim("frequency")).toParam,
      FlattenedImplementation("gpgpu_standard", gpgpu, SortedMap("core" -> 100), 10, 10).toParam),
    "decoding")
  val transforming = AtomicTask(List(
    FlattenedImplementation("cpu_standard", cpu, SortedMap.empty, 10, 110 / Dim("mflops") + 1000 / Dim("mips")).toParam,
    FlattenedImplementation("gpgpu_standard", gpgpu, SortedMap("core" -> 1), 10, 10).toParam),
    "Transforming")
  val transforming2 = AtomicTask(List(
    FlattenedImplementation("cpu_standard", cpu, SortedMap.empty, 10, 110 / Dim("mflops") + 1000 / Dim("mips")).toParam,
    FlattenedImplementation("gpgpu_standard", gpgpu, SortedMap("core" -> 1), 10, 10).toParam),
    "Transforming2")
  val watermarking = AtomicTask(List(
    FlattenedImplementation("cpu_standard", cpu, SortedMap.empty, 0, 100 / Dim("mflops") + 1000 / Dim("mips")).toParam,
    FlattenedImplementation("PI_fpga_supplier1", fpga, SortedMap("kgate" -> 20, "multiplier" -> 30), 10, 100 / Dim("frequency")).toParam,
    FlattenedImplementation("PI_fpga_supplier2", fpga, SortedMap("kgate" -> 10, "multiplier" -> 35), 10, 101 / Dim("frequency")).toParam,
    FlattenedImplementation("gpgpu_standard", gpgpu, SortedMap("core" -> 100), 10, 10).toParam),
    "watermarking")
  val encoding = AtomicTask(
    List(FlattenedImplementation("cpu_standard", cpu, SortedMap.empty, 10, 100 + 100 / Dim("mflops") + 1000 / Dim("mips")).toParam,
      ParametricImplementation("cpu_param", cpu, SortedMap.empty, 5 * (Dim("toto") + 1), 100 + 100 / Dim("mflops") + 1000 / Dim("mips") - 10 * Dim("toto"), SortedMap("toto" -> List(0, 1, 2)))),
    "encoding")

  val inputToDecode = Transmission(inputting, decoding, 50, timing = Free, "inputToDecode")
  val decodeToTransform = Transmission(decoding, transforming, 2, timing = Asap, "decodeToTransform")
  val transformToWatermark = Transmission(transforming, watermarking, 2, timing = Asap, "transformToWatermark")
  val decodeToTransform2 = Transmission(decoding, transforming2, 2, timing = Asap, "decodeToTransform2")
  val transform2ToWatermark = Transmission(transforming2, watermarking, 2, timing = Asap, "transform2ToWatermark")
  val watermarkToEncode = Transmission(watermarking, encoding, 20, timing = Asap, "watermarkToEncode")
  val sideComm = Transmission(inputting, encoding, 5, timing = Free, "side_comm")

  val softwareModel = SoftwareModel(
    Array(inputting, decoding, transforming, transforming2, watermarking, encoding),
    Array(inputToDecode, decodeToTransform, transformToWatermark, decodeToTransform2, transform2ToWatermark, watermarkToEncode, sideComm),
    OneShotSoftware(Some(20000)),
    SortedMap.empty[String,Int]) //IterativeSoftware(maxMakespan = None,maxFrameDelay = None /*Some(500)*/)) //OneShotSoftware(Some(20000)))

  val goal = Pareto(MinMakeSpan(),MinEnergy()) //ParetoMakeSpanEnergy //MinMakeSpan() //MinEnergy() //ParetoMakeSpanEnergy() //ParetoMakeSpanEnergy() //() // // MinEnergy() //MinMakeSpan()

  val problem = new MappingProblem("ms","bit","Example2.scala",SortedMap.empty[String,Int],hardwareModel.processorClasses,softwareModel, hardwareModel, List.empty, goal)

}