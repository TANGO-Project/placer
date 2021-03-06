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

import oscar.cp.core.CPSol
import placer.io.JSonHelper
import placer.metadata.hw.{Bus, ProcessingElement, SelfLoopBus}
import placer.metadata.sw.{AtomicTask, FlattenedImplementation, Transmission}

case class TaskMapping(task:AtomicTask,
                       pe:ProcessingElement,
                       implem:FlattenedImplementation,
                       s:Int,
                       d:Int,
                       e:Int)

case class SharedFunctionMapping(implem:FlattenedImplementation,
                                 pe:ProcessingElement,
                                 nbInstances:Int
                                ){
  require(nbInstances>0)
}

case class Mapping(hardwareName: String,
                   sharedFunctionMapping:Iterable[SharedFunctionMapping],
                   taskMapping: Array[TaskMapping],
                   transmissionMapping: Array[(Transmission, ProcessingElement, ProcessingElement, Bus, Int, Int, Int)],
                   makespan: Int,
                   energy: Int,
                   width:Option[Int],
                   objValues:List[Int],
                   cpSol:CPSol) {

  private def padToLength(s: String, l: Int) = (s + nStrings(l, " ")).substring(0, l)

  private def nStrings(n: Int, s: String): String = if (n <= 0) "" else s + nStrings(n - 1, s)

  def coreUsages:List[String] = {
    val coreAndTaskAndDuration = taskMapping.map({case TaskMapping(task:AtomicTask,pe:ProcessingElement,i,s,d,e) => (pe.name,task.name + "(dur:" + d + ")",d)}).toList
    val coreToTask = coreAndTaskAndDuration.groupBy(_._1).toList.sortBy(_._1)
    coreToTask.map({case (core,tasks) => "" + core +":" + tasks.map(_._3).sum + ":" + tasks.map(_._2).mkString(",")}).toList
  }

  def busUsages:List[String] = {
    val busAndDuration = transmissionMapping.toList.flatMap{
      {case (trans,pr1,pe2,bus,s,d,e) =>
        bus match{
          case s:SelfLoopBus => None
          case b:Bus => Some(b.name,d)
        }
      }
    }
    val busToDurations = busAndDuration.groupBy(_._1).toList.sortBy(_._1)
    busToDurations.map({case(bus,durs) => "" + bus + ":" + durs.map(_._2).sum }).toList
  }

  override def toString: String = {
    val stringAndStart = taskMapping.map(
      { case TaskMapping(task, pe, implem, start, dur, end) =>
        (padToLength(task.name, 60) + " " + padToLength(pe.name + "(" + implem.description + ")", 60) + " start:" + padToLength("" + start, 10) + " dur:" + padToLength("" + dur, 10) + "end:" + padToLength("" + end, 10), start)
      }).toList ++
      transmissionMapping.filter(p => p._2.id != p._3.id).map(
        { case (trans, fromPE, toPE, bus, start, dur, end) =>
          (padToLength(trans.name, 60) + " " + padToLength(bus.name, 60) + " start:" + padToLength("" + start, 10) + " dur:" + padToLength("" + dur, 10) + "end:" + padToLength("" + end, 10), start)
        })

    val usages = coreUsages.mkString("\n\t\t","\n\t\t","") +
      (if(busUsages.nonEmpty) {busUsages.mkString("\n\t\t","\n\t\t","\n\t")} else "\n\t")

    "Mapping(hardwareName:" + hardwareName + " makeSpan:" + makespan + " width:" + width + " energy:" + energy + ")" +
      "{\n\tsharedFunctions{\n\t\t" + sharedFunctionMapping.map(s => "(implem:" + s.implem.name +
      "(" + s.implem.parameterValues.map({ case (name, value) => name + ":" + value}).mkString(",") + ")" +
      " pe:" + s.pe.name + " nbInstances:" + s.nbInstances + ")").mkString("\n\t\t") + "\n\t}" +
      "\n\tschedule{\n\t\t" + stringAndStart.sortBy(_._2).map(_._1).mkString("\n\t\t") + "\n\t}" +
      "\n\tusages{" + usages + "}\n}"
  }

  def toJSon: String = "{" +
    JSonHelper.string("hardwareName", hardwareName) + "," +
    JSonHelper.int("makespan", makespan) + "," +
    JSonHelper.optionIntComa("width", width) +
    JSonHelper.int("energy", energy) + "," +
    JSonHelper.multiples("sharedFunctions", sharedFunctionMapping.map(sharedFunctionToJSon)) + "," +
    JSonHelper.multiples("taskMapping", taskMapping.map(taskMappingToJSon)) + "," +
    JSonHelper.multiples("transmissionMapping", transmissionMapping.filter(p => p._2.id != p._3.id).map(transmissionMappingToJSon)) + "}"

  private def sharedFunctionToJSon(s:SharedFunctionMapping):String = {
    "{" + JSonHelper.string("sharedImplem",s.implem.name) + "," +
      JSonHelper.multiples("parameters", s.implem.parameterValues.map({ case (name, value) => "{" + JSonHelper.string("name", name) + "," + JSonHelper.int("value", value) + "}" })) + "," +
      JSonHelper.int("nbInstance",s.nbInstances) + "," +
      JSonHelper.string("host",s.pe.name) + "}"
  }

  private def taskMappingToJSon(m:TaskMapping): String = {
    "{" + JSonHelper.string("task", m.task.name) + "," +
      JSonHelper.string("processingElement", m.pe.name) + "," +
      JSonHelper.string("implementation", m.implem.name) +
      JSonHelper.multiples("parameters", m.implem.parameterValues.map({ case (name, value) => "{" + JSonHelper.string("name", name) + "," + JSonHelper.int("value", value) + "}" })) + "," +
      sdetoJSon(m.s, m.d, m.e) + "}"
  }

  private def transmissionMappingToJSon(m: (Transmission, ProcessingElement, ProcessingElement, Bus, Int, Int, Int)): String = {
    "{" +
      JSonHelper.string("transmission", m._1.name) + "," +
      JSonHelper.string("bus", m._4.name) + "," +
      JSonHelper.string("fromPE", m._2.name) + "," +
      JSonHelper.string("toPE", m._3.name) + "," +
      JSonHelper.string("fromTask", m._1.source.name) + "," +
      JSonHelper.string("toTask", m._1.target.name) + "," +
      sdetoJSon(m._5, m._6, m._7) + "}"
  }

  private def sdetoJSon(s: Int, d: Int, e: Int): String = {
    JSonHelper.int("start", s) + "," +
      JSonHelper.int("duration", d) + "," +
      JSonHelper.int("end", e)
  }
}

case class Mappings(timeUnit:String,
                    dataUnit:String,
                    energyUnit:Option[String],
                    info:String,
                    mapping: Iterable[Mapping]) {

  def toJSon(additionalFields:String = ""): String = {
    "{" +
      JSonHelper.string("jsonFormat","PlacerBeta6Out") + "," +
      JSonHelper.string("timeUnit",timeUnit) + "," +
      JSonHelper.string("dataUnit",dataUnit) + "," +
      (energyUnit match{case None => ""; case Some(e) => JSonHelper.string("energyUnit", e) + ","}) +
      JSonHelper.string("info",info) + "," +
      additionalFields +
      JSonHelper.multiples("mappings", mapping.map(_.toJSon)) + "}"
  }

  override def toString: String = {
    "Mappings(" +
      "\n\tnbMapping:" + mapping.size +
      "\n\tinfo:" + info +
      "\n\ttimeUnit:" + timeUnit +
      "\n\tdataUnit:" + dataUnit +
      (energyUnit match{case None => ""; case Some(e) => "\n\tenergyUnit:" + e + "\n"}) +
      mapping.map(l => l.toString).mkString("\n\n") + "\n)"
  }
}
