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
import placerT.metadata.hw.{Bus, ProcessingElement, SelfLoopBus}
import placerT.metadata.sw.{AtomicTask, FlattenedImplementation, Transmission}

case class TaskMapping(task:AtomicTask,
                       pe:ProcessingElement,
                       implem:FlattenedImplementation,
                       s:Int,
                       d:Int,
                       e:Int,
                       subcores:List[Int] = Nil)

case class Mapping(timeUnit:String,
                   dataUnit:String,
                   hardwareName: String,
                   taskMapping: Array[TaskMapping],
                   transmissionMapping: Array[(Transmission, ProcessingElement, ProcessingElement, Bus, Int, Int, Int)],
                   makeSpan: Int,
                   energy: Int,
                   width:Option[Int]) {

  private def padToLength(s: String, l: Int) = (s + nStrings(l, " ")).substring(0, l)

  private def nStrings(n: Int, s: String): String = if (n <= 0) "" else s + nStrings(n - 1, s)


  def coreUsages:List[String] = {
    val coreAndTaskAndDuration = taskMapping.map({case TaskMapping(task:AtomicTask,pe:ProcessingElement,i,s,d,e,_) => (pe.name,task.name + "(dur:" + d + ")",d)}).toList
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

  override def toString: String = "Mapping(\n\t" +
    taskMapping.map(
      { case TaskMapping(task, pe, implem, start, dur, end,_) =>
        padToLength(task.name, 22) + "implem:" + padToLength(implem.description, 22) + " on:" + padToLength(pe.name, 15) + " start:" + padToLength("" + start, 4) + " dur:" + padToLength("" + dur, 4) + "end:" + padToLength("" + end, 4)
      }).mkString("\n\t") + "\n\t" +
    transmissionMapping.filter(p => p._2.id != p._3.id).map(
      { case (trans, fromPE, toPE, bus, start, dur, end) =>
        padToLength(trans.name, 21) + " from:" + padToLength(fromPE.name, 10) + " to:" + padToLength(toPE.name, 10) + " on:" + padToLength(bus.name, 15) + " start:" + padToLength("" + start, 4) + " dur:" + padToLength("" + dur, 4) + "end:" + padToLength("" + end, 4)
      }).mkString("\n\t") + "\n)"

  def toStringSorted: String = {
    val stringAndStart = taskMapping.map(
      { case TaskMapping(task, pe, implem, start, dur, end,_) =>
        (padToLength(task.name, 40) + "implem:" + padToLength(implem.description, 40) + " on:" + padToLength(pe.name, 15) + " start:" + padToLength("" + start, 4) + " dur:" + padToLength("" + dur, 4) + "end:" + padToLength("" + end, 4), start)
      }).toList ++
      transmissionMapping.filter(p => p._2.id != p._3.id).map(
        { case (trans, fromPE, toPE, bus, start, dur, end) =>
          (padToLength(trans.name, 31) + " from:" + padToLength(fromPE.name, 10) + " to:" + padToLength(toPE.name, 10) + " on:" + padToLength(bus.name, 15) + " start:" + padToLength("" + start, 4) + " dur:" + padToLength("" + dur, 4) + "end:" + padToLength("" + end, 4), start)
        })
    "Mapping(hardwareName:" + hardwareName + " makeSpan:" + makeSpan + " width:" + width + " energy:" + energy + "){\n\t" + stringAndStart.sortBy(_._2).map(_._1).mkString("\n\t") + "\n}"
  }

  def toStringSortedLight: String = {
    val stringAndStart = taskMapping.map(
      { case TaskMapping(task, pe, implem, start, dur, end,_) =>
        (padToLength(task.name, 60) + " " + padToLength(pe.name + "(" + implem.description + ")", 45) + " start:" + padToLength("" + start, 10) + " dur:" + padToLength("" + dur, 10) + "end:" + padToLength("" + end, 10), start)
      }).toList ++
      transmissionMapping.filter(p => p._2.id != p._3.id).map(
        { case (trans, fromPE, toPE, bus, start, dur, end) =>
          (padToLength(trans.name, 60) + " " + padToLength(bus.name, 45) + " start:" + padToLength("" + start, 10) + " dur:" + padToLength("" + dur, 10) + "end:" + padToLength("" + end, 10), start)
        })
    "Mapping(timeUnit:" + timeUnit + " dataUnit:" + dataUnit + " hardwareName:" + hardwareName + " makeSpan:" + makeSpan + " width:" + width + " energy:" + energy + "){\n\t" + stringAndStart.sortBy(_._2).map(_._1).mkString("\n\t") + "\n}"
  }

  def usages:String = "usages{" + coreUsages.mkString("\n\t","\n\t","") + busUsages.mkString("\n\t","\n\t","\n") + "}"

  def toJSon: String = "{" +
    JSonHelper.string("timeUnit", timeUnit) + "," +
    JSonHelper.string("dataUnit", dataUnit) + "," +
    JSonHelper.string("hardwareName", hardwareName) + "," +
    JSonHelper.int("makeSpan", makeSpan) + "," +
    JSonHelper.optionIntComa("width", width) +
    JSonHelper.int("energy", energy) + "," +
    JSonHelper.multiples("taskMapping", taskMapping.map(taskMappingToJSon)) + "," +
    JSonHelper.multiples("transmissionMapping", transmissionMapping.filter(p => p._2.id != p._3.id).map(transmissionMappingToJSon)) + "}"

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
      sdetoJSon(m._5, m._6, m._7) + "}"
  }

  private def sdetoJSon(s: Int, d: Int, e: Int): String = {
    JSonHelper.int("start", s) + "," +
      JSonHelper.int("duration", d) + "," +
      JSonHelper.int("end", e)
  }
}

case class Mappings(mapping: Iterable[Mapping]) {
  def toJSon: String = {
    "{" + JSonHelper.multiples("mappings", mapping.map(_.toJSon)) + "}"
  }

  override def toString: String = {
    "Mappings(nbMapping:" + mapping.size + "\n" +
      mapping.map(l => l.toStringSortedLight + "\n" + l.usages).mkString("\n\n") + "\n)"
  }

}
