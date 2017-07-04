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
import placerT.metadata.hw.{Bus, ProcessingElement}
import placerT.metadata.sw.{AtomicTask, FlattenedImplementation, Transmission}


case class Mapping(hardwareName: String,
                   taskMapping: Array[(AtomicTask, ProcessingElement, FlattenedImplementation, Int, Int, Int)],
                   transmissionMapping: Array[(Transmission, ProcessingElement, ProcessingElement, Bus, Int, Int, Int)],
                   makeSpan: Int,
                   energy: Int,
                   width:Option[Int]) {

  private def padToLength(s: String, l: Int) = (s + nStrings(l, " ")).substring(0, l)

  private def nStrings(n: Int, s: String): String = if (n <= 0) "" else s + nStrings(n - 1, s)


  override def toString: String = "Mapping(\n\t" +
    taskMapping.map(
    { case (task, pe, implem, start, dur, end) =>
      padToLength(task.name, 22) + "implem:" + padToLength(implem.description, 22) + " on:" + padToLength(pe.name, 15) + " start:" + padToLength("" + start, 4) + " dur:" + padToLength("" + dur, 4) + "end:" + padToLength("" + end, 4)
    }).mkString("\n\t") + "\n\t" +
    transmissionMapping.map(
    { case (trans, fromPE, toPE, bus, start, dur, end) =>
      padToLength(trans.name, 21) + " from:" + padToLength(fromPE.name, 10) + " to:" + padToLength(toPE.name, 10) + " on:" + padToLength(bus.name, 15) + " start:" + padToLength("" + start, 4) + " dur:" + padToLength("" + dur, 4) + "end:" + padToLength("" + end, 4)
    }).mkString("\n\t") + "\n)"

  def toStringSorted: String = {
    val stringAndStart = taskMapping.map(
    { case (task, pe, implem, start, dur, end) =>
      (padToLength(task.name, 22) + "implem:" + padToLength(implem.description, 22) + " on:" + padToLength(pe.name, 15) + " start:" + padToLength("" + start, 4) + " dur:" + padToLength("" + dur, 4) + "end:" + padToLength("" + end, 4), start)
    }).toList ++
      transmissionMapping.map(
      { case (trans, fromPE, toPE, bus, start, dur, end) =>
        (padToLength(trans.name, 21) + " from:" + padToLength(fromPE.name, 10) + " to:" + padToLength(toPE.name, 10) + " on:" + padToLength(bus.name, 15) + " start:" + padToLength("" + start, 4) + " dur:" + padToLength("" + dur, 4) + "end:" + padToLength("" + end, 4), start)
      })
    "Mapping(hardwareName:" + hardwareName + " makeSpan:" + makeSpan + " width:" + width + " energy:" + energy + "){\n\t" + stringAndStart.sortBy(_._2).map(_._1).mkString("\n\t") + "\n}"
  }

  def toStringSortedLight: String = {
    val stringAndStart = taskMapping.map(
    { case (task, pe, implem, start, dur, end) =>
      (padToLength(task.name, 22) + " " + padToLength(pe.name + "(" + implem.description + ")", 25) + " start:" + padToLength("" + start, 4) + " dur:" + padToLength("" + dur, 4) + "end:" + padToLength("" + end, 4), start)
    }).toList ++
      transmissionMapping.map(
      { case (trans, fromPE, toPE, bus, start, dur, end) =>
        (padToLength(trans.name, 22) + " " + padToLength(bus.name, 25) + " start:" + padToLength("" + start, 4) + " dur:" + padToLength("" + dur, 4) + "end:" + padToLength("" + end, 4), start)
      })
    "Mapping(hardwareName:" + hardwareName + " makeSpan:" + makeSpan + " width:" + width + " energy:" + energy + "){\n\t" + stringAndStart.sortBy(_._2).map(_._1).mkString("\n\t") + "\n}"
  }

  def toJSon: String = "{" +
    JSonHelper.string("hardwareName", hardwareName) + "," +
    JSonHelper.int("makeSpan", makeSpan) + "," +
    JSonHelper.optionIntComa("width", width) +
    JSonHelper.int("energy", energy) + "," +
    JSonHelper.multiples("taskMapping", taskMapping.map(taskMappingToJSon)) + "," +
    JSonHelper.multiples("transmissionMapping", transmissionMapping.map(transmissionMappingToJSon)) + "}"

  private def taskMappingToJSon(m: (AtomicTask, ProcessingElement, FlattenedImplementation, Int, Int, Int)): String = {
    "{" + JSonHelper.string("task", m._1.name) + "," +
      JSonHelper.string("processingElement", m._2.name) + "," +
      JSonHelper.string("implementation", m._3.name) +
      JSonHelper.multiples("parameters", m._3.parameterValues.map({ case (name, value) => "{" + JSonHelper.string("name", name) + "," + JSonHelper.int("value", value) + "}" })) + "," +
      sdetoJSon(m._4, m._5, m._6) + "}"
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
      mapping.map(_.toStringSortedLight).mkString("\n") + "\n)"
  }
}
