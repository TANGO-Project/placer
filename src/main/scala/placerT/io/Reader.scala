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


package placerT.io

import net.liftweb.json._


object Reader extends App {

  val parsed = parse( """ { "numbers" : [1, 2, 3, 4] } """)

  println(parsed)

  println(compactRender(parsed))

  println(JSonHelper.int("a", 5))
  println(JSonHelper.string("attribute", "stringValue"))
  println(JSonHelper.strings("attribute", List("strValue1", "strValue2")))

}

object JSonHelper {
  def strings(attribute: String, strings: Iterable[String]): String = {
    """"""" + attribute + """":[""" + strings.map( """"""" + _ + """"""").mkString(",") + "]"
  }

  def multiples(attribute: String, strings: Iterable[String]): String = {
    """"""" + attribute + """":[""" + strings.mkString(",") + "]"
  }

  def int(attribute: String, value: Int): String = {
    """"""" + attribute + """":""" + value
  }

  def intOpt(attribute: String, value: Option[Int],default:Int): String = {
    """"""" + attribute + """":""" + value.getOrElse(default)
  }


  def optionIntComa(name:String,value:Option[Int]):String = {
    value match{case None => "" case Some(v) => int(name,v) + ","}
  }

  def string(attribute: String, value: String): String = {
    """"""" + attribute + """":"""" + value + """""""
  }

  def complex(attribute: String, value: String): String = {
    """"""" + attribute + """":""" + value
  }

  def nameIntValue(name: String, value: Int): String =
    "{" + JSonHelper.string("name", name) + "," + JSonHelper.int("value", value) + "}"
}
