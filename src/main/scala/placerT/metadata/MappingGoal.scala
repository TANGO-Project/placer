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

/*
package placerT.metadata

import placerT.io.JSonHelper


sealed abstract class MappingGoal{
  def needsWidth:Boolean
  def toJSon:String
}

case class Sat() extends MappingGoal{
  override def needsWidth:Boolean = false
  final override def toJSon: String = "sat"
}

sealed abstract class SimpleMappingGoal extends MappingGoal{
  override def needsWidth:Boolean = false
  final override def toJSon: String = "{" + JSonHelper.string("singleObjective",simpleJSonName) + "}"
  def simpleJSonName:String
}

case class MinEnergy() extends SimpleMappingGoal{
  override def simpleJSonName: String = "minEnergy"
}
case class MinMakeSpan() extends SimpleMappingGoal {
  override def simpleJSonName: String = "minMakeSpan"
}
case class MinFrame() extends SimpleMappingGoal{
  override def needsWidth:Boolean = true
  override def simpleJSonName: String = "minFrame"
}
case class Pareto(a:SimpleMappingGoal,b:SimpleMappingGoal) extends MappingGoal{

  require(a ne b,"cannot define multi objective twice the same basic objective:" + a)

  override def needsWidth:Boolean = a.needsWidth || b.needsWidth

  override def toJSon: String = "{" +
    JSonHelper.complex("multiObjective","{" +
      JSonHelper.string("a",a.simpleJSonName) + "," +
      JSonHelper.string("b",b.simpleJSonName) + "}") + "}"
}

*/