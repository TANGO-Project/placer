package placerT.metadata

import placerT.io.JSonHelper


sealed abstract class MappingGoal{
  def needsWidth:Boolean
  def toJSon:String
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
  override def needsWidth:Boolean = a.needsWidth || b.needsWidth


  override def toJSon: String = "{" +
    JSonHelper.complex("multiObjective","{" +
      JSonHelper.string("a",a.simpleJSonName) + "," +
      JSonHelper.string("b",b.simpleJSonName) + "}") + "}"
}