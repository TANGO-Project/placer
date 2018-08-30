package placer.metadata

import placer.metadata.hw.{HardwareModel, ProcessingElement}
import placer.metadata.sw.{AtomicTask, Implementation}

import scala.collection.immutable.SortedSet

class ConstraintList(val cl:List[MappingConstraint]){
  def isWidthNeeded = cl.exists(_.needsWidth)

  val objective:Option[MappingObjective] = {
    val allObjective = cl.flatMap(_ match{case o:MappingObjective => Some(o); case _ => None})
    require(allObjective.size <= 1,
      "you cannot have more than one objective. For multi-criterion, please use the Pareto construct. Declared objectives: " + allObjective.mkString(","))
    if(allObjective.size == 1) Some(allObjective.head) else None
  }

  override def toString: String = "{\n\t" + cl.mkString(",\n\t") + "}"

  lazy val maxMakeSpanOpt:Option[Int] ={
    var toReturn = Int.MaxValue
    for(c <- cl){
      c match{
        case MaxMakespan(maxMakeSpan) => toReturn = toReturn min maxMakeSpan
        case _ => ;
      }
    }
    if(toReturn == Int.MaxValue) None else Some(toReturn)
  }
}

abstract sealed class MappingConstraint{
  def needsWidth:Boolean = false
  def isMappingObjective:Boolean = false
}

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

  lazy val idOfTasks:SortedSet[Int] = SortedSet.empty[Int] ++ processes.map(_.id)
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

case class SymmetricTasksConstraint(tasks:List[AtomicTask]) extends MappingConstraint

object SymmetricPEConstraintType extends Enumeration {
  val Workload,LongTask = Value
}

case class PowerCap(maxPower:Int) extends MappingConstraint
case class EnergyCap(maxEnergy:Int) extends MappingConstraint
case class MaxMakespan(maxMakeSpan:Int) extends MappingConstraint
case class WidthCap(maxDelay:Int) extends MappingConstraint{
  override def needsWidth: Boolean = true
}


//software model is shared, so no need for specifying it
//case class RestrictImplementations(task:AtomicTask,implementations:List[Implementation]) extends MappingConstraint
//software model is shared, so no need for specifying it
//case class RestrictParameter(task:AtomicTask,implementation:Implementation,parameter:String,value:List[Int]) extends MappingConstraint

// restricts the possible value of a parameter to a specified su
case class ForbidHardwarePlatform(forbiddenHardwarePlatforms:SortedSet[String]) extends MappingConstraint
//case class RestrictHardwarePlatform(acceptedPlatforms:List[HardwareModel]) extends MappingConstraint

sealed abstract class MappingObjective extends MappingConstraint {
  override def isMappingObjective:Boolean = true
}

sealed abstract class SimpleMappingGoal extends MappingObjective{
}

case class MinEnergy() extends SimpleMappingGoal{
}
case class MinMakeSpan() extends SimpleMappingGoal{
}
case class MinFrame() extends SimpleMappingGoal{
  override def needsWidth:Boolean = true
}

case class MinPareto(a:SimpleMappingGoal,b:SimpleMappingGoal) extends MappingObjective{

  require(a ne b,"cannot define multi objective with the same basic objective twice:" + a)

  override def needsWidth:Boolean = a.needsWidth || b.needsWidth
}

