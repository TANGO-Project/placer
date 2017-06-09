package placerT.algo.sw

import oscar.cp
import oscar.cp._
import oscar.cp.core.variables.CPIntVar
import placerT.algo.Mapper
import placerT.algo.hw.{CPMultiTaskProcessor, CPProcessor}
import placerT.metadata.Formula
import placerT.metadata.sw.{TaskSet, FlattenedImplementation, AtomicTask}

import scala.collection.immutable.SortedMap

case class CPTaskSet(taskSet:TaskSet,
                     explanation: String,
                     mapper: Mapper,
                     maxHorizon: Int)
  extends CPAbstractTask(mapper) {
  implicit val solver = mapper.solver

  val numberOfTasks = taskSet.numberOfTasks

  val start: CPIntVar = CPIntVar(0, maxHorizon)
  val end: CPIntVar = CPIntVar(0, maxHorizon)
  override def duration: cp.CPIntVar = end - start


  val subTasks: Array[CPTask] = Array.tabulate(numberOfTasks)(
    id => new CPTask(taskSet.task, taskSet.task.name + "_" + id, mapper, maxHorizon)
  )






  //this is for multiTask processors.
  override def buildArrayImplemAndMetricUsage(target: CPMultiTaskProcessor): Option[(Array[cp.CPIntVar], SortedMap[String, Array[Int]])] = ???

  override def variablesToDistribute: Iterable[cp.CPIntVar] = ???

  override def couldBeExecutingOnProcessor(procID: Int): Boolean = ???

}
