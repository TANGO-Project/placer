/*
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
 * You should have received a copy of the GNU Lesser General Public License along with OscaR.
 * If not, see http://www.gnu.org/licenses/lgpl-3.0.en.html
 *
 */

/*
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
 * You should have received a copy of the GNU Lesser General Public License along with OscaR.
 * If not, see http://www.gnu.org/licenses/lgpl-3.0.en.html
 *
 */

/*
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
 * You should have received a copy of the GNU Lesser General Public License along with OscaR.
 * If not, see http://www.gnu.org/licenses/lgpl-3.0.en.html
 *
 */

package main.scala.placerT.algo

import main.scala.placerT.algo.hw._
import main.scala.placerT.algo.sw.{CPTask, CPTransmission}
import main.scala.placerT.metadata.MappingGoal.{MappingGoal, _}
import main.scala.placerT.metadata._
import main.scala.placerT.metadata.hw._
import main.scala.placerT.metadata.sw._
import oscar.cp._
import oscar.cp.core.variables.CPIntVar
import oscar.cp.modeling.Constraints
import placerT.metadata.MappingProblem

object Mapper {
  def findMapping(softwareModel: SoftwareModel, hardwareModel: HardwareModel, goal: MappingGoal): Iterable[Mapping] = {
    new Mapper(softwareModel, hardwareModel, goal: MappingGoal).mapping
  }

  def findMapping(problem: MappingProblem): Mappings = {
    Mappings(new Mapper(problem.softwareModel, problem.hardwareModel, problem.goal: MappingGoal).mapping)
  }
}

class Mapper(val softwareModel: SoftwareModel, val hardwareModel: HardwareModel, goal: MappingGoal) extends CPModel with Constraints {

  val cpProblem = postProblem(softwareModel, hardwareModel)
  val mapping = searchMappingProblem(cpProblem, goal)

  def postProblem(softwareModel: SoftwareModel,
                  hardwareModel: HardwareModel): CPMappingProblem = {

    val summedMaxTaskDurations =
      softwareModel.simpleProcesses.map(_.maxDuration(hardwareModel.processors, hardwareModel.properties)).sum
    val summedMaxTransmissionTimes =
      softwareModel.transmissions.map(flow => hardwareModel.busses.map(bus => bus.transmissionDuration(flow.size)).max).sum
    val maxHorizon = summedMaxTaskDurations + summedMaxTransmissionTimes

    val cpTasks: Array[CPTask] = softwareModel.simpleProcesses.map(
      process => new CPTask(process.id, process, process.name, this, maxHorizon)
    )

    val cpProcessors = hardwareModel.processors.map(
      processor => processor.processorClass match {
        case m: MultiTaskPermanentTasks => new CPMultiTaskProcessor(processor.id, processor, processor.memSize, this)
        case m: MonoTaskSwitchingTask => new CPMonoTaskProcessor(processor.id, processor, processor.memSize, m.switchingDelay, this)
      })

    val processorToBusAdjacencyNoSelfLoop: Set[(Int, Int)] =
      hardwareModel.busses.flatMap(bus => bus.receivingFromProcessors.map(proc => (proc.id, bus.id))).toSet

    val busToProcessorAdjacencyNoSelfLoop: Set[(Int, Int)] =
      hardwareModel.busses.flatMap(bus => bus.sendingToProcessors.map(proc => (bus.id, proc.id))).toSet

    val processorToBusToProcessorAdjacencyNoSelfLoop: Set[(Int, Int, Int)] = hardwareModel.busses.flatMap(
      bus => bus.receivingFromProcessors.flatMap(
        receivingFrom => bus.sendingToProcessors.flatMap(
          sendingTo =>
            if (receivingFrom != sendingTo) Some((receivingFrom.id, bus.id, sendingTo.id))
            else None))).toSet

    val selfLoopBusses = hardwareModel.processors.toList.map(
      proc => new CPSelfLoopBus(proc.id + hardwareModel.busses.length, proc, this))

    val processorToBusToProcessorAdjacency: Set[(Int, Int, Int)] =
      processorToBusToProcessorAdjacencyNoSelfLoop ++ selfLoopBusses.map((bus: CPSelfLoopBus) => (bus.id, bus.processor.id, bus.id))

    val cpBusses: Array[CPBus] = (hardwareModel.busses.toList.map(
      bus => new CPRegularBus(bus.id, bus, this)
    ) ++ selfLoopBusses).toArray

    val cpTransmissions: Array[CPTransmission] = softwareModel.transmissions.map(
      flow => new CPTransmission(flow.id, flow,
        cpTasks(flow.source.id), cpTasks(flow.target.id),
        cpBusses,
        flow.size, flow.name, flow.timing,
        this, maxHorizon, processorToBusToProcessorAdjacency)
    )

    //registering tasks and transmissions to processors and busses
    for (bus <- cpBusses) {
      for (transmission <- cpTransmissions) {
        bus.accumulatePotentialTransmissionOnBus(transmission)
      }
      bus.close()
    }

    for (proc <- cpProcessors) {
      for (task <- cpTasks) {
        proc.accumulateExecutionConstraintsOnTask(task)
      }
      proc.close()
    }

    val energyForEachTask = cpTasks.map(task => task.energy)
    val taskEnds = cpTasks.map(task => task.end)
    val backgroundPower: Int = cpProcessors.map(p => p.p.constantPower.value).sum
    val makeSpan = maximum(taskEnds)
    val energy = sum(energyForEachTask) + makeSpan * backgroundPower

    //deadline
    softwareModel.softwareClass match {
      case OneShotSoftware(Some(deadline)) => add(makeSpan <= deadline)
      case OneShotSoftware(None) => ;
    }

    //powerCap
    hardwareModel.powerCap match {
      case None => ;
      case Some(cap) =>
        val simpleCumulativeTasks = cpTasks.toList.map(
          task => CumulativeTask(start = task.start, duration = task.duration, end = task.end, amount = task.power, explanation = "power of task " + task)
        )
        CumulativeTask.postCumulativeForSimpleCumulativeTasks(simpleCumulativeTasks, CPIntVar(cap - backgroundPower))
    }

    //energyCap
    hardwareModel.energyCap match {
      case None => ;
      case Some(cap) =>
        add(energy <= cap)
    }

    CPMappingProblem(
      hardwareModel.name,
      cpTasks,
      cpProcessors,
      cpBusses,
      cpTransmissions,
      makeSpan,
      energy)
  }

  def searchMappingProblem(problem: CPMappingProblem, goal: MappingGoal): Iterable[Mapping] = {

    goal match {
      case MinEnergy => minimize(problem.energy)
      case MinMakeSpan => minimize(problem.makeSpan)
      case ParetoMakeSpanEnergy => solver.paretoMinimize(problem.makeSpan, problem.energy)
    }

    solver.addDecisionVariables(problem.varsToSave)

    search {
      //binaryFirstFail(problem.varsToDistribute)

      //TODO: essayer cette stratégie-ci:
      //search(conflictOrderingSearch(startsVar,startsVar(_).min,startsVar(_).min))
      //setTimes(startsVar, durationsVar, endsVar)

      discrepancy(binaryFirstFail(problem.varsToDistribute), 5)
    } onSolution {
      println("solution found, makeSpan=" + problem.makeSpan.value + " energy:" + problem.energy.value)
    }

    val stat = start()

    println(stat)

    goal match {
      case ParetoMakeSpanEnergy =>
        solver.nonDominatedSolutions.sortBy(_.apply(problem.makeSpan)).map(cpSol => problem.getMapping(cpSol))
      case MinEnergy | MinMakeSpan =>
        val lastSol = solver.lastSol
        if (lastSol.dict.isEmpty) {
          None
        } else {
          Some(problem.getMapping(lastSol))
        }
    }
  }
}
