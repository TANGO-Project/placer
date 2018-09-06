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

package placer

import java.io.{File, PrintWriter}

import net.liftweb.json._
import placer.algo.{Mapper, MapperConfig, Strategy}
import placer.io.Extractor
import placer.metadata.MappingProblem

import scala.io.Source

case class Config(in: File = new File("."),
                  out: File = null,
                  verbose: Boolean = false,
                  license:Boolean = false,
                  discrepancy:Int = Int.MaxValue,
                  strategies:Option[List[Strategy.Value]] = None,
                  timeLimit:Int = Int.MaxValue,
                  lns:Boolean = false,
                  lnsTimeLimit:Int = Int.MaxValue,
                  lnsMaxFails:Int = 2000,
                  lnsRelaxProba:Int = 90,
                  lnsNbRelaxations:Int = 500,
                  lnsNbRelaxationNoImprove:Int = 200,
                  lnsCarryOnObjForMultiHardware:Int = 1,
                  lnsUseEarlyStop:Boolean = true,
                  benchmark:Boolean = false)

object Main extends App {

  val parser = new scopt.OptionParser[Config]("placer") {
    head("placer", "beta3")

    help("help").text("prints this usage text")

    opt[File]("in").required().maxOccurs(1).valueName("<file>").
      action((x, c) => c.copy(in = x)).
      text("the JSon file representing the placement problem")

    opt[File]("out").maxOccurs(1).valueName("<file>").
      action((x, c) => c.copy(out = x)).
      text("the file where the JSon representing the placements will be stored")

    opt[Int]("discrepancy").maxOccurs(1).
      action((x, c) => c.copy(discrepancy = x)).
      text("the maximal discrepancy to use during the search must be >=0 , lower is faster but incomplete, use 20 for instance (5 if in a hurry). default is MaxInt")

    opt[Seq[String]]("strategy").maxOccurs(1).valueName("strategy1,strategy2,...").action( (strategies,c) =>
      c.copy(strategies = Some(strategies.toList.map(decodeStrategy)))).text(
      """|Search strategies to use, in the order they must be searched. The available search stategies are:
                                 TransmissionRouting
                                 TaskPlacementLessBuzyProcFirst
                                 LocalOrBusTransmissionLargestFirstLocalFirst
                                 LocalOrBusTransmissionLongestAdjFirstNonLocalFirst
                                 TaskPlacementFastestImplemPlusLessBuzyProcFirst
                                 SharedImplementationInstances
                                 TaskAndTransmissionStarts
                 when not specified, a defulat search strategy is used.
        """.stripMargin('|'))

    import Strategy._
    def decodeStrategy(s:String):Strategy.Value = {
      s match{
        case "TransmissionRouting" => TransmissionRouting
        case "TaskPlacementLessBuzyProcFirst" => TaskPlacementLessBuzyProcFirst
        case "LocalOrBusTransmissionLargestFirstLocalFirst" => LocalOrBusTransmissionLargestFirstLocalFirst
        case "LocalOrBusTransmissionLongestAdjFirstNonLocalFirst" => LocalOrBusTransmissionLongestAdjFirstNonLocalFirst
        case "TaskPlacementFastestImplemPlusLessBuzyProcFirst" => TaskPlacementFastestImplemPlusLessBuzyProcFirst
        case "SharedImplementationInstances" => SharedImplementationInstances
        case "TaskAndTransmissionStarts" => TaskAndTransmissionStarts
        case _ => throw new Error("unknown search strategy:" + s)
      }
    }

    opt[Int]("timeLimit").maxOccurs(1).
      action((x, c) => c.copy(timeLimit = x)).
      text("the maximal run time for Placer in seconds, default is MaxInt, also used for LNS")

    opt[Unit]("verbose").maxOccurs(1).action((_, c) =>
      c.copy(verbose = true)).text("prints some verbosities")

    opt[Unit]("license").maxOccurs(1).action((_, c) =>
      c.copy(license = true)).text("prints license and stops")

    //LNS stuff

    opt[Boolean]("lns").maxOccurs(1).action((bool,c) =>
      c.copy(lns = bool)).text("use LNS, only for single objective goal (minMakespan,minEnergy,...) not for sat or Pareto")

    opt[Int]("lnsMaxFails").maxOccurs(1).
      action((x, c) => c.copy(lnsMaxFails = x)).
      text("for LNS: the maximal number of fail per CP search default is 2000")

    opt[Int]("lnsTimeLimit").maxOccurs(1).
      action((x, c) => c.copy(lnsTimeLimit = x)).
      text("for LNS: the time limit for each LNS search. ")


    opt[Int]("lnsRelaxProba").maxOccurs(1).
      action((x, c) => c.copy(lnsRelaxProba = x)).
      text("for LNS: the probability (in percentage) to maintain an element from one solution to the relaxed one, default is 90")

    opt[Int]("lnsNbRelaxations").maxOccurs(1).
      action((x, c) => c.copy(lnsNbRelaxations = x)).
      text("for LNS: the total number of relaxation to try out, default is 500")

    opt[Int]("lnsNbRelaxationNoImprove").maxOccurs(1).
      action((x, c) => c.copy(lnsNbRelaxationNoImprove = x)).
      text("for LNS: the maximal number of consecutive relaxation without improvement, default is 200")

    opt[Int]("lnsCarryOnObjForMultiHardware").maxOccurs(1).action((int,c) =>
      c.copy(lnsCarryOnObjForMultiHardware = int)).
      text("""|when using multi hardware, should Placer carry on the best values for the objective function from one hardware to the next one?
                                 0 is no;
                                 1 is yes, but if first solution cannot be found, try again without carry on (this is the default)
                                 2 is yes, and without retry""".stripMargin('|'))

    opt[Boolean]("lnsUseEarlyStop").maxOccurs(1).action((bool,c) =>
      c.copy(lnsUseEarlyStop = bool)).text("when using LNS, this option ask the solver to first make a search with all limits (time and fails) divided by ten, " +
      "and stop if this search was not fruitful. If the search was fruitful, it then proceeds with the normal set limits (time and fails)")

    opt[Boolean]("benchmark").maxOccurs(1).action((bool,c) =>
      c.copy(benchmark = bool)).text("run the tool, reports the run time and obj, and optionally saves the outfile, if an outfile is given. ")

    note("""|Note: Placer is a java software so that all options taken by the JVM also apply.
            |Among them, you should consider the -Xmx and -Xms parameters to grant more memory to Placer:
            |example: java -Xms4G -Xmx15G -jar Placer.jar --in=...""".stripMargin('|'))
  }

  parser.parse(args, Config()) match {
    case None =>
      // arguments are bad, error message will have been displayed
      System.err.println("exiting Placer")
      System.exit(-1)
    case Some(config) if config.license =>
      // do stuff
      println(
        """
          |Copyright 2017 CETIC www.cetic.be
          |This is being developed for the TANGO Project: http://tango-project.eu
          |
          |Placer is free software: you can redistribute it and/or modify
          |it under the terms of the GNU Lesser General Public License as published by
          |the Free Software Foundation, either version 3 of the License, or
          |(at your option) any later version.
          |
          |Placer is distributed in the hope that it will be useful,
          |but WITHOUT ANY WARRANTY; without even the implied warranty of
          |MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
          |GNU Lesser General Public License  for more details.
          |
          |You should have received a copy of the GNU Lesser General Public License along with Placer.
          |If not, see http://www.gnu.org/licenses/lgpl-3.0.en.html""".stripMargin('|'))
      System.exit(0)
    case Some(config) =>
      val verbose = config.verbose

      val problemFile = Source.fromFile(config.in)
      val parsed = parse(problemFile.mkString)
      val problem: MappingProblem = Extractor.extractProblem(parsed,verbose)

      require(config.out != null || config.benchmark, "---out must be specified if not running benchmark mode")

      if (config.verbose) println(problem)

      val startTime = System.nanoTime()
      val mappingSet = Mapper.findMapping(problem,
        MapperConfig(maxDiscrepancy = config.discrepancy,
          timeLimit = config.timeLimit,
          lns = config.lns,
          lnsTimeLimit = config.lnsTimeLimit,
          strategy = config.strategies,
          lnsMaxFails = config.lnsMaxFails,
          lnsRelaxProba = config.lnsRelaxProba,
          lnsNbRelaxations = config.lnsNbRelaxations,
          lnsNbRelaxationNoImprove = config.lnsNbRelaxationNoImprove,
          lnsCarryOnObjForMultiHardware = config.lnsCarryOnObjForMultiHardware,
          lnsUseEarlyStop = config.lnsUseEarlyStop,
          verbose = verbose))

      val endTime = System.nanoTime()

      if (config.verbose) println(mappingSet)

      if(config.benchmark){
        println("runTime: " + ((endTime - startTime)/(1000*1000)) + " ms" + " obj: " + mappingSet.mapping.map(_.objValues.mkString(",")))
        if(config.out != null) {
          val outJSon = mappingSet.toJSon
          new PrintWriter(config.out) {
            val parsed = parse(outJSon)
            write(prettyRender(parsed))
            close()
          }
        }
      }else{
        val outJSon = mappingSet.toJSon
        new PrintWriter(config.out) {
          val parsed = parse(outJSon)
          write(prettyRender(parsed))
          close()
        }
      }
      System.exit(0)
  }
}

