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

package placerT

import java.io.{File, PrintWriter}

import net.liftweb.json._
import placerT.algo.{Mapper, MapperConfig}
import placerT.io.Extractor
import placerT.metadata.MappingProblem

import scala.io.Source


case class Config(in: File = new File("."),
                  out: File = new File("."),
                  verbose: Boolean = false,
                  license:Boolean = false,
                  discrepancy:Int = Int.MaxValue,
                  timeLimit:Int = Int.MaxValue,
                  lns:Boolean = false)

object Main extends App {

  val parser = new scopt.OptionParser[Config]("placer") {
    head("placer", "alpha6")

    opt[File]('i', "in").required().valueName("<file>").
      action((x, c) => c.copy(in = x)).
      text("'in' is the JSon file representing the placement problem")

    opt[File]('o', "out").required().valueName("<file>").
      action((x, c) => c.copy(out = x)).
      text("'out' is the file where the JSon representing the placements will be stored")

    opt[Int]('d', "discrepancy").
      action((x, c) => c.copy(discrepancy = x)).
      text("'d' the maximal discrepancy to use during the search must be >=0 , lower is faster but incomplete, use 20 for instance (5 if in a hurry). default is MaxInt")

    opt[Int]('t', "timeLimit").
      action((x, c) => c.copy(timeLimit= x)).
      text("'t' the maximal run time for Placer in seconds, default is MaxInt")

    opt[Unit]("verbose").action((_, c) =>
      c.copy(verbose = true)).text("prints some verbosities")

    opt[Unit]("license").action((_, c) =>
      c.copy(license = true)).text("prints license and stops")

    opt[Boolean]("lns").action((_,c) =>
      c.copy(lns = true)).text("use LNS, only for single objective goal (minMakespan,minEnergy,...) not for sat or Pareto")



    help("help").text("prints this usage text")

    note("""|Note: Placer is a java software so that all options taken by the JVM also apply.
        |Among them, you should consider the -Xmx and -Xms parameters to grant more memory to Placer:
        |example: java -Xms4G -Xmx15G -jar Placer.jar --in=...""".stripMargin('|'))
  }

  parser.parse(args, Config()) match {
    case None =>
      // arguments are bad, error message will have been displayed
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

      if (config.verbose) println(problem)
      
      val mappingSet = Mapper.findMapping(problem,MapperConfig(config.discrepancy,config.timeLimit))

      if (config.verbose) println(mappingSet)

      val outJSon = mappingSet.toJSon
      new PrintWriter(config.out) {
        write(prettyRender(parse(outJSon)))
        close()
      }
      System.exit(0)
  }
}

