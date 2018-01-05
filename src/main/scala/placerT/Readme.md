# Placer

&copy; CETIC 2017 www.cetic.be

Placer is a component of the European Project TANGO (http://tango-project.eu ).

Placer is distributed under a [GNU LGPL License, version 3.0 or above](https://www.gnu.org/licenses/lgpl-3.0.en.html).

## Description

Placer is a model-based tool that, given a model of heterogeneous (or at least multi-core) hardware and a task-based complex software, finds a mapping of the software tasks on the various processing elements of the hardware, together with a routing of the transmissions on the available busses, and provides a schedule for the tasks and transmissions.
The mapping can minimize either the run time, energy consumption, or both in a multi-objective fashion.

## Installation

### Requirements

Placer is an R&D Tool that runs on the JVM. As such it can run on any office machine running a JVM, with say 10Gb of RAM available for the tool.

To use Placer you need to install two pieces of software:
* [jdk1.8](http://www.oracle.com/technetwork/java/javase/downloads/jdk8-downloads-2133151.html).
* [scala 2.11](https://www.scala-lang.org/download/2.11.8.html).

### Installation and configuration procedure

Just copy the .jar of the release, once with requirements are installed and run it through command line. 
In the repository, \data contains two examples: example1.json and smallExample.json and the output that should be produced by Placer: placement_of_example1.json and placment_of_smallExample.json, respectively

## Usage:

Placer is a command-line tool. A documentation of all parameter is available through the command line as well using the –help flag:

```
> java -jar Placer.jar --help

placer beta1
Usage: placer [options]

  --in <file>	the JSon file representing the placement problem
  --out <file>	the file where the JSon representing the placements will be stored
  --discrepancy <value> 	the maximal discrepancy to use during the search must be >=0 , lower is faster but incomplete, use 20 for instance (5 if in a hurry). default is MaxInt
  --timeLimit <value>	the maximal run time for Placer in seconds, default is MaxInt. In case of LNS it is taken as the time limit pers CP exploration.
  --verbose	prints some verbosities
  --license	prints license and stops
  --lns <value>	use LNS, only for single objective goal (minMakespan,minEnergy,...) not for sat or Pareto
  --lnsMaxFails <value>	for LNS: the maximal number of fail per CP search default is 2000
  --lnsRelaxProba <value>	for LNS: the probability (in percentage) to maintain an element from one solution to the relaxed one, default is 90
  --lnsNbRelaxations <value> 	for LNS: the total number of relaxation to try out, default is 500
  --lnsNbRelaxationNoImprove <value>	for LNS: the maximal number of consecutive relaxation without improvement, default is 200
  --help	prints this usage text

Note: Placer is a java software so that all options taken by the JVM also apply. Among them, you should consider the -Xmx and -Xms parameters to grant more memory to Placer.

example: java -Xms4G -Xmx15G -jar Placer.jar --in=...
```



another example of usage:
notice that OscaR is quite verbose during its execution, even if verbosities are disabled.

```
> java -Xms5G -Xmx20G Placer.jar  --in=aquascanDemo1.json
		--out=aquascanOut1.json --timeLimit=10

staticMaxHorizon:291553
maxHorizon:      291553
nbTasks:51
nbTransmissions:62
creating tasks
creating processors
constants about adjacency
creating busses
creating transmissions
registering tasks and transmissions to processors and busses
registering execution constraints per processors and task
INFO: skipping tautological cumulative constraint: temporary storage for processor Core1, summedMaxAmount:146506, min available resource:1048576
INFO: skipping tautological cumulative constraint: temporary storage for processor Core2, summedMaxAmount:146506, min available resource:1048576
INFO: skipping tautological cumulative constraint: temporary storage for processor Core3, summedMaxAmount:146506, min available resource:1048576
INFO: skipping tautological cumulative constraint: temporary storage for processor Core4, summedMaxAmount:146506, min available resource:1048576
computing makeSpan on tasks
redundant bin-packing constraint on usage per bus
computing total energy consumption
posting mapping constraints
posting MustBeUsedConstraint(Core1)
posting MustBeUsedConstraint(Core2)
posting MustBeUsedConstraint(Core3)
posting MustBeUsedConstraint(Core4)
breaking symmetry among List(Core1, Core2, Core3, Core4) by workload
starting search
solution found, makeSpan=125024 energy:0
solution found, makeSpan=88643 energy:0
solution found, makeSpan=88642 energy:0
solution found, makeSpan=88641 energy:0
nNodes: 975297
nFails: 487355
time(ms): 10002
completed: false
timeInTrail: 605
nSols: 4
secondLevels:487897
thirdLevels:4


Process finished with exit code 0
```
