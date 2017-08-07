# Placer

&copy; CETIC 2017 www.cetic.be

Placer is a component of the European Project TANGO (http://tango-project.eu ).

Placer is distributed under a [GNU LGPL License, version 3.0 or above](https://www.gnu.org/licenses/lgpl-3.0.en.html).

## Description

Placer is a placement optimization engine. it helps finding adequate placement of complex software onto heterogeneous multi processing hardware platform. 

## Installation for Use

### Requirements

To use Placer you need to install two pieces of software:
* [jdk1.8](http://www.oracle.com/technetwork/java/javase/downloads/jdk8-downloads-2133151.html).
* [scala 2.11](https://www.scala-lang.org/download/2.11.8.html).

### Installation and configuration procedure

Just copy the .jar of the release, once with requirements are installed and run it through command line. 
In the repository, \data contains two examples: example1.json and smallExample.json and the output that should be produced by Placer: placement_of_example1.json and placment_of_smallExample.json, respectively

examples of usage: 
```
>java -jar Placer.jar --help
placer alpha4
Usage: placer [options]
  -i, --in <file>   'in' is the JSon file representing the placement problem
  -o, --out <file>  'out' is the file where the JSon representing the placements will be stored
  --verbose         prints some verbosities
  --license         prints license and stops
  --help            prints this usage text
```

another example of usage: 
```
>java -jar Placer.jar --in=data\smallExample.json --out=out.json
new solution:258,32706
solution found, makeSpan=258 energy:32706
new solution:118,35026
solution found, makeSpan=118 energy:35026
nNodes: 22
nFails: 12
time(ms): 19
completed: true
timeInTrail: 0
nSols: 2
```

















