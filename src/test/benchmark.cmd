
@ECHO OFF

SET JAVA="C:\Program Files\Java\jdk1.8.0_162\bin\java.exe"
REM -Xms5G -Xmx20G
SET ALLJARS="-javaagent:C:\Program Files\JetBrains\IntelliJ IDEA Community Edition 2018.1.6\lib\idea_rt.jar=57435:C:\Program Files\JetBrains\IntelliJ IDEA Community Edition 2018.1.6\bin" -Dfile.encoding=UTF-8 -classpath "C:\Program Files\Java\jdk1.8.0_162\jre\lib\charsets.jar;C:\Program Files\Java\jdk1.8.0_162\jre\lib\deploy.jar;C:\Program Files\Java\jdk1.8.0_162\jre\lib\ext\access-bridge-64.jar;C:\Program Files\Java\jdk1.8.0_162\jre\lib\ext\cldrdata.jar;C:\Program Files\Java\jdk1.8.0_162\jre\lib\ext\dnsns.jar;C:\Program Files\Java\jdk1.8.0_162\jre\lib\ext\jaccess.jar;C:\Program Files\Java\jdk1.8.0_162\jre\lib\ext\jfxrt.jar;C:\Program Files\Java\jdk1.8.0_162\jre\lib\ext\localedata.jar;C:\Program Files\Java\jdk1.8.0_162\jre\lib\ext\nashorn.jar;C:\Program Files\Java\jdk1.8.0_162\jre\lib\ext\sunec.jar;C:\Program Files\Java\jdk1.8.0_162\jre\lib\ext\sunjce_provider.jar;C:\Program Files\Java\jdk1.8.0_162\jre\lib\ext\sunmscapi.jar;C:\Program Files\Java\jdk1.8.0_162\jre\lib\ext\sunpkcs11.jar;C:\Program Files\Java\jdk1.8.0_162\jre\lib\ext\zipfs.jar;C:\Program Files\Java\jdk1.8.0_162\jre\lib\javaws.jar;C:\Program Files\Java\jdk1.8.0_162\jre\lib\jce.jar;C:\Program Files\Java\jdk1.8.0_162\jre\lib\jfr.jar;C:\Program Files\Java\jdk1.8.0_162\jre\lib\jfxswt.jar;C:\Program Files\Java\jdk1.8.0_162\jre\lib\jsse.jar;C:\Program Files\Java\jdk1.8.0_162\jre\lib\management-agent.jar;C:\Program Files\Java\jdk1.8.0_162\jre\lib\plugin.jar;C:\Program Files\Java\jdk1.8.0_162\jre\lib\resources.jar;C:\Program Files\Java\jdk1.8.0_162\jre\lib\rt.jar;C:\Users\rdl\Documents\TANGO\Optim5TANGO\placer\target\scala-2.11\classes;C:\Users\rdl\.ivy2\cache\org.scala-lang.modules\scala-xml_2.11\jars\scala-xml_2.11-1.0.6.jar;C:\Users\rdl\.ivy2\cache\org.scala-lang.modules\scala-xml_2.11\bundles\scala-xml_2.11-1.0.6.jar;C:\Users\rdl\.ivy2\cache\org.jfree\jfreechart\jars\jfreechart-1.0.19.jar;C:\Users\rdl\.ivy2\cache\org.jfree\jcommon\jars\jcommon-1.0.23.jar;C:\Users\rdl\.ivy2\cache\org.swinglabs\swingx\jars\swingx-1.0.jar;C:\Users\rdl\.ivy2\cache\com.jhlabs\filters\jars\filters-2.0.235.jar;C:\Users\rdl\.ivy2\cache\org.swinglabs\swing-worker\jars\swing-worker-1.1.jar;C:\Users\rdl\.ivy2\cache\org.swinglabs\swingx-ws\jars\swingx-ws-1.0.jar;C:\Users\rdl\.ivy2\cache\jdom\jdom\jars\jdom-1.0.jar;C:\Users\rdl\.ivy2\cache\commons-httpclient\commons-httpclient\jars\commons-httpclient-3.0.1.jar;C:\Users\rdl\.ivy2\cache\junit\junit\jars\junit-3.8.1.jar;C:\Users\rdl\.ivy2\cache\commons-logging\commons-logging\jars\commons-logging-1.1.jar;C:\Users\rdl\.ivy2\cache\log4j\log4j\jars\log4j-1.2.12.jar;C:\Users\rdl\.ivy2\cache\logkit\logkit\jars\logkit-1.0.1.jar;C:\Users\rdl\.ivy2\cache\avalon-framework\avalon-framework\jars\avalon-framework-4.1.3.jar;C:\Users\rdl\.ivy2\cache\javax.servlet\servlet-api\jars\servlet-api-2.3.jar;C:\Users\rdl\.ivy2\cache\commons-codec\commons-codec\jars\commons-codec-1.3.jar;C:\Users\rdl\.ivy2\cache\net.sf.json-lib\json-lib\jars\json-lib-0.9.jar;C:\Users\rdl\.ivy2\cache\commons-beanutils\commons-beanutils\jars\commons-beanutils-1.7.0.jar;C:\Users\rdl\.ivy2\cache\commons-lang\commons-lang\jars\commons-lang-2.2.jar;C:\Users\rdl\.ivy2\cache\xom\xom\jars\xom-1.1.jar;C:\Users\rdl\.ivy2\cache\xerces\xmlParserAPIs\jars\xmlParserAPIs-2.6.2.jar;C:\Users\rdl\.ivy2\cache\xalan\xalan\jars\xalan-2.7.0.jar;C:\Users\rdl\.ivy2\cache\jaxen\jaxen\jars\jaxen-1.1-beta-8.jar;C:\Users\rdl\.ivy2\cache\dom4j\dom4j\jars\dom4j-1.6.1.jar;C:\Users\rdl\.ivy2\cache\net.sf.ezmorph\ezmorph\jars\ezmorph-0.8.1.jar;C:\Users\rdl\.ivy2\cache\xerces\xercesImpl\jars\xercesImpl-2.8.1.jar;C:\Users\rdl\.ivy2\cache\jtidy\jtidy\jars\jtidy-4aug2000r7-dev.jar;C:\Users\rdl\.ivy2\cache\rome\rome\jars\rome-0.8.jar;C:\Users\rdl\.ivy2\cache\org.scala-lang.modules\scala-parser-combinators_2.11\jars\scala-parser-combinators_2.11-1.0.6.jar;C:\Users\rdl\.ivy2\cache\org.scala-lang.modules\scala-parser-combinators_2.11\bundles\scala-parser-combinators_2.11-1.0.6.jar;C:\Users\rdl\.ivy2\cache\org.scala-lang\scala-library\jars\scala-library-2.11.4.jar;C:\Users\rdl\.m2\repository\net\liftweb\lift-json_2.11\3.0.1\lift-json_2.11-3.0.1.jar;C:\Users\rdl\.m2\repository\org\scala-lang\scala-library\2.11.7\scala-library-2.11.7.jar;C:\Users\rdl\.m2\repository\org\scala-lang\scala-reflect\2.11.8\scala-reflect-2.11.8.jar;C:\Users\rdl\.m2\repository\org\scala-lang\modules\scala-parser-combinators_2.11\1.0.4\scala-parser-combinators_2.11-1.0.4.jar;C:\Users\rdl\.m2\repository\org\scala-lang\modules\scala-xml_2.11\1.0.5\scala-xml_2.11-1.0.5.jar;C:\Users\rdl\.m2\repository\org\scala-lang\scala-compiler\2.11.7\scala-compiler-2.11.7.jar;C:\Users\rdl\.m2\repository\org\scala-lang\scalap\2.11.7\scalap-2.11.7.jar;C:\Users\rdl\.m2\repository\com\thoughtworks\paranamer\paranamer\2.8\paranamer-2.8.jar;C:\Users\rdl\.m2\repository\com\github\scopt\scopt_2.11\3.5.0\scopt_2.11-3.5.0.jar;C:\Users\rdl\.m2\repository\org\scala-lang\scala-library\2.11.8\scala-library-2.11.8.jar;C:\Users\rdl\.ivy2\cache\oscar\oscar-cp_2.11\jars\oscar-cp_2.11-4.0.0-SNAPSHOT.jar;C:\Users\rdl\.ivy2\cache\oscar\oscar-algo_2.11\jars\oscar-algo_2.11-4.0.0-SNAPSHOT.jar"
REM placer.Main
REM --in=

REM SET IN="C:\Users\rdl\Documents\TANGO\Optim5TANGO\placer\data\matMul\MatMulLarger.json"
REM SET OUT="C:\Users\rdl\Documents\TANGO\Optim5TANGO\placer\data\matMul\MatMulLargerOut.json"

SET IN="C:\Users\rdl\Documents\TANGO\Optim5TANGO\placer\data\aquaScan\AquascanForJournalPaperBench.json"
SET OUT="C:\Users\rdl\Documents\TANGO\Optim5TANGO\placer\data\aquaScan\AquascanForJournalPaperBenchOUT.json"




REM %JAVA% -Xms5G -Xmx20G %ALLJARS% placer.Main --benchmark=true --in=%IN% --out=%OUT% --lns=true --lnsNbRelaxationNoImprove=50 --lnsMaxFails=5000 --lnsRelaxProba=5 --lnsUseEarlyStop=true --strategy=%STRATEGY%

SET BASICPLACERCOMMAND=%JAVA% -Xms5G -Xmx20G %ALLJARS% placer.Main --benchmark=true --in=%IN% --out=%OUT%


SET STRATEGY0="TaskPlacementLessBuzyProcFirst,LocalOrBusTransmissionLargestFirstLocalFirst,TaskAndTransmissionStarts"
SET STRATEGY1="LocalOrBusTransmissionLargestFirstLocalFirst,TaskPlacementLessBuzyProcFirst,TaskAndTransmissionStarts"
SET STRATEGY2="TaskPlacementLessBuzyProcFirst,TaskAndTransmissionStarts,LocalOrBusTransmissionLargestFirstLocalFirst"
SET STRATEGY3="LocalOrBusTransmissionLongestAdjFirstNonLocalFirst,TaskAndTransmissionStarts,TaskPlacementLessBuzyProcFirst,LocalOrBusTransmissionLargestFirstLocalFirst"


SET LNS=false
REM this one is pointless because of CP, but it is required for correct parsing
SET RELAXPROBA=5
CALL :runAllTimingAndStrategy

SET RELAXPROBA=5
SET LNS=true
CALL :runAllTimingAndStrategy
SET RELAXPROBA=10
SET LNS=true
CALL :runAllTimingAndStrategy
SET RELAXPROBA=20
SET LNS=true
CALL :runAllTimingAndStrategy
SET RELAXPROBA=80
SET LNS=true
CALL :runAllTimingAndStrategy

EXIT /B %ERRORLEVEL%

REM generates a group of three lines
:runAllTimingAndStrategy
SET TIMELIMIT=5
CALL :runAllStrategy
SET TIMELIMIT=30
CALL :runAllStrategy
SET TIMELIMIT=60
CALL :runAllStrategy
EXIT /B 0

REM generates a line in the bench
:runAllStrategy
SET STRATEGY=%STRATEGY0%
CALL :run
SET STRATEGY=%STRATEGY1%
CALL :run
SET STRATEGY=%STRATEGY2%
CALL :run
SET STRATEGY=%STRATEGY3%
CALL :run
EXIT /B 0


:run
ECHO LNS %LNS%
ECHO RELAXPROBA %RELAXPROBA%
ECHO TIMELIMIT %TIMELIMIT%
ECHO STRATEGY %STRATEGY%
%BASICPLACERCOMMAND% --lns=%LNS% --lnsNbRelaxationNoImprove=5000 --lnsMaxFails=50000 --timeLimit=%TIMELIMIT% --lnsRelaxProba=%RELAXPROBA% --lnsUseEarlyStop=true --strategy=%STRATEGY% --lnsTimeLimit=20
EXIT /B 0