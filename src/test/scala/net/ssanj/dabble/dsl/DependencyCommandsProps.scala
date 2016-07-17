package net.ssanj.dabble
package dsl

import scala.io.Source
import scala.collection.mutable.{Map => MMap, LinkedHashSet, ArrayBuffer}

import org.scalacheck.Properties
import org.scalacheck.{Prop, Gen}

import DependencyCommands._
import DabblePathTypes._
import DabblePrinter._
import DabbleHistory.CommandlineParser
import TerminalSupport.historyParser
import DabbleProps._
import ScalaCheckSupport._

object DependencyCommandsProps extends Properties("DependencyCommands") {

  private case class OsName(name: String)

  //load osnames from resources/osnames.txt
  lazy val osNames =
    Source.
      fromInputStream(this.getClass.getResourceAsStream("/osnames.txt"), "UTF-8").
        getLines.
        toList

  private def genOsName: Gen[Option[OsName]] =
    Gen.option(
      Gen.oneOf(osNames).
        map(OsName)
    )

  private def genDabbleHomePath: Gen[DabbleHomePath] = for {
    n <- Gen.choose(1, 5)
    paths <- Gen.listOfN(n, Gen.alphaLowerChar)
  } yield DabbleHomePath(DirPath(paths.mkString("/")))

  property("should return sbt executable for a given OS") =
    Prop.forAll(genOsName) { osOp =>
      val world = MMap[String, Seq[String]]()
      //doesn't add the os.name for the None case
      //which simulates an environment where os.name is not defined
      initOs(osOp)(world)
      val executable = getSBTExec.foldMap(new SaveHistoryFileInterpreter(world))

      val windowsProp     = booleanProp("isWindows")(osOp.forall(_.name.toLowerCase.contains("windows")), true)
      val windowsExecProp = contentProp("sbt executable")(Seq(executable), Seq("sbt.bat"))
      val defaultExecProp = contentProp("sbt executable")(Seq(executable), Seq("sbt"))

      Prop.collect(osOp.map(_.name).getOrElse("*Not Defined*")) {
        (windowsProp && windowsExecProp) || defaultExecProp
      }
    }

 property("should run the sbt executable for a given OS") =
  Prop.forAll(genOsName, genDabbleHomePath) { (osOp, homePath) =>
    val world = MMap[String, Seq[String]]()
    initOs(osOp)(world)
    val execName = if (world.get("os.name").
                        flatMap(_.headOption).
                        filter(_.toLowerCase.startsWith("windows")).isDefined) "sbt.bat"
                   else "sbt"

    val result = executeSbt(homePath).foldMap(new SaveHistoryFileInterpreter(world))

    val isRightProp = booleanProp("isRight")(result.isRight, true)

    val callProcProp = {
      val Some(Seq(args, workingDir)) = world.get(execName)
      val argsProp = contentProp("arguments")(Seq(args), Seq("console-quick"))
      val workingDirProp = contentProp("workingDir")(
        Seq(homePath.work.path.dir), Seq(s"${homePath.path.dir}/work"))

      argsProp && workingDirProp
    }

    isRightProp && callProcProp
  }

  property("should launchSbt") = {
      Prop.forAllNoShrink(genDabbleHomePath, genDabbleHistoryLine, many(2)(genDabbleHistoryFileLine)) {  (dabbleHomePath, selection, historyFile) =>
        val historyLineParser: CommandlineParser = historyParser.parse(_, DabbleRunConfig())

        val world = MMap[String, Seq[String]](
            dabbleHomePath.history.path.file          -> historyFile.map(printHistoryLine),
            dabbleHomePath.defaultBuildFile.path.file -> Seq("some.build.file"),
            "os.name"                                 -> Seq("Linux 1.4.2"))

        // println(s"$world")
        val result = launchSbtConsole(dabbleHomePath, selection, historyLineParser, printHistoryLine).
          foldMap(new SaveHistoryFileInterpreter(world))

        val resultProp = contentProp("launchSbtConsole")(Seq(result), Seq(DabbleSuccess(Seq.empty)))

        val procArgs   = world.get("sbt")
        val launchProp = contentProp("launch args")(Seq(procArgs), Seq(Some(Seq("console-quick", dabbleHomePath.work.path.dir))))

        val historyFileContent = world.get(dabbleHomePath.history.path.file)
        val expectedHistoryFileContent = (LinkedHashSet() ++ (selection +: historyFile)).map(printHistoryLine).toSeq
        val historyfileProp = contentProp("historyFile")(Seq(historyFileContent), Seq(Some(expectedHistoryFileContent)))

        // println(s"after: $world")

        resultProp &&
        launchProp &&
        historyfileProp
      }
  }

  private def initOs(osOp: Option[OsName])(world: MMap[String, Seq[String]]): Unit = {
    osOp.foreach { os =>
      world += ("os.name" -> Seq(os.name))
    }
  }
}