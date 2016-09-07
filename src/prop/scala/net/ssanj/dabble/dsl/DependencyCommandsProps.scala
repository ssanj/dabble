package net.ssanj.dabble
package dsl

import scala.io.Source
import scala.collection.mutable.{Map => MMap, LinkedHashSet, ArrayBuffer}

import org.scalacheck.Properties
import org.scalacheck.{Prop, Gen}

import scalaz.syntax.nel._

import DependencyCommands._
import DabblePathTypes._
import DabblePrinter._
import DefaultTemplate.inMemSbtTemplate
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
      Prop.forAllNoShrink(genDabbleHomePath,
                          genDabbleHistoryLine,
                          genSbtTemplateContent,
                          many(2)(genDabbleHistoryFileLine)) {
        case (dabbleHomePath, selection, sbtTemplate, historyFile) =>
          val historyLineParser: CommandlineParser = historyParser.parse(_, DabbleRunConfig())

          val world = MMap[String, Seq[String]](
              dabbleHomePath.history.path.file          -> historyFile.map(printHistoryLine),
              dabbleHomePath.defaultBuildFile.path.file -> Seq(sbtTemplate.content),
              "os.name"                                 -> Seq("Linux 1.4.2"))

          val result = launchSbtConsole(dabbleHomePath, selection, historyLineParser, printHistoryLine).
            foldMap(new SaveHistoryFileInterpreter(world))

          val resultProp = contentProp("launchSbtConsole")(Seq(result), Seq(DabbleSuccess(Seq.empty)))

          val Some(buildFileContent)   = world.get(dabbleHomePath.work.defaultBuildFile.path.file)
          val expectedBuildFileContent = formatSbtTemplate(sbtTemplate.content, selection)
          val buildFileContentsProp    = contentProp("build.sbt")(buildFileContent, Seq(expectedBuildFileContent))

          val procArgs   = world.get("sbt")
          val launchProp = contentProp("launch args")(Seq(procArgs), Seq(Some(Seq("console-quick", dabbleHomePath.work.path.dir))))

          val historyFileContent         = world.get(dabbleHomePath.history.path.file)
          val expectedHistoryFileContent = (LinkedHashSet() ++ (selection +: historyFile)).map(printHistoryLine).toSeq
          val historyfileProp            = contentProp("historyFile")(
            Seq(historyFileContent), Seq(Some(expectedHistoryFileContent)))

          resultProp &&
          launchProp &&
          buildFileContentsProp &&
          historyfileProp
      }
  }

  property("launchSbt.should handle errors reading the history file") = {
      Prop.forAllNoShrink(genDabbleHomePath, genDabbleHistoryLine, genWords) { (dabbleHomePath, selection, words) =>
          val historyLineParser: CommandlineParser = historyParser.parse(_, DabbleRunConfig())
          val expectedError = words.map(_.word).mkString(" ")

          val world = MMap[String, Seq[String]](
            s"${dabbleHomePath.history.path.file}" -> Seq("this will not be read"),
            s"ReadFile.${dabbleHomePath.history.path.file}.error" -> Seq(expectedError)
          )

          val result = launchSbtConsole(dabbleHomePath, selection, historyLineParser, printHistoryLine).
            foldMap(new SaveHistoryFileInterpreter(world))

          contentProp("launchSbt")(
            Seq(result),
            Seq(DabbleFailure(s"Could not read history file: ${dabbleHomePath.history.path.file} due to: ${expectedError}".wrapNel)))
      }
  }

  property("should launchSbt with in memory sbt template if a custom template doesn't exist") = {
      Prop.forAllNoShrink(genDabbleHomePath,
                          genDabbleHistoryLine,
                          many(2)(genDabbleHistoryFileLine)) {
        case (dabbleHomePath, selection, historyFile) =>
          val historyLineParser: CommandlineParser = historyParser.parse(_, DabbleRunConfig())

          val world = MMap[String, Seq[String]](
              dabbleHomePath.history.path.file          -> historyFile.map(printHistoryLine),
              "os.name"                                 -> Seq("Linux 1.4.2"))

          val result = launchSbtConsole(dabbleHomePath, selection, historyLineParser, printHistoryLine).
            foldMap(new SaveHistoryFileInterpreter(world))

          val resultProp = contentProp("launchSbtConsole")(Seq(result), Seq(DabbleSuccess(Seq.empty)))

          val Some(buildFileContent)   = world.get(dabbleHomePath.work.defaultBuildFile.path.file)
          val expectedBuildFileContent = formatSbtTemplate(inMemSbtTemplate(newlines(2)), selection)
          val buildFileContentsProp    = contentProp("build.sbt")(buildFileContent, Seq(expectedBuildFileContent))

          val procArgs   = world.get("sbt")
          val launchProp = contentProp("launch args")(Seq(procArgs), Seq(Some(Seq("console-quick", dabbleHomePath.work.path.dir))))

          val historyFileContent         = world.get(dabbleHomePath.history.path.file)
          val expectedHistoryFileContent = (LinkedHashSet() ++ (selection +: historyFile)).map(printHistoryLine).toSeq
          val historyfileProp            = contentProp("historyFile")(
            Seq(historyFileContent), Seq(Some(expectedHistoryFileContent)))

          resultProp &&
          launchProp &&
          buildFileContentsProp &&
          historyfileProp
      }
  }


 property("should fail to launchSbt if the build.sbt file can't be written") = {
      Prop.forAllNoShrink(genDabbleHomePath,
                          genDabbleHistoryLine,
                          genSbtTemplateContent,
                          many(2)(genDabbleHistoryFileLine),
                          genWords) {
        case (dabbleHomePath, selection, sbtTemplate, historyFile, errorMessages) =>
          val historyLineParser: CommandlineParser = historyParser.parse(_, DabbleRunConfig())
          val writeError = errorMessages.map(_.word).mkString(" ")

          val world = MMap[String, Seq[String]](
              dabbleHomePath.history.path.file                                     -> historyFile.map(printHistoryLine),
              dabbleHomePath.defaultBuildFile.path.file                            -> Seq(sbtTemplate.content),
              "os.name"                                                            -> Seq("Linux 1.4.2"),
              s"WriteFile.${dabbleHomePath.work.defaultBuildFile.path.file}.error" -> Seq(writeError))

          val result = launchSbtConsole(dabbleHomePath, selection, historyLineParser, printHistoryLine).
            foldMap(new SaveHistoryFileInterpreter(world))

          contentProp("launchSbtConsole")(Seq(result), Seq(DabbleFailure(writeError.wrapNel)))
      }
  }

 property("should fail to launchSbt there is an error calling sbt") = {
      Prop.forAllNoShrink(genDabbleHomePath,
                          genDabbleHistoryLine,
                          genSbtTemplateContent,
                          many(2)(genDabbleHistoryFileLine),
                          genWords) {
        case (dabbleHomePath, selection, sbtTemplate, historyFile, errorMessages) =>
          val historyLineParser: CommandlineParser = historyParser.parse(_, DabbleRunConfig())
          val writeError = errorMessages.map(_.word).mkString(" ")

          val world = MMap[String, Seq[String]](
              dabbleHomePath.history.path.file                                       -> historyFile.map(printHistoryLine),
              dabbleHomePath.defaultBuildFile.path.file                              -> Seq(sbtTemplate.content),
              "os.name"                                                              -> Seq("Linux 1.4.2"),
              s"CallProcess.sbt.console-quick.${dabbleHomePath.work.path.dir}.error" -> Seq(writeError))

          val result = launchSbtConsole(dabbleHomePath, selection, historyLineParser, printHistoryLine).
            foldMap(new SaveHistoryFileInterpreter(world))

          contentProp("launchSbtConsole")(Seq(result), Seq(DabbleFailure(writeError.wrapNel)))
      }
  }

 property("should fail to launchSbt if the history file can't be saved") = {
      Prop.forAllNoShrink(genDabbleHomePath,
                          genDabbleHistoryLine,
                          genSbtTemplateContent,
                          many(2)(genDabbleHistoryFileLine),
                          genWords) {
        case (dabbleHomePath, selection, sbtTemplate, historyFile, errorMessages) =>
          val historyLineParser: CommandlineParser = historyParser.parse(_, DabbleRunConfig())
          val writeError = errorMessages.map(_.word).mkString(" ")

          val world = MMap[String, Seq[String]](
              dabbleHomePath.history.path.file                       -> historyFile.map(printHistoryLine),
              dabbleHomePath.defaultBuildFile.path.file              -> Seq(sbtTemplate.content),
              "os.name"                                              -> Seq("Linux 1.4.2"),
              s"WriteFile.${dabbleHomePath.history.path.file}.error" -> Seq(writeError))

          val result = launchSbtConsole(dabbleHomePath, selection, historyLineParser, printHistoryLine).
            foldMap(new SaveHistoryFileInterpreter(world))

          contentProp("launchSbtConsole")(Seq(result), Seq(DabbleFailure(writeError.wrapNel)))
      }
  }

  property("should read the default sbt template when valid") = {
    Prop.forAllNoShrink(genDabbleHomePath, genSbtTemplateContent) { (dabbleHomePath, sbtTemplateContent) =>
      val world = MMap[String, Seq[String]](
        dabbleHomePath.defaultBuildFile.path.file -> Seq(sbtTemplateContent.content)
      )

      val content = readSbtTemplateOrDefault(dabbleHomePath.defaultBuildFile.path.file).
                      foldMap(new SaveHistoryFileInterpreter(world))

      val sbtContentProp  = contentProp("sbtTemplate")(Seq(content), Seq(sbtTemplateContent.content))

      val expectedLog = s"Using default sbt template at: ${dabbleHomePath.defaultBuildFile.path.file}"
      val logProp     = contentProp("log")(Seq(world.get("log").get.head), Seq(expectedLog))

      sbtContentProp && logProp
    }
  }

  private def initOs(osOp: Option[OsName])(world: MMap[String, Seq[String]]): Unit = {
    osOp.foreach { os =>
      world += ("os.name" -> Seq(os.name))
    }
  }
}