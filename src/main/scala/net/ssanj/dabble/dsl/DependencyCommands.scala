package net.ssanj.dabble
package dsl

import scala.collection.mutable.LinkedHashSet

import ammonite.ops._

import scalaz._
import scalaz.syntax.bind._
import DabbleHistory._
import DabbleDslDef._
import DabbleResult._
import DabblePathTypes._
import DefaultTemplate._
import CommonCommands._
import DabblePrinter.formatSbtTemplate
import ResolverParser._

object DependencyCommands {

  //TODO: Delete once we have the EitherT sorted out
  @SuppressWarnings(Array("UnusedMethodParameter"))
  def launchDabble(dabbleHomePath: DabbleHomePath, line: DabbleHistoryLine): DabbleScript[ErrorOr[Unit]] = ???
  //TODO: split this method up

  def getSBTExec: DabbleScript[String] = for {
    sbtE <- systemProp("os.name")
    sbtExec = sbtE match {
      case -\/(error) => "sbt"
      case \/-(os) if os.toLowerCase.startsWith("windows") => "sbt.bat"
      case \/-(_) => "sbt"
    }
  } yield sbtExec

  def executeSbt(dabbleHomePath: DabbleHomePath): DabbleScript[ErrorOr[Unit]] = for {
    sbt    <- getSBTExec
    result <- callProcess(sbt, "console-quick", dabbleHomePath.work.path.dir)
  } yield result

  def readSbtTemplateOrDefault(defaultSbtTemplate: String): DabbleScript[String] = {
    log(s"Using default sbt template at: ${defaultSbtTemplate}") >>
      readFile(defaultSbtTemplate).flatMap {
        case -\/(error) =>
          log(s"could not load template file due to: $error").
            flatMap(_ => useInMemoryTemplate)
        case \/-(content) => newlinesDS(1).map(content.mkString(_))
      }
  }

  def useInMemoryTemplate: DabbleScript[String] = for {
        _  <- log("Using in-memory sbt template. Create a build.sbt file in ~/.dabble/ to override.")
        nl <- newlinesDS(2)
  } yield inMemSbtTemplate(nl)

  def logDabbleVersion: DabbleScript[Unit] = {
    log(s"${DabbleInfo.version}-b${DabbleInfo.buildInfoBuildNumber}")
  }

  //TODO: Write out log at source or error or return error in an ExecutionResult2?
  def saveHistoryFile(filename: String,
                      selection: DabbleHistoryLine,
                      hLines: Seq[DabbleHistoryLine],
                      historyPrinter: DabbleHistoryLine => String):
    DabbleScript[ErrorOr[Unit]] = {
      val uniqueHLines = LinkedHashSet() ++ (selection +: hLines)
      writeFile(filename, uniqueHLines.map(historyPrinter).toSeq)
    }

  //program
  def launchSbtConsole(dabbleHomePath: DabbleHomePath,
                       line: DabbleHistoryLine,
                       argParser: CommandlineParser,
                       historyPrinter: DabbleHistoryLine => String): DabbleScript[DabbleResult] = {
    import DabbleScriptErrorOr._
    val resultET: DabbleScriptErrorOr[HistoryLinesAndWarnings] =
      for {
        hlaw <- liftScriptErrorOr(loadHistoryFile(dabbleHomePath.history.path.file, argParser)).
                  leftMap(error => s"Could not read history file: ${dabbleHomePath.history.path.file} due to: $error")
        _ <- liftScript(logDabbleVersion)
        defaultSbtTemplate  = dabbleHomePath.defaultBuildFile.path.file
        outputSBTFile       = dabbleHomePath.work.defaultBuildFile.path.file
        lineSeparator <- liftScript(newlinesDS(1))
        sbtTemplateContent  <- liftScript(fileExists(defaultSbtTemplate).
                                ifM(readSbtTemplateOrDefault(defaultSbtTemplate),
                                    useInMemoryTemplate))
        sbtBuildContent = formatSbtTemplate(sbtTemplateContent, line)
        _ <- liftScriptErrorOr(writeFile(outputSBTFile, Seq(sbtBuildContent)))
        _ <- liftScriptErrorOr(executeSbt(dabbleHomePath))
        _ <- liftScriptErrorOr(saveHistoryFile(dabbleHomePath.history.path.file,
                                        line,
                                        getHistoryLines(hlaw),
                                        historyPrinter))
      } yield hlaw

      resultET.run map {
        case -\/(error) => dabbleFailure(error)
        case \/-(hlaw)  => dabbleSuccess(getWarnings(hlaw))
      }
    }
}

