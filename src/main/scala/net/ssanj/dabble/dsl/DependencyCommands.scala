package net.ssanj.dabble
package dsl

import ammonite.ops._

import scalaz._
import scalaz.syntax.bind._
import DabbleHistory._
import DabbleDslDef._
import ExecutionResult2._
import DabblePaths._
import DefaultTemplate._
import CommonCommands._
import DabblePrinter._
import ResolverParser._

object DependencyCommands {

  //TODO: split this method up
  def launchDabble(line: DabbleHistoryLine): DabbleScript[ExecutionResult2] = for {
    _ <- logDabbleVersion
    dependencies        = line.dependencies.list.toList
    resolvers           = line.resolvers
    mpVersion           = line.mpVersion

    defaultSbtTemplate  = dabbleHome.path/defaultBuildFile
    outputSBTFile       = dabbleHome.work.path/defaultBuildFile

    lineSeparator       <- newlinesDS(1)
    doubleLineSepator = s"${lineSeparator}${lineSeparator}"

    sbtTemplateContent  <- fileExists(defaultSbtTemplate.toString).
                             ifM(readSbtTemplateOrDefault(defaultSbtTemplate.toString),
                                 useInMemoryTemplate)

    initialCommands     = getInitialCommands(dependencies, resolvers, mpVersion)(lineSeparator)
    sbtDependencyString = printLibraryDependency(dependencies)
    sbtResolverString   = printResolvers(resolvers)


    formattedSbtTemplateContent  = sbtTemplateContent + doubleLineSepator
    formattedSbtDependencyString = sbtDependencyString + doubleLineSepator
    formattedResolverString      = (if (resolvers.nonEmpty) (sbtResolverString + doubleLineSepator) else "")
    formattedMacroParadise       = mpVersion.map(printMacroParadise).fold("")(_ + doubleLineSepator)

    writeResult <- writeFile(outputSBTFile.toString,
                              Seq(formattedSbtTemplateContent  +
                                  formattedResolverString      +
                                  formattedSbtDependencyString +
                                  formattedMacroParadise       +
                                  initialCommands))

    result <- writeResult match {
              case -\/(error) =>
                liftDS(
                  ExecutionResult2(Option(s"couldn't create sbt file: $outputSBTFile due to: $error."),
                    UnsuccessfulAction))
              case \/-(_) => executeSbt
    }

  } yield result

  def getSBTExec: DabbleScript[String] = for {
    sbtE <- systemProp("os.name")
    sbtExec = sbtE match {
      case -\/(error) => "sbt"
      case \/-(os) if os.toLowerCase.startsWith("windows") => "sbt.bat"
      case \/-(_) => "sbt"
    }
  } yield sbtExec

  def executeSbt: DabbleScript[ExecutionResult2] = for {
    sbt <- getSBTExec
    resultE <- callProcess(sbt, "console-quick", dabbleHome.work.path.toString)
    result = resultE match {
      case -\/(error) => ExecutionResult2(Option(error), UnsuccessfulAction)
      case \/-(er) => er
    }
  } yield result

  private def getInitialCommands(dependencies: Seq[Dependency], resolvers: Seq[Resolver],
    mpVersion: Option[String])(lineSeparator: String): String = {

    val dependencyText = printLibraryDependenciesText(dependencies)

    val resolverText   = printResolversAsText(resolvers)

    val depString      = s"${lineSeparator}Dabble injected the following libraries:" +
                          s"${lineSeparator}${dependencyText}${lineSeparator}"

    val resolverString = if (resolvers.nonEmpty) {
                          s"${lineSeparator}Dabble injected the following resolvers:" +
                           s"${lineSeparator}${resolverText}${lineSeparator}"
                         } else ""

    val cpString       = mpVersion.fold("")(v => s"${lineSeparator}Dabble injected macro paradise version:" +
                                                  s" ${v}${lineSeparator}")
    val injections     = depString      +
                         resolverString +
                         cpString

    val replString     = s"""println("${injections}")"""

    s"""initialCommands := "${replEscaped(replString)}""""
  }

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

  //TODO: Figure out what to do about HLAW.
  def launchDabbleAndSaveHistory(historyFileName: String,
                                 line: DabbleHistoryLine,
                                 hlaw: HistoryLinesAndWarnings,
                                 historyPrinter: DabbleHistoryLine => String):DabbleScript[ExecutionResult2] = {
     combineEV(launchDabble(line),
               saveHistoryFile(historyFileName,
                               line,
                               hlaw,
                               historyPrinter))
  }

  //TODO: Write out log at source or error or return error in an ExecutionResult2?
  def saveHistoryFile(filename: String, selection: DabbleHistoryLine, hlaw: HistoryLinesAndWarnings,
    historyPrinter: DabbleHistoryLine => String):
    DabbleScript[ErrorOr[Unit]] = {
      import scala.collection.mutable.LinkedHashSet
      import scalaz.\&/._

      val (errors, warnings, hLines) =
        hlaw match {
          //All failures
          case This(_) =>
            val truncationNotice = s"All history lines are in error. $filename will be truncated."
            (Seq(truncationNotice), Seq.empty, Seq.empty)

          //Only successes!
          case That(successes) => (Seq.empty, Seq.empty, successes)

          //Mix of successes and failures, could be both are empt (as for empty history file)
          case Both(warnings, successes) => (Seq.empty, warnings, successes)
        }

      val uniqueHLines = LinkedHashSet() ++ (selection +: hLines)
      writeFile(filename, uniqueHLines.map(historyPrinter).toSeq) flatMap {
        case e@(-\/(_)) => liftDS(e)
        case s@(\/-(_)) =>
          if (errors.nonEmpty)
            newlinesDS(1).flatMap(nl =>
              log(s"Dabble has the following errors:${nl}${tabAsSpaces}${errors.mkString(nl + tabAsSpaces)}").map(_ => s)
            )
          else if (warnings.nonEmpty) newlinesDS(1).flatMap(nl =>
            log(s"Dabble could not parse the following history lines:${nl}${tabAsSpaces}${warnings.mkString(nl + tabAsSpaces)}")).map(_ => s)
          else liftDS(s)
      }
    }

  //program
  def launchSbtConsole(historyFileName: String,
                       line: DabbleHistoryLine,
                       argParser: CommandlineParser,
                       historyPrinter: DabbleHistoryLine => String): DabbleScript[Unit] = for {
    er2 <- loadHistoryFile(historyFileName, argParser).flatMap {
            case -\/(error) =>
              liftDS(ExecutionResult2(
                      Option(s"could not read history file: $historyFileName due to: $error"),
                      UnsuccessfulAction))
            case \/-(hlaw) => launchDabbleAndSaveHistory(historyFileName, line, hlaw, historyPrinter)
    }

    _ <- er2 match {
      case ExecutionResult2(_, SuccessfulAction) =>
        log(s"Dabble completed successfully") >> exit(er2)
      case ExecutionResult2(Some(error), UnsuccessfulAction) =>
        log(s"Dabble failed with the following error: $error") >> exit(er2)
      case ExecutionResult2(None, UnsuccessfulAction) =>
        log(s"Dabble failed with errors.") >> exit(er2)
    }
  } yield ()
}

