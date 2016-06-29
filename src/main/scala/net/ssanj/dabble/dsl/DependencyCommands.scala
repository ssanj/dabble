package net.ssanj.dabble
package dsl

import ammonite.ops._

import scalaz._
import scalaz.syntax.bind._
import DabbleHistory._
import DabbleDslDef._
import DabbleResult._
import DabblePaths._
import DefaultTemplate._
import CommonCommands._
import DabblePrinter._
import ResolverParser._

object DependencyCommands {

  //TODO: split this method up
  def launchDabble(line: DabbleHistoryLine): DabbleScript[ErrorOr[Unit]] = for {
    lineSeparator       <- newlinesDS(1)
    dependencies        = line.dependencies.list.toList
    resolvers           = line.resolvers
    mpVersion           = line.mpVersion

    defaultSbtTemplate  = dabbleHome.path/defaultBuildFile
    outputSBTFile       = dabbleHome.work.path/defaultBuildFile

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

    _ <- writeFile(outputSBTFile.toString,
              Seq(formattedSbtTemplateContent  +
                  formattedResolverString      +
                  formattedSbtDependencyString +
                  formattedMacroParadise       +
                  initialCommands))

   result <- executeSbt

  } yield result

  def getSBTExec: DabbleScript[String] = for {
    sbtE <- systemProp("os.name")
    sbtExec = sbtE match {
      case -\/(error) => "sbt"
      case \/-(os) if os.toLowerCase.startsWith("windows") => "sbt.bat"
      case \/-(_) => "sbt"
    }
  } yield sbtExec

  def executeSbt: DabbleScript[ErrorOr[Unit]] = for {
    sbt <- getSBTExec
    result <- callProcess(sbt, "console-quick", dabbleHome.work.path.toString)
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
                                 historyPrinter: DabbleHistoryLine => String):DabbleScript[ErrorOr[Unit]] = {
    for {
     _      <- launchDabble(line)
     result <- saveHistoryFile(historyFileName,
                        line,
                        hlaw,
                        historyPrinter)
    } yield result
  }

  //TODO: Write out log at source or error or return error in an ExecutionResult2?
  def saveHistoryFile(filename: String, selection: DabbleHistoryLine, hlaw: HistoryLinesAndWarnings,
    historyPrinter: DabbleHistoryLine => String):
    DabbleScript[ErrorOr[Unit]] = {
      import scala.collection.mutable.LinkedHashSet
      import scalaz.\&/._

      val hLines =
        hlaw match {
          //All failures
          case This(_) => Seq.empty

          //Only successes!
          case That(successes) => successes

          //Mix of successes and failures, could be both are empty (as for empty history file)
          case Both(_, successes) => successes
        }

      val uniqueHLines = LinkedHashSet() ++ (selection +: hLines)
      writeFile(filename, uniqueHLines.map(historyPrinter).toSeq)
    }

  //program
  def launchSbtConsole(historyFileName: String,
                       line: DabbleHistoryLine,
                       argParser: CommandlineParser,
                       historyPrinter: DabbleHistoryLine => String): DabbleScript[DabbleResult] =
    loadHistoryFile(historyFileName, argParser).flatMap {
        case -\/(error) =>
          liftDS(dabbleFailure(s"could not read history file: $historyFileName due to: $error"))

        case \/-(hlaw) =>
          for {
            _ <- logDabbleVersion
            //we should resolve any errors from HLAW here -we could just dump the warnings and errors here
            result <- launchDabbleAndSaveHistory(historyFileName, line, hlaw, historyPrinter)
            warnings = hlaw.fold(identity, _ => Seq.empty[String], (l, r) => l)
          } yield result.fold(l => dabbleFailure(l, warnings:_*),
                              _ => dabbleSuccess(warnings))
    }
}

