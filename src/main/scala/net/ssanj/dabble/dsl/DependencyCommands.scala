package net.ssanj.dabble
package dsl

import ammonite.ops._

import scalaz._
import scalaz.syntax.bind._
import DabbleHistory._
import DabbleHistoryDslDef._
import ExecutionResult2._
import DabblePaths._
import DefaultTemplate._
import CommonCommands._
import DabblePrinter._
import ResolverParser._

object DependencyCommands {

  //TODO: This should actually return a ExecutionResult2. If it succeeds then we can save history.
  def launchDabble(line: DabbleHistoryLine): DabbleScript[ExecutionResult2] = for {
    _ <- logDabbleVersion
    dependencies        = line.dependencies.list.toList
    resolvers           = line.resolvers
    mpVersion           = line.mpVersion

    defaultSbtTemplate  = dabbleHome.path/defaultBuildFile
    outputSBTFile       = dabbleHome.work.path/defaultBuildFile

    lineSeparator       <- newlinesDS(1)
    doubleLineSepator   = s"${lineSeparator}${lineSeparator}"

    sbtTemplateContent  <- fileExists(defaultSbtTemplate.toString).
                             ifM(readSbtTemplateOrDefault(defaultSbtTemplate.toString)(lineSeparator),
                                 useInMemoryTemplate(doubleLineSepator))

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

  def executeSbt: DabbleScript[ExecutionResult2] = liftDS(withResult(SuccessfulAction))

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

  def readSbtTemplateOrDefault(defaultSbtTemplate: String)(lineSeparator: String): DabbleScript[String] = {
    log(s"Using default sbt template at: ${defaultSbtTemplate}") >>
      readFile(defaultSbtTemplate).flatMap {
        case -\/(error) =>
          log(s"could not load template file due to: $error").
            flatMap(_ => useInMemoryTemplate(lineSeparator))
        case \/-(content) => liftDS(content.mkString(lineSeparator))
      }
  }

  def useInMemoryTemplate(lineSeparator: String): DabbleScript[String] = {
        log("Using in-memory sbt template. Create a build.sbt file in ~/.dabble/ to override.").
          map(_ => inMemSbtTemplateF(lineSeparator))
  }

// def genBuildFileFrom(home: DabbleHome, dependencies: Seq[Dependency], resolvers: Seq[Resolver],
//     mpVersion: Option[String]): Unit = {

//     val defaultSbtTemplate   = home.path/defaultBuildFile
//     val outputSBTFile        = home.work.path/defaultBuildFile
//     val sbtTemplateContent   =
//       if (exists(defaultSbtTemplate)) {
//         log(s"Using default sbt template at: ${defaultSbtTemplate}")
//         read(defaultSbtTemplate)
//       } else {
//         log("Using in-memory sbt template. Create a build.sbt file in ~/.dabble/ to override.")
//         inMemSbtTemplate
//       }

//     val initialCommands     = getInitialCommands(dependencies, resolvers, mpVersion)
//     val sbtDependencyString = printLibraryDependency(dependencies)
//     val sbtResolverString   = printResolvers(resolvers)

//     val formattedSbtTemplateContent  = sbtTemplateContent + newlines(2)
//     val formattedSbtDependencyString = sbtDependencyString + newlines(2)
//     val formattedResolverString      = (if (resolvers.nonEmpty) (sbtResolverString + newlines(2)) else "")
//     val formattedMacroParadise       = mpVersion.map(printMacroParadise).fold("")(_ + newlines(2))

//     write.over (outputSBTFile,
//                 formattedSbtTemplateContent  +
//                 formattedResolverString      +
//                 formattedSbtDependencyString +
//                 formattedMacroParadise       +
//                 initialCommands)
//   }

  def logDabbleVersion: DabbleScript[Unit] = {
    log(s"${DabbleInfo.version}-b${DabbleInfo.buildInfoBuildNumber}")
  }

  def launchDabbleAndSaveHistory(historyFileName: String,
                                 line: DabbleHistoryLine,
                                 hlaw: HistoryLinesAndWarnings,
                                 historyPrinter: DabbleHistoryLine => String):DabbleScript[ExecutionResult2] = {
     combineEV(launchDabble(line),
               saveHistoryFile(historyFileName,
                               line,
                               hlaw.onlyThat.getOrElse(Seq.empty),
                               historyPrinter))
  }

  def saveHistoryFile(filename: String, selection: DabbleHistoryLine, hLines: Seq[DabbleHistoryLine],
    historyPrinter: DabbleHistoryLine => String):
    DabbleScript[ErrorOr[Unit]] = {
      import scala.collection.mutable.LinkedHashSet
      val uniqueHLines = LinkedHashSet() ++ (selection +: hLines)
      writeFile(filename, uniqueHLines.map(historyPrinter).toSeq)
    }

}

