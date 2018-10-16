package net.ssanj.dabble

import scala.util.Try
import ammonite.ops._
import scalaz._
import scalaz.NonEmptyList.nels
import scalaz.syntax.std.`try`._
import \&/.{This, That, Both}
import DabblePrinter._
import DabbleHistory._
import DabblePaths._
import DefaultTemplate._
import TerminalSupport._

trait Executor {
  type Outcome = Seq[String] \&/ Unit

  def exit(result: ExecutionResult): Unit = {
    result.message.foreach(m => log(m))
    System.exit(result.code)
  }

  def processingFailed(error: String): ExecutionResult = ExecutionResult(Option(error), 1)

  val processingFailed: ExecutionResult = ExecutionResult(None, 1)

  def build(dependencies: Seq[Dependency],
            resolvers: Seq[Resolver],
            mpVersion: Option[String],
            hlaw: HistoryLinesAndWarnings): ExecutionResult = {
    Try {
      log(s"${DabbleInfo.version}-b${DabbleInfo.buildInfoBuildNumber}")
      genBuildFileFrom(dabbleHome, dependencies, resolvers, mpVersion)

      val result = Try(%(getSBTExec, "consoleQuick")(dabbleHome.work.path)).fold(_ => -1, _ => 0)

      val (message, code) =
        if (result == 0) {
          writeToHistoryFile(dependencies, resolvers, mpVersion, hlaw) match {
            case This(errors)      => (s"Could not update history file ${dabbleHome.history}," +
                                       s" due to the following errors: ${errors.mkString(newline)}", -1)

            case That(_)           => ("Dabble completed successfully.", result)

            case Both(warnings, _) => ("Dabble completed successfully" +
                                       ", but had the following warnings: " +
                                       s"${warnings.mkString(newline)}", result)
          }
        } else ("Could not launch console. See SBT output for details.", result)

      ExecutionResult(Option(message), code)
    }.toDisjunction.fold(x => ExecutionResult(Option(s"Could not launch console due to: ${x.getMessage}"), 1), identity(_))
  }

  def readFile(filename: Path): Seq[String] = {
      Try(read.lines(filename, "UTF-8")).
        toOption.
        getOrElse(Seq.empty[String])
  }

  def readHistoryFileWithWarnings(): HistoryLinesAndWarnings = {
    val lines = readFile(dabbleHome.history)
    readHistoryWithWarnings(historyParser.parse(_, DabbleRunConfig()))(lines)
  }


  private def writeToHistoryFile(dependencies: Seq[Dependency],
                                 resolvers: Seq[Resolver],
                                 mpVersion: Option[String],
                                 hlaw: HistoryLinesAndWarnings): Outcome = {
    val newHistoryLine = DabbleHistoryLine(nels(dependencies.head, dependencies.tail:_*), resolvers, mpVersion)

    import scala.collection.mutable.LinkedHashSet
    val unqiueHistoryLines = LinkedHashSet() ++ (newHistoryLine +: hlaw.onlyThat.getOrElse(Seq.empty))

    //TODO: Encode errors and warnings separately.
    //sealed trait Failure
    //final case class Error(message: String) extends Failure
    //final case class Warning(message: String) extends Failure
    Try(write.over(dabbleHome.history, printHistoryLines(unqiueHistoryLines.toSeq))).
      cata(
        {_ => hlaw.bimap(identity, _ => ()) },
        e  => hlaw.bimap(_ => Seq(e.getMessage), _ => ())
      )
  }

  def genBuildFileFrom(home: DabbleHome, dependencies: Seq[Dependency], resolvers: Seq[Resolver],
    mpVersion: Option[String]): Unit = {

    val defaultSbtTemplate   = home.path/buildFile
    val outputSBTFile        = home.work.path/buildFile
    val sbtTemplateContent   =
      if (exists(defaultSbtTemplate)) {
        log(s"Using default sbt template at: ${defaultSbtTemplate}")
        read(defaultSbtTemplate)
      } else {
        log("Using in-memory sbt template. Create a build.sbt file in ~/.dabble/ to override.")
        inMemSbtTemplate(newlines(2))
      }

    val initialCommands     = getInitialCommands(dependencies, resolvers, mpVersion)
    val sbtDependencyString = printLibraryDependency(dependencies)
    val sbtResolverString   = printResolvers(resolvers)

    val formattedSbtTemplateContent  = sbtTemplateContent + newlines(2)
    val formattedSbtDependencyString = sbtDependencyString + newlines(2)
    val formattedResolverString      = (if (resolvers.nonEmpty) (sbtResolverString + newlines(2)) else "")
    val formattedMacroParadise       = mpVersion.map(printMacroParadise).fold("")(_ + newlines(2))

    write.over (outputSBTFile,
                formattedSbtTemplateContent  +
                formattedResolverString      +
                formattedSbtDependencyString +
                formattedMacroParadise       +
                initialCommands)
  }

  private def getInitialCommands(dependencies: Seq[Dependency], resolvers: Seq[Resolver],
    mpVersion: Option[String]): String = {

    val dependencyText = printLibraryDependenciesText(dependencies)

    val resolverText   = printResolversAsText(resolvers)

    val depString      = s"${newline}Dabble injected the following libraries:" +
                          s"${newline}${dependencyText}${newline}"

    val resolverString = if (resolvers.nonEmpty) {
                          s"${newline}Dabble injected the following resolvers:" +
                           s"${newline}${resolverText}${newline}"
                         } else ""

    val cpString       = mpVersion.fold("")(v => s"${newline}Dabble injected macro paradise version:" +
                                                  s" ${v}${newline}")
    val injections     = depString      +
                         resolverString +
                         cpString

    val replString     = s"""println("${injections}")"""

    s"""initialCommands := "${replEscaped(replString)}""""
  }

  def getSBTExec: String =
    if (System.getProperty("os.name").toLowerCase.startsWith("windows")) "sbt.bat"
    else "sbt"
}

object Executor extends Executor
