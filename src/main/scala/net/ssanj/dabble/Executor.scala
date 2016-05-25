package net.ssanj.dabble

import scala.util.Try
import ammonite.ops._
import scalaz._
import scalaz.NonEmptyList.nels
import scalaz.syntax.std.`try`._
import scalaz.syntax.either._
import scalaz.syntax.applicative._

final case class Outcome[+E, +A](values: Seq[A], warnings: Seq[E]) {
  val hasValues: Boolean = values.nonEmpty
  val hasWarnings: Boolean = warnings.nonEmpty
  val hasBoth: Boolean = hasValues && hasWarnings
}

object Outcome {
  def values[E, A](values: Seq[A]): Outcome[E, A] =
    Outcome(values = values, warnings = Seq.empty[E])

  def warnings[E, A](warnings: Seq[E]): Outcome[E, A] =
    Outcome(values = Seq.empty[A], warnings = warnings)

  def both[E, A](values: Seq[A], warnings: Seq[E]): Outcome[E, A] =
    Outcome(values = values, warnings = warnings)
}

trait Executor { self: DefaultTemplate with
                       DabblePrinter   with
                       DabblePaths     with
                       DabbleHistory   with
                       TerminalSupport =>

  protected case class ExecutionResult(message: Option[String], code: Int)

  protected def exit(result: ExecutionResult): Unit = {
    result.message.foreach(m => log(m))
    System.exit(result.code)
  }

  protected def processingFailed(error: String): ExecutionResult = ExecutionResult(Option(error), 1)

  protected val processingFailed: ExecutionResult = ExecutionResult(None, 1)

  protected def build(dependencies: Seq[Dependency], resolvers: Seq[Resolver], mpVersion: Option[String]): ExecutionResult = {
    Try {
      log(s"${DabbleInfo.version}-b${DabbleInfo.buildInfoBuildNumber}")
      genBuildFileFrom(dabbleHome, dependencies, resolvers, mpVersion)

      val result = %(getSBTExec, "console-quick")(dabbleHome.work.path)

      val (message, code) =
        if (result == 0) {
          writeToHistoryFile(dependencies, resolvers, mpVersion).
            fold(l => (s"Could not update history file ${dabbleHome.history}," +
                        s" due to the following error: $l", -1),
                 o => ("Dabble completed successfully" +
                        (if (o.hasWarnings)
                          s", but had the following warnings: ${o.warnings.mkString(newline)}"
                         else ".") , result))
        } else ("Could not launch console. See SBT output for details.", result)

      ExecutionResult(Option(message), code)
    }.toDisjunction.fold(x => ExecutionResult(Option(s"Could not launch console due to: ${x.getMessage}"), 1), identity)
  }

  private def readHistoryFile(): HistoryLinesOr = {
    val lines =
      Try(read.lines(dabbleHome.history, "UTF-8")).
        toOption.
        getOrElse(Seq.empty[String])

    readHistory(historyParser.parse(_, DabbleRunConfig()))(lines)
  }

  private def writeToHistoryFile(dependencies: Seq[Dependency],
                                 resolvers: Seq[Resolver],
                                 mpVersion: Option[String]): String \/ Outcome[String, Nothing] = {
    val historyLines   = readHistoryFile() //Seq[ValidationNel[String, DabbleHistoryLine]]
    val newHistoryLine = DabbleHistoryLine(nels(dependencies.head, dependencies.tail:_*), resolvers, mpVersion)

    val oldHistoryLines: Seq[DabbleHistoryLine] = historyLines.collect { case Success(lines) => lines }
    val historyLineWarnings: Seq[String] = historyLines.collect { case Failure(warnings) => warnings.list.toList }.flatten

    Try(write.over(dabbleHome.history, printHistoryLines(newHistoryLine +: oldHistoryLines))).
      cata(_ => Outcome.warnings[String, Nothing](warnings = historyLineWarnings).right[String],
           e => e.getMessage.left[Outcome[String, Nothing]])
  }

  protected def genBuildFileFrom(home: DabbleHome, dependencies: Seq[Dependency], resolvers: Seq[Resolver],
    mpVersion: Option[String]): Unit = {

    val defaultSbtTemplate   = home.path/defaultBuildFile
    val outputSBTFile        = home.work.path/defaultBuildFile
    val sbtTemplateContent   =
      if (exists(defaultSbtTemplate)) {
        log(s"Using default sbt template at: ${defaultSbtTemplate}")
        read(defaultSbtTemplate)
      } else {
        log("Using in-memory sbt template. Create a build.sbt file in ~/.dabble/ to override.")
        inMemSbtTemplate
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

  def getSBTExec = if (System.getProperty("os.name").toLowerCase.startsWith("windows")) "sbt.bat" else "sbt"
}