package net.ssanj.dabble

import scala.util.Try
import ammonite.ops._
import scalaz._
import scalaz.syntax.std.`try`._

object DabbleApp extends DependencyParser with DependencyPrinter with TerminalSupport {

  private val inMemSbtTemplate = """name := "Dabble"

organization := "biz.awesome"

version := "0.0.1"

scalaVersion := "2.11.7""""

  private val userHomePath = Path(userHome)
  private final case class ExecutionResult(message: Option[String], code: Int)

  private final case class DabbleWork(path: Path)
  private final case class DabbleTemplates(path: Path)

  private final case class DabbleHome(path: Path) {
    def work = DabbleWork(path/'work)
    def templates = DabbleTemplates(path/'templates)
  }

  private val dabbleHome = DabbleHome(userHomePath/".dabble")

  def main(args: Array[String]) {
    parser.parse(args, DabbleRunConfig()) match {
      case Some(DabbleRunConfig(deps)) =>
        val result = parse(deps)fold(processingFailed, build)
        exit(result)
      case None => exit(processingFailed("Could not parse dependencies. Please run with --help for usage."))
    }
  }

  private def exit(result: ExecutionResult): Unit = {
    result.message.foreach(m => log(m))
    System.exit(result.code)
  }

  private def processingFailed(error: String): ExecutionResult = ExecutionResult(Option(error), 1)

  private def build(dependencies: Seq[Dependency]): ExecutionResult = {
    Try {

      genBuildFileFrom(dabbleHome, dependencies)

      val result = %(getSBTExec, "console")(dabbleHome.work.path)
      ExecutionResult(if (result == 0) Option("Dabble completed successfully.") else Option("Could not launch console. See SBT output for details."), result)
    }.toDisjunction.fold(x => ExecutionResult(Option(s"Could not launch console due to: ${x.getMessage}"), 1), identity)
  }

  private def genBuildFileFrom(home: DabbleHome, dependencies: Seq[Dependency]): Unit = {
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

      val dependencyText      = printText(dependencies)
      val initialCommands     = s"""initialCommands := "println(\\"Dabble loaded:${escapedNewline}${dependencyText}\\")" """
      val sbtDependencyString = print(dependencies)

     write.over (outputSBTFile, sbtTemplateContent + newline + newline + sbtDependencyString + newline + newline + initialCommands)
  }

  private def getSBTExec = if (System.getProperty("os.name").toLowerCase.startsWith("windows")) "sbt.bat" else "sbt"

  private val title = s"Welcome to dabble version: ${DabbleInfo.version}-b${DabbleInfo.buildInfoBuildNumber}"

  private def log(messages: String*): Unit = println(messages.mkString(newline))

}

