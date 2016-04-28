package net.ssanj.dabble

import scala.util.Try
import ammonite.ops._
import scalaz._
import scalaz.syntax.std.`try`._

object DabbleApp extends DependencyParser with DependencyPrinter {

  private val inMemSbtTemplate = """name := "Dabble"

organization := "biz.awesome"

version := "0.0.1"

scalaVersion := "2.11.7""""

  private val newline  = System.getProperty("line.separator")
  private val userHome = Path(System.getProperty("user.home"))

  private final case class ExecutionResult(message: Option[String], code: Int)

  private final case class DabbleWork(path: Path)
  private final case class DabbleTemplates(path: Path)

  private final case class DabbleHome(path: Path) {
    def work = DabbleWork(path/'work)
    def templates = DabbleTemplates(path/'templates)
  }

  private val dabbleHome = DabbleHome(userHome/".dabble")

  def main(args: Array[String]) {
    log(title)
    val result = parse(args).map(print).fold(processingFailed, build)
    exit(result)
  }

  private def exit(result: ExecutionResult): Unit = {
    result.message.foreach(m => log(m))
    System.exit(result.code)
  }

  private def processingFailed(error: String): ExecutionResult = ExecutionResult(Option(error), 1)

  private def build(dependencies: String): ExecutionResult = {
    Try {

      genBuildFileFrom(dabbleHome, dependencies)

      val result = %(getSBTExec, "console")(dabbleHome.work.path)
      ExecutionResult(if (result == 0) Option("Dabble completed successfully.") else Option("Could not launch console. See SBT output for details."), result)
    }.toDisjunction.fold(x => ExecutionResult(Option(s"Could not launch console due to: ${x.getMessage}"), 1), identity)
  }

  private def genBuildFileFrom(home: DabbleHome, dependencies: String): Unit = {
    val defaultSbtTemplate   = home.path/"build.sbt"
    val outputSBTFile        = home.work.path/"build.sbt"
    val sbtTemplateContent   =
      if (exists(defaultSbtTemplate)) {
        log(s"Using default sbt template at: ${defaultSbtTemplate}")
        read(defaultSbtTemplate)
      } else {
        log("Using in-memory sbt template. Create a build.sbt file in ~/.dabble/ to override.")
        inMemSbtTemplate
      }

     write.over (outputSBTFile, sbtTemplateContent + newline + newline + dependencies)
  }

  private def getSBTExec = if (System.getProperty("os.name").toLowerCase.startsWith("windows")) "sbt.bat" else "sbt"

  private val title = s"Welcome to dabble version: ${DabbleInfo.version}-b${DabbleInfo.buildInfoBuildNumber}"

  private def log(messages: String*): Unit = println(messages.mkString(newline))

}

