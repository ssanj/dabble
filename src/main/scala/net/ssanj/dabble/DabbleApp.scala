package net.ssanj.dabble

import scala.util.Try
import ammonite.ops._
import scalaz._
import scalaz.syntax.std.`try`._

object DabbleApp extends DependencyParser with DependencyPrinter {

  val templateSBTFile = """name := "Dabble"

organization := "biz.awesome"

version := "0.0.1"

scalaVersion := "2.11.7""""

  private case class ExecutionResult(message: Option[String], code: Int)

  def main(args: Array[String]) {
    println(title)
    val result = parse(args).map(print).fold(processingFailed, build)
    exit(result)
  }

  private def exit(result: ExecutionResult): Unit = {
    result.message.foreach(m => println(m))
    System.exit(result.code)
  }

  private def processingFailed(error: String): ExecutionResult = ExecutionResult(Option(error), 1)

  private def build(dependencies: String): ExecutionResult = {
    Try {
      val dabbleHome = Path(System.getProperty("user.home"))/".dabble"
      val dabbleWork = dabbleHome/'work
      val newLine = System.getProperty("line.separator")

      write.over (dabbleWork/"build.sbt", templateSBTFile + newLine + newLine + dependencies)
      val result = %(getSBTExec, "console")(dabbleWork)
      ExecutionResult(if (result == 0) Option("Dabble completed successfully.") else Option("Could not launch console. See SBT output for details."), result)
    }.toDisjunction.fold(x => ExecutionResult(Option(s"Could not launch console due to: ${x.getMessage}"), 1), identity)
  }

  private def getSBTExec = if (System.getProperty("os.name").toLowerCase.startsWith("windows")) "sbt.bat" else "sbt"

  private val title = s"Dabble version: ${DabbleInfo.version}-${DabbleInfo.buildInfoBuildNumber}"

}

