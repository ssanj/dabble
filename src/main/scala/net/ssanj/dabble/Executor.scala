package net.ssanj.dabble

import scala.util.Try
import ammonite.ops._
import scalaz._
import scalaz.syntax.std.`try`._

trait Executor { self: DefaultTemplate with DependencyPrinter =>

  val userHomePath = Path(userHome)
  protected case class ExecutionResult(message: Option[String], code: Int)

  protected case class DabbleWork(path: Path)
  protected case class DabbleTemplates(path: Path)

  protected case class DabbleHome(path: Path) {
    def work = DabbleWork(path/'work)
    def templates = DabbleTemplates(path/'templates)
  }

  protected val dabbleHome = DabbleHome(userHomePath/".dabble")

  protected def exit(result: ExecutionResult): Unit = {
    result.message.foreach(m => log(m))
    System.exit(result.code)
  }

  protected def processingFailed(error: String): ExecutionResult = ExecutionResult(Option(error), 1)

  protected val processingFailed: ExecutionResult = ExecutionResult(None, 1)

  protected def build(dependencies: Seq[Dependency]): ExecutionResult = {
    Try {

      genBuildFileFrom(dabbleHome, dependencies)

      val result = %(getSBTExec, "console")(dabbleHome.work.path)
      ExecutionResult(if (result == 0) Option("Dabble completed successfully.") else Option("Could not launch console. See SBT output for details."), result)
    }.toDisjunction.fold(x => ExecutionResult(Option(s"Could not launch console due to: ${x.getMessage}"), 1), identity)
  }

  protected def genBuildFileFrom(home: DabbleHome, dependencies: Seq[Dependency]): Unit = {
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
      val initialCommands     = s"""initialCommands := "println(\\"${escapedNewline}Dabble injected the following libraries:${escapedNewline}${dependencyText}${escapedNewline}\\")""""
      val sbtDependencyString = print(dependencies)

     write.over (outputSBTFile, sbtTemplateContent + newline + newline + sbtDependencyString + newline + newline + initialCommands)
  }

  def getSBTExec = if (System.getProperty("os.name").toLowerCase.startsWith("windows")) "sbt.bat" else "sbt"
}