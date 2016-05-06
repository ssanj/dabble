package net.ssanj.dabble

import scala.util.Try
import ammonite.ops._
import scalaz._
import scalaz.syntax.std.`try`._

trait Executor { self: DefaultTemplate with DabblePrinter =>

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

  protected def build(dependencies: Seq[Dependency], resolvers: Seq[Resolver]): ExecutionResult = {
    Try {
      log(s"${DabbleInfo.version}-b${DabbleInfo.buildInfoBuildNumber}")
      genBuildFileFrom(dabbleHome, dependencies, resolvers)

      val result = %(getSBTExec, "console-quick")(dabbleHome.work.path)
      ExecutionResult(if (result == 0) Option("Dabble completed successfully.") else Option("Could not launch console. See SBT output for details."), result)
    }.toDisjunction.fold(x => ExecutionResult(Option(s"Could not launch console due to: ${x.getMessage}"), 1), identity)
  }

  protected def genBuildFileFrom(home: DabbleHome, dependencies: Seq[Dependency], resolvers: Seq[Resolver]): Unit = {
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

    val initialCommands     = getInitialCommands(dependencies, resolvers)
    val sbtDependencyString = printLibraryDependency(dependencies)
    val sbtResolverString   = printResolvers(resolvers)

    val formattedSbtTemplateContent  = sbtTemplateContent + newlines(2)
    val formattedSbtDependencyString = sbtDependencyString + newlines(2)
    val formattedResolverString      = (if (resolvers.nonEmpty) (sbtResolverString + newlines(2)) else "")

    write.over (outputSBTFile,
                formattedSbtTemplateContent  +
                formattedResolverString      +
                formattedSbtDependencyString +
                initialCommands)
  }

  private def getInitialCommands(dependencies: Seq[Dependency], resolvers: Seq[Resolver]): String = {
    val dependencyText = printLibraryDependenciesText(dependencies)
    val resolverText   = printResolversAsText(resolvers)

    val depString      = s"Dabble injected the following libraries:${newline}${dependencyText}${newline}"
    val resolverString = s"Dabble injected the following resolvers:${newline}${resolverText}${newline}"
    val injections     = newline + depString + (if (resolvers.nonEmpty) newline + resolverString else "")
    val replString     = s"""println("${injections}")"""

    s"""initialCommands := "${replEscaped(replString)}""""
  }

  def getSBTExec = if (System.getProperty("os.name").toLowerCase.startsWith("windows")) "sbt.bat" else "sbt"
}