package net.ssanj.dabble

import scalaz.Show
import net.ssanj.dabble.Implicits._

/**
 * Functions-related to printing Dependencies.
 */
trait DabblePrinter {
  /**
   * Returns the libraryDependency String used by SBT.
   *
   * Example:
   *
   * {{{
   * libraryDependencies ++= Seq(
   *   "org.scalaz"     %% "scalaz-core"  % "7.1.4",
   *   "org.scalatest"  %% "scalatest"    % "2.2.4"
   * )
   * }}}
   *
   * @param deps The Seq[Dependency] to make into a String
   * @return the dependency String.
   */
  def printLibraryDependency(deps: Seq[Dependency]): String = {
    val depStrings = deps.map(Show[Dependency].shows)
    val formatted  = depStrings.mkString(s",$newline$tabAsSpaces")
    s"libraryDependencies ++= Seq($newline$tabAsSpaces" +
    formatted.mkString +
    s"$newline)"
  }

  /**
   * Returns the resolvers String used by SBT.
   *
   * Example:
   *
   * {{{
   * resolvers ++= Seq(
   *   "bmjames" at "https://dl.bintray.com/bmjames/maven",
   *   "Resolver.sonatypeRepo("releases")"
   * )
   * }}}
   *
   * @param deps The Seq[Dependency] to make into a String
   * @return the dependency String.
   */
  def printResolvers(resolvers: Seq[Resolver]): String = {
    val resolverString = resolvers.map(Show[Resolver].shows)
    val formatted = resolverString.mkString(s",$newline$tabAsSpaces")
    s"resolvers ++= Seq($newline$tabAsSpaces" +
      formatted.mkString +
    s"$newline)"
  }

  /**
   * Returns the text-only representation of the dependencies supplied. Each dependency
   * is processed as follows:
   *
   * 1. An index number is added as a prefix.
   * 2. Quotes are removed.
   * 3. Dependencies are separated by a newline.
   *
   * Example:
   *
   * {{{
   * [1] org.scalaz %% scalaz-core % 7.1.4
   * [2] org.scalatest %% scalatest % 2.2.4
   * }}}
   *
   * This is to be used primarily for displaying the libraries used from within the SBT console.
   *
   * @param deps The dependencies to make into text.
   * @return the textual representation.
   *
   */
  def printLibraryDependenciesText(deps: Seq[Dependency]): String =  {
    val depStrings = deps.map(Show[Dependency].shows).map(_.replace("\"", ""))
    depStrings.zipWithIndex.map{ case (d, i) => s"[${i+1}] $d" }.mkString(newline)
  }

/**
   * Returns the text-only representation of the resolvers supplied. Each resolver
   * is formatted as supplied with extraneous padding removed.
   *
   *
   * Example:
   *
   * {{{
   * [1] sonatype:r
   * [2] bintray:someuser:somerepo
   * }}}
   *
   * This is to be used primarily for displaying the libraries used from within the SBT console.
   *
   * @param resolvers The resolvers to make into text.
   * @return the textual representation.
   *
   */
  def printResolversAsText(resolvers: Seq[Resolver]): String = {
    val resolverStrings = resolvers.map(r => Show[ResolverString].shows(ResolverString(r)))
    resolverStrings.zipWithIndex.map { case (r, i) => s"[${i+1}] $r" }.mkString(newline)
  }

  /**
   * Double escapes content for displaying in the REPLs initialCommands String.
   * Escapes the following: newlines, carriage returns, tabs, double quotes.
   */
  def replEscaped(value: String): String = value.replace("\n", "\\\\n").
                                                 replace("\r", "\\\\r").
                                                 replace("\t", "\\\\t").
                                                 replace("\"", "\\\"")

  /** Prints initial commands on launching SBT.
    *
    * @example {{{
    * Dabble injected the following libraries:
    * [1] org.scalaz %% scalaz-core % 7.1.4
    * [2] org.scalatest %% scalatest % 2.2.4
    * }}}
    *
    * @param dependencies Dependencies
    * @param resolvers Resolvers
    * @param mpVersion Macro paradise version
    */
  def printInitialSbtCommands(dependencies: Seq[Dependency], resolvers: Seq[Resolver],
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

  def printMacroParadise(version: String): String =
    s"""addCompilerPlugin("org.scalamacros" % "paradise" % "${version}" cross CrossVersion.full)"""


  def formatSbtTemplate(sbtTemplateContent: String, line: DabbleHistoryLine): String = {
    val dependencies        = line.dependencies.list.toList
    val resolvers           = line.resolvers
    val mpVersion           = line.mpVersion

    val doubleLineSepator   = s"${newline}${newline}"
    val initialCommands     = printInitialSbtCommands(dependencies, resolvers, mpVersion)

    val sbtDependencyString = printLibraryDependency(dependencies)
    val sbtResolverString   = printResolvers(resolvers)

    val formattedSbtTemplateContent  = sbtTemplateContent + doubleLineSepator
    val formattedSbtDependencyString = sbtDependencyString + doubleLineSepator
    val formattedResolverString      = (if (resolvers.nonEmpty) (sbtResolverString + doubleLineSepator) else "")
    val formattedMacroParadise       = mpVersion.map(printMacroParadise).fold("")(_ + doubleLineSepator)

    formattedSbtTemplateContent  +
    formattedResolverString      +
    formattedSbtDependencyString +
    formattedMacroParadise       +
    initialCommands
  }


  def printHistoryLines(lines: Seq[DabbleHistoryLine]): String =
    lines.map(printHistoryLine).mkString(newline)

  def printHistoryLine(line: DabbleHistoryLine): String = {

      val depsString =
        line.dependencies.map(d => Show[DependencyHistoryString].shows(DependencyHistoryString(d))).list.toList.mkString(" + ")

      val resOp =
        if (line.resolvers.isEmpty) None
        else Option(line.resolvers.
                      map(r => Show[ResolverString].shows(ResolverString(r))).
                      mkString(",")).map(r => s"-r ${r}")

        val mpvOp = line.mpVersion.map(v => s"$v").map(v => s"-mp $v")

      Seq(Option(depsString),
          resOp,
          mpvOp).
        flatten.
        mkString(" ")
  }
}

object DabblePrinter extends DabblePrinter