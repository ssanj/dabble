package net.ssanj.dabble

import scalaz.Show
import net.ssanj.dabble.Implicits._

/**
 * Functions-related to printing Dependencies.
 */
trait DependencyPrinter {
  /**
   * Returns the libraryDependency String used by SBT.
   *
   * Example:
   *
   * {{{
   * libraryDependencies ++= Seq(
   * "org.scalaz"     %% "scalaz-core"  % "7.1.4",
   * "org.scalatest"  %% "scalatest"    % "2.2.4"
   * )
   * }}}
   *
   * @param deps The Seq[Dependency] to make into a String
   * @return the dependency String.
   */
  def printLibraryDependency(deps: Seq[Dependency]): String = {
    val newline = System.getProperty("line.separator")
    val tab = "  "

    val depStrings = deps.map(Show[Dependency].shows)
    val formatted = depStrings.init.map(_ + s",$newline$tab") :+ depStrings.last
    s"libraryDependencies ++= Seq($newline$tab" +
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
  def printText(deps: Seq[Dependency]): String =  {
    val depStrings = deps.map(Show[Dependency].shows).map(_.replace("\"", ""))
    depStrings.zipWithIndex.map{ case (d, i) => s"[${i+1}] $d" }.mkString(escapedNewline)
  }
}