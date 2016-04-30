package net.ssanj.dabble

import scalaz.Show
import net.ssanj.dabble.Implicits._

/**
 * Functions-related to printing Dependencies.
 */
trait DependencyPrinter {
  /**
   * Prints out the libraryDependency String used by SBT.
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
  def print(deps: Seq[Dependency]): String = {
    val newline = System.getProperty("line.separator")
    val tab = "  "

    val depStrings = deps.map(Show[Dependency].shows)
    val formatted = depStrings.init.map(_ + s",$newline$tab") :+ depStrings.last
    s"libraryDependencies ++= Seq($newline$tab" +
    formatted.mkString +
    s"$newline)"
  }

  def printText(deps: Seq[Dependency]): String =  {
    val depStrings = deps.map(Show[Dependency].shows).map(_.replace("\"", ""))
    depStrings.zipWithIndex.map{ case (d, i) => s"[${i+1}] $d" }.mkString(escapedNewline)
  }
}