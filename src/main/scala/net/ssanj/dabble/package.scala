package net.ssanj

import scalaz.Show
import scalaz.Show.shows

package object dabble {

  object Implicits {
    implicit val dependencyShows: Show[Dependency] = shows {
      case ScalaVersionSupplied(org, name, version, _) => s""""${org}" % "${name}" % "$version""""
      case ScalaVersionDerived (org, name, version, _) => s""""${org}" %% "${name}" % "$version""""
    }
  }

  val newline           = System.getProperty("line.separator")
  val userHome          = System.getProperty("user.home")
  val tab               = "\t"
  val escapedNewline    = "\\\\n"
  val defaultBuildFile  = "build.sbt"
  lazy val title        = s"${DabbleInfo.name} version: ${DabbleInfo.version}-b${DabbleInfo.buildInfoBuildNumber}"

  def log(messages: String*): Unit = println(s"${DabbleInfo.name}: ${messages.mkString(newline)}")
}