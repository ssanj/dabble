package net.ssanj

import scalaz.Show
import scalaz.Show.shows

package object dabble {

  object Implicits {
    implicit val dependencyShows: Show[Dependency] = shows {
      case ScalaVersionSupplied(org, name, version) => s""""${org}" % "${name}" % "$version""""
      case ScalaVersionDerived (org, name, version) => s""""${org}" %% "${name}" % "$version""""
    }
  }
}