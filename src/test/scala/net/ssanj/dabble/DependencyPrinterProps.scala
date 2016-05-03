package net.ssanj.dabble

import org.scalacheck.Properties
import org.scalacheck.{Prop, Gen}
import org.scalacheck.Prop.BooleanOperators
import scalaz.\/-

object DependencyPrinterProps             extends
       Properties("A Dependency Printer") with
       DependencyPrinter                  with
       DabbleProps {

  /**
   * Example format:
   *
   * libraryDependencies ++= Seq(
   *  "org.scalaz"  %% "scalaz-core"  % "7.1.4",
   *  "com.lihaoyi" %% "ammonite-ops" % "0.5.7"
   * )
   */
  property("prints valid libraryDependencies") =
    Prop.forAll(genDependencies) { deps: Seq[Dependency] =>
        val tab = "  "
        val output = printLibraryDependency(deps)

        val dependencyLines = deps map {
          case ScalaVersionSupplied(org, name, version, _) => s""""${org}" % "${name}" % "${version}""""
          case ScalaVersionDerived (org, name, version, _) => s""""${org}" %% "${name}" % "${version}""""
        }

        val dependencyString =
          s"libraryDependencies ++= Seq($newline" +
          dependencyLines.map(s"$tab" + _).mkString("," + newline) +
          s"${newline})"

        (dependencyString == output) :| labeled(dependencyString, output)
    }

 property("prints text representation of dependencies") =
  Prop.forAll(genDependencies) { deps: Seq[Dependency] =>
    val output = printText(deps)

    val lines =
      deps.map {
        case ScalaVersionSupplied(org, name, version, _) => s"$org % $name % $version"
        case ScalaVersionDerived (org, name, version, _) => s"$org %% $name % $version"
      }

    val text = lines.zipWithIndex.map { case (l, i) => s"[${i+1}] $l" }.mkString(escapedNewline)
    (text == output) :| labeled(text, output)
  }
}