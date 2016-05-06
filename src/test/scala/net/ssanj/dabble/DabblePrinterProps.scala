package net.ssanj.dabble

import org.scalacheck.Properties
import org.scalacheck.{Prop, Gen}
import org.scalacheck.Prop.BooleanOperators
import scalaz.{\/-, Show}
import net.ssanj.dabble.Implicits._

object DabblePrinterProps             extends
       Properties("A Dependency Printer") with
       DabblePrinter                      with
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
        val output = printLibraryDependency(deps)

        val dependencyLines = deps map {
          case ScalaVersionSupplied(org, name, version, _) => s""""${org}" % "${name}" % "${version}""""
          case ScalaVersionDerived (org, name, version, _) => s""""${org}" %% "${name}" % "${version}""""
        }

        val dependencyString =
          s"libraryDependencies ++= Seq($newline" +
          dependencyLines.map(s"$tabAsSpaces" + _).mkString("," + newline) +
          s"${newline})"

        (dependencyString == output) :| labeled(output, dependencyString)
    }

 property("prints text representation of dependencies") =
  Prop.forAll(genDependencies) { deps: Seq[Dependency] =>
    val output = printLibraryDependenciesText(deps)

    val lines =
      deps.map {
        case ScalaVersionSupplied(org, name, version, _) => s"$org % $name % $version"
        case ScalaVersionDerived (org, name, version, _) => s"$org %% $name % $version"
      }

    val text = lines.zipWithIndex.map { case (l, i) => s"[${i+1}] $l" }.mkString(newline)
    (text == output) :| labeled(text, output)
  }

  property("prints valid resolvers") =
    Prop.forAll(genResolvers) { resolvers: Seq[Resolver] =>
      val output = printResolvers(resolvers)

      val resolverString =
        s"resolvers ++= Seq($newline" +
         resolvers.map(r => tabAsSpaces + Show[Resolver].shows(r)).mkString("," + newline) +
        s"${newline})"

     (resolverString == output) :| labeled(output, resolverString)
    }

 property("print text representation of resolvers") =
   Prop.forAllNoShrink(genResolverStringList) { resolverStrings: Seq[String] =>
    val \/-(resolvers) = ResolverParser.parseResolvers(resolverStrings)
    val output = printResolversAsText(resolvers)
    //remove all extraneous spaces in custom repos between name and url.
    val fixed = resolverStrings.map(r => if (r.contains("@")) r.split("@").map(_.trim).mkString("@") else r)
    val expected = fixed.zipWithIndex.map { case (r, i) => s"[${i+1}] $r" }.mkString(newline)

    (expected == output) :| labeled(output, expected)
   }

 property("print repl-escaped content") =
   Prop.forAllNoShrink(genReplCommand) {
    case (input: String, res: String) =>
      val output = replEscaped(input)
      (output == res) :| labeled(output.toArray.mkString(","), res.toArray.mkString(","))
   }
}