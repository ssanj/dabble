package net.ssanj.dabble

import org.scalacheck.Properties
import org.scalacheck.{Prop, Gen}
import org.scalacheck.Prop.BooleanOperators
import scalaz.{\/-, NonEmptyList, Show}
import scalaz.NonEmptyList.nels
import net.ssanj.dabble.Implicits._
import ScalaCheckSupport.contentProp

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

        (output == dependencyString) :| labeled(output, dependencyString)
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
    (output == text) :| labeled(output, text)
  }

  property("prints valid resolvers") =
    Prop.forAll(genResolvers) { resolvers: Seq[Resolver] =>
      val output = printResolvers(resolvers)

      val resolverString =
        s"resolvers ++= Seq($newline" +
         resolvers.map(r => tabAsSpaces + Show[Resolver].shows(r)).mkString("," + newline) +
        s"${newline})"

     (output == resolverString) :| labeled(output, resolverString)
    }

 property("print text representation of resolvers") =
   Prop.forAllNoShrink(genResolverStringList) { resolverStrings: Seq[String] =>
    val \/-(resolvers) = ResolverParser.parseResolvers(resolverStrings)
    val output = printResolversAsText(resolvers)
    //remove all extraneous spaces in custom repos between name and url.
    val fixed    = resolverStrings.map(r => if (r.contains("@")) r.split("@").map(_.trim).mkString("@") else r)
    val expected = fixed.zipWithIndex.map { case (r, i) => s"[${i+1}] $r" }.mkString(newline)

    (output == expected) :| labeled(output, expected)
   }

 property("print repl-escaped content") =
   Prop.forAllNoShrink(genReplCommand) {
    case (input: String, res: String) =>
      val output = replEscaped(input)
      (output == res) :| labeled(output.toArray.mkString(","), res.toArray.mkString(","))
   }

 property("print macro paradise dependency") =
   Prop.forAllNoShrink(genLibraryVersion) { version =>
    val output   = printMacroParadise(version)
    val expected = s"""addCompilerPlugin("org.scalamacros" % "paradise" % """" +
                       version +
                       """" cross CrossVersion.full)"""

    (output == expected) :| labeled(output, expected)
   }

  property("print initial sbt commands") = {
    Prop.forAllNoShrink(genDependencies, genResolversWithEmpty, genMacroParadise) {  (deps, resolvers, mp) =>
      val initialCommands = printInitialSbtCommands(deps, resolvers, mp)

      val dependencyText = printLibraryDependenciesText(deps)
      val dependencyInfo = s"${newline}Dabble injected the following libraries:" +
                           s"${newline}${dependencyText}${newline}"
      val resolverInfo =
        if (resolvers.nonEmpty) {
          val resolverText = printResolversAsText(resolvers)
          s"${newline}Dabble injected the following resolvers:" +
          s"${newline}${resolverText}${newline}"
        } else ""


      val mpInfo = mp.fold(""){ mpv =>
        s"${newline}Dabble injected macro paradise version: " +
        s"${mpv}${newline}"
      }

      val commands = s"""println("${dependencyInfo + resolverInfo + mpInfo}")"""
      val expectedCommands = s"""initialCommands := "${replEscaped(commands)}""""

      contentProp("initialCommands")(Seq(initialCommands), Seq(expectedCommands))
    }
  }


 property("print history line") =
   Prop.forAll(genDabbleHistoryLine) {
      case dhl@DabbleHistoryLine(deps: NonEmptyList[Dependency], res: Seq[Resolver], mpv: Option[String]) =>
        val output = printHistoryLine(dhl)
        val expectedDeps = deps.map(d =>
                                        Show[DependencyHistoryString].
                                          shows(DependencyHistoryString(d))).
                                list.
                                toList.
                                mkString(" + ")

        val expectedResolvers =
          if (res.isEmpty) ""
          else {
            val r =  res.map(r => Show[ResolverString].shows(ResolverString(r))).mkString(",")
            s""" -r ${r}"""
          }

        val expectedMacroParadiseVesion =
          mpv.map(v => s""" -mp ${v}""").getOrElse("")

        val expected = s"${expectedDeps}${expectedResolvers}${expectedMacroParadiseVesion}"

        (output == expected) :| labeled(output.toArray.mkString(","), expected.toArray.mkString(","))
   }

property("print history lines") =
  Prop.forAll(between(5, 10)(genDabbleHistoryLine)) { lines: Seq[DabbleHistoryLine] =>
    val output   = printHistoryLines(lines)
    val expected = lines.map(printHistoryLine).mkString(newline)

    (output == expected) :| labeled(output.toArray.mkString(","), expected.toArray.mkString(","))
  }
}