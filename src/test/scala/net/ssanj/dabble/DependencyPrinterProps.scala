package net.ssanj.dabble

import org.scalacheck.Properties
import org.scalacheck.{Prop, Gen}
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
    Prop.forAll(genDependencyList) { inputs: Seq[String] =>
        val \/-(deps) = DependencyParser.parse(inputs)
        val newline = System.getProperty("line.separator")
        val tab = "  "
        val output = print(deps)

        val dependencyLine = inputs.grouped(6).toList

        val dependencyString =
          s"libraryDependencies ++= Seq($newline$tab" +
          dependencyLine.map { xs =>
            xs.map {
              case x@"%" => x
              case x@"%%" => x
              case x@"+" => x
              case x => s""""${x}""""
            }.mkString(" ").
              replace(" +", s",$newline$tab")
          }.mkString +
          s"$newline)"
          dependencyString == output
    }
  }