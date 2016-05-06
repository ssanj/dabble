package net.ssanj.dabble

import org.scalacheck.Properties
import org.scalacheck.{Prop, Gen}
import org.scalacheck.Gen.{posNum, negNum}
import scalaz._
import scalaz.std.list._

object DependencyParserProps extends Properties("DependencyParser") with DabbleProps {

 property("returns a valid dependency from a valid input") =
  Prop.forAll(genDependency) { inputs: Seq[String] =>
      val \/-(Seq(dep)) = DependencyParser.parseDependencies(inputs)
      dep match {
        case ScalaVersionSupplied(org, name, version, None) =>
          Seq(org, "%" , name, "%", version) == inputs
        case ScalaVersionSupplied(org, name, version, Some(config)) =>
          Seq(org, "%" , name, "%", version, "%", config) == inputs
        case ScalaVersionDerived (org, name, version, None) =>
          Seq(org, "%%", name, "%", version) == inputs
        case ScalaVersionDerived (org, name, version, Some(config)) =>
          Seq(org, "%%", name, "%", version, "%", config) == inputs
      }
  }

property("returns a valid list of dependencies from a valid list of inputs")=
  Prop.forAll(genDependencyList) { inputs: Seq[String] =>
    val \/-(deps) = DependencyParser.parseDependencies(inputs)
    val outputs = intersperse(deps.map {
      case ScalaVersionSupplied(org, name, version, None) =>
        Seq(org, "%" , name, "%", version)
      case ScalaVersionSupplied(org, name, version, Some(config)) =>
        Seq(org, "%" , name, "%", version, "%", config)
      case ScalaVersionDerived (org, name, version, None) =>
        Seq(org, "%%", name, "%", version)
      case ScalaVersionDerived (org, name, version, Some(config)) =>
        Seq(org, "%%", name, "%", version, "%", config)
    }.toList, Seq("+")).flatten

    inputs == outputs
  }

property("returns an empty list of dependencies if the input is invalid") =
  Prop.forAll(emptyInput) { inputs: Seq[String] =>
    val -\/(error) = DependencyParser.parseDependencies(inputs)
    error == s"unable to derive dependencies from: $inputs"
  }
}