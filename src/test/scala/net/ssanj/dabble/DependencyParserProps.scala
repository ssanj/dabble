package net.ssanj.dabble

import org.scalacheck.Properties
import org.scalacheck.{Prop, Gen}
import org.scalacheck.Gen.{posNum, negNum}
import scalaz._
import scalaz.std.list._

object DependencyParserProps extends Properties("DependencyParser") {

  private def genShortStrings: Gen[String] = for {
    length <- Gen.choose(3, 10)
    value  <- Gen.listOfN(length, Gen.alphaLowerChar)
  } yield value.mkString

  private def genOrg: Gen[String] = for {
    domain   <- Gen.listOfN(3, Gen.alphaLowerChar)
    orgName  <- genShortStrings
  } yield s"${domain.mkString}.${orgName}"

  private def genVersion: Gen[String] = for {
     major <- Gen.posNum[Int]
     minor <- Gen.posNum[Int]
     patch <- Gen.posNum[Int]
  } yield s"${major}.${minor}.${patch}"

  private def genScalaVersionSupplied: Gen[Seq[String]] = for {
     org          <- genOrg
     name         <- genShortStrings
     scalaVersion <- Gen.oneOf("2.10", "2.11")
     version      <- genVersion
  } yield  Seq(org, "%", s"${name}_${scalaVersion}", "%", version)

  private def genScalaVersionDerived: Gen[Seq[String]] = for {
     org          <- genOrg
     name         <- genShortStrings
     version      <- genVersion
  } yield  Seq(org, "%%", s"${name}", "%", version)

  private def genDependency: Gen[Seq[String]] = for {
    dep <- Gen.oneOf(genScalaVersionSupplied, genScalaVersionDerived)
  } yield dep


  private def genDependencyList: Gen[Seq[String]] =
    (for {
      length <- Gen.choose(2, 10)
      deps   <- Gen.listOfN(length, Gen.oneOf(genScalaVersionDerived, genScalaVersionSupplied))
    } yield deps).map(dl => intersperse(dl, Seq("+")).flatten)


  private def emptyInput: Gen[Seq[String]] = for {
    length <- Gen.choose(0, 10)
    deps   <- Gen.const(List.fill(length)(" "))
  } yield deps

 property("returns a valid dependency from a valid input") =
  Prop.forAll(genDependency) { inputs: Seq[String] =>
      val \/-(Seq(dep)) = DependencyParser.parse(inputs)
      dep match {
        case ScalaVersionSupplied(org, name, version) => Seq(org, "%" , name, "%", version) == inputs
        case ScalaVersionDerived (org, name, version) => Seq(org, "%%", name, "%", version) == inputs
      }
  }

property("returns a valid list of dependencies from a valid list of inputs")=
  Prop.forAll(genDependencyList) { inputs: Seq[String] =>
    val \/-(deps) = DependencyParser.parse(inputs)
    val outputs = intersperse(deps.map {
      case ScalaVersionSupplied(org, name, version) => Seq(org, "%" , name, "%", version)
      case ScalaVersionDerived (org, name, version) => Seq(org, "%%", name, "%", version)
    }.toList, Seq("+")).flatten

    inputs == outputs
  }

property("returns an empty list of dependencies if the input is invalid") =
  Prop.forAll(emptyInput) { inputs: Seq[String] =>
    val -\/(error) = DependencyParser.parse(inputs)
    error == s"unable to derive dependencies from: $inputs"
  }
}