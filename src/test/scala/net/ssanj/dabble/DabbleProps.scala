package net.ssanj.dabble

import org.scalacheck.{Gen, Shrink}
import org.scalacheck.Gen.{posNum, negNum}
import scalaz._
import scalaz.std.list._

trait DabbleProps {

 private [dabble] def genShortStrings: Gen[String] = for {
    length <- Gen.choose(3, 10)
    value  <- Gen.listOfN(length, Gen.alphaLowerChar)
  } yield value.mkString

  private [dabble] def genOrg: Gen[String] = for {
    domain   <- Gen.listOfN(3, Gen.alphaLowerChar)
    orgName  <- genShortStrings
  } yield s"${domain.mkString}.${orgName}"

  private [dabble] def genVersion: Gen[String] = for {
     major <- Gen.posNum[Int]
     minor <- Gen.posNum[Int]
     patch <- Gen.posNum[Int]
  } yield s"${major}.${minor}.${patch}"

  private [dabble] def genScalaVersionSupplied: Gen[Seq[String]] = for {
     org          <- genOrg
     name         <- genShortStrings
     scalaVersion <- Gen.oneOf("2.10", "2.11")
     version      <- genVersion
  } yield  Seq(org, "%", s"${name}_${scalaVersion}", "%", version)

  private [dabble] def getConfigurationString: Gen[String] = Gen.oneOf("compile", "test", "default")

  private [dabble] def genScalaVersionSuppliedWithConfiguration: Gen[Seq[String]] = for {
     dep    <- genScalaVersionSupplied
     config <- getConfigurationString
  } yield  dep ++ Seq("%", config)

  private [dabble] def genScalaVersionDerived: Gen[Seq[String]] = for {
     org          <- genOrg
     name         <- genShortStrings
     version      <- genVersion
  } yield  Seq(org, "%%", s"${name}", "%", version)

  private [dabble] def genScalaVersionDerivedWithConfiguration: Gen[Seq[String]] = for {
     dep    <- genScalaVersionDerived
     config <- getConfigurationString
  } yield  dep ++ Seq("%", config)

  private [dabble] def genDependency: Gen[Seq[String]] =
    Gen.oneOf(genScalaVersionSupplied,
              genScalaVersionDerived,
              genScalaVersionSuppliedWithConfiguration,
              genScalaVersionDerivedWithConfiguration)

  private [dabble] def genDependencyList: Gen[Seq[String]] =
    (for {
      length <- Gen.choose(2, 10)
      deps   <- Gen.listOfN(length, genDependency)
    } yield deps).map(dl => intersperse(dl, Seq("+")).flatten)

  //Using Option.get is ugly. We expect these instances to be valid and fail otherwise.
  private [dabble] def genDependencies: Gen[Seq[Dependency]] =
    genDependencyList.map(d => DependencyParser.parse(d).toOption.get)

  private [dabble] def emptyInput: Gen[Seq[String]] = for {
    length <- Gen.choose(0, 10)
    deps   <- Gen.const(List.fill(length)(" "))
  } yield deps

  private[dabble] def labeled(actual: String, expected: String): String = s"expected:${newline}${expected}${newline}got:${newline}$actual"

  private[dabble] implicit val dependencyShrink: Shrink[Seq[Dependency]] = Shrink[Seq[Dependency]] {
    case Seq() => Stream.empty
    case Seq(one) => Stream.empty
    case Seq(one, two, _*) => Stream(Seq(one), Seq(two))
    case Seq(one, two, three, _*) => Stream(Seq(one), Seq(two), Seq(three), Seq(one, two), Seq(one, three), Seq(two, three))
  }
}

