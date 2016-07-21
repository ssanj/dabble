package net.ssanj.dabble

import org.scalacheck.{Gen, Shrink}
import org.scalacheck.Gen.{posNum, negNum}
import scalaz._
import scalaz.NonEmptyList
import scalaz.NonEmptyList.nels
import scalaz.std.list._

import DabblePathTypes.{DirPath, FilePath, DabbleHomePath}

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

  private [dabble] def genDependencies: Gen[Seq[Dependency]] =
    genDependencyList.map(d => DependencyParser.parseDependencies(d).fold(failGen[Seq[Dependency]], identity))

  private [dabble] def emptyInput: Gen[Seq[String]] = for {
    length <- Gen.choose(0, 10)
    deps   <- Gen.const(List.fill(length)(" "))
  } yield deps

  private[dabble] def labeled(actual: String, expected: String): String = s"expected:${newline}${tab}${expected}${newline}got:${newline}${tab}$actual"

  private[dabble] def labeled(property: String)(actual: String, expected: String): String = s"$property: expected:${newline}${tab}[${expected}]${newline}got:${newline}${tab}[$actual]"

  private[dabble] implicit val dependencyShrink: Shrink[Seq[Dependency]] = Shrink[Seq[Dependency]] {
    case Seq() => Stream.empty
    case Seq(one) => Stream.empty
    case Seq(one, two, _*) => Stream(Seq(one), Seq(two))
    case Seq(one, two, three, _*) => Stream(Seq(one), Seq(two), Seq(three), Seq(one, two), Seq(one, three), Seq(two, three))
  }

  private[dabble] def genReleaseAndSnapshotsResolverString: Gen[String] = for {
    repo  <- Gen.oneOf("sonatype", "typesafe", "typesafeIvy", "sbtPlugin")
    rtype <- Gen.oneOf(":s", ":r", "")
  } yield (s"${repo}${rtype}")

  private[dabble] def genDirectoryResolverString: Gen[String] = Gen.oneOf("maven2", "jcenter")

  private[dabble] def genDomainString: Gen[String] = for {
    tld        <- Gen.oneOf(".com", ".net", ".com.au", ".net.au")
    domain     <- genShortStrings
    cname      <- Gen.oneOf("", "www.")
    } yield s"${cname}${domain}${tld}"

  private[dabble] def genBintrayResolverString: Gen[String] = for {
    owner <- genShortStrings
    repo  <- genShortStrings
  } yield s"bintray:$owner:$repo"

  private[dabble] def genSpaces(n: Int): Gen[String] = for {
    no <- Gen.choose(1, n)
    spaces <- Gen.listOfN(no, Gen.const(" "))
  } yield spaces.mkString

  private[dabble] def emptyStringOrSpaces(n: Int): Gen[String] = for {
    spaces <- Gen.oneOf(emptyString, genSpaces(n))
  } yield spaces

  private[dabble] def emptyString: Gen[String] = Gen.const("")

  private[dabble] def genSpaceSeparatedName: Gen[String] = for {
    length <- Gen.choose(1, 4)
    names <- Gen.listOfN(length, genShortStrings)
  } yield names.mkString(" ")

  private[dabble] def genCustomResolverString: Gen[String] = for {
    name       <- genSpaceSeparatedName
    nbSpaces   <- emptyStringOrSpaces(3) //space before the name
    naSpaces   <- emptyStringOrSpaces(3) //space after the name
    protocol   <- Gen.oneOf("http", "https")
    ubSpaces   <- emptyStringOrSpaces(3) //space before url
    uaSpaces   <- emptyStringOrSpaces(3) //space after url
    pathLength <- Gen.choose(3, 10)
    path       <- Gen.listOfN(pathLength, genShortStrings)
    domain     <- genDomainString
  } yield s"${nbSpaces}${name}${naSpaces}@${ubSpaces}$protocol://${domain}/${path.mkString("/")}${uaSpaces}"

  private[dabble] def genResolverString: Gen[String] =
    Gen.oneOf(genReleaseAndSnapshotsResolverString,
              genDirectoryResolverString,
              genBintrayResolverString,
              genCustomResolverString)

  private[dabble] def genResolverStrings: Gen[String] =
    between(1, 10)(genResolverString).map(_.mkString(","))

  private [dabble] def genResolverStringList: Gen[Seq[String]] =
    (for {
      length   <- Gen.choose(1, 3)
      resolver <- Gen.listOfN(length, genResolverString)
    } yield resolver)

  private [dabble] def genResolver: Gen[Resolver] =
    genResolverString.map(r => ResolverParser.parseResolvers(Seq(r)).
      fold(failGen,
           rr => if (rr.nonEmpty) rr.head
                 else failGen[Resolver](s"parsing $r resulted in an empty resolver list")))

  private[dabble] def genResolvers: Gen[Seq[Resolver]] =
    genResolverStringList.map(r => ResolverParser.parseResolvers(r).fold(failGen[Seq[Resolver]], identity))

  private[dabble] def genResolversWithEmpty: Gen[Seq[Resolver]] =
    Gen.oneOf(genResolvers, Gen.const(Seq.empty[Resolver]))

  private[dabble] def genMacroParadise: Gen[Option[String]] =
    Gen.oneOf(genLibraryVersion.map(Option(_)), Gen.const(None: Option[String]))

  private[dabble] def genDabbleHistoryLine: Gen[DabbleHistoryLine] = for {
    deps <- genDependencies.map(d => nels(d.head, d.tail:_*))
    res  <- genResolversWithEmpty
    mpv  <- genMacroParadise
  } yield DabbleHistoryLine(deps, res, mpv)

  //History file lines should not have any extraneous spaces within resolvers
  private[dabble] def genDabbleHistoryFileLine: Gen[DabbleHistoryLine] = for {
    deps <- genDependencies.map(d => nels(d.head, d.tail:_*))
    res  <- genResolversWithEmpty.map(_.map {
              case Custom(name, url) => Custom(name.replace(" ", ""), url)
              case other => other
            })
    mpv  <- genMacroParadise
  } yield DabbleHistoryLine(deps, res, mpv)

  private[dabble] def genInvalidDabbleHistoryLineInputs: Gen[(Seq[String], Seq[String], Seq[String], Seq[String])] = {
    import Implicits._
    for {
      validDeps   <- genDependencies.map(DependencyParser.toInputStrings)
      invalidDeps <- genDependencies.map(deps => DependencyParser.toInputStrings(deps).map(
                      _.replace("%", "#")))
      validRes    <- genResolvers.map(_.map(r => implicitly[Show[ResolverString]].shows(ResolverString(r))))
      invalidRes  <- genResolvers.map(_.map(r => implicitly[Show[ResolverString]].shows(ResolverString(r)).
                      drop(2).replace("@", "$$")))
    } yield (validDeps, validRes, invalidDeps, invalidRes)
  }

  private[dabble] def genSimpleDabbleHistoryLine: Gen[DabbleHistoryLine] =
    genDabbleHistoryLine.
      map(dhl => dhl.copy(
                  dependencies = nels(dhl.dependencies.head),
                  resolvers = Seq.empty,
                  mpVersion = None))

  private[dabble] def between[T](min: Int, max: Int)(gen: Gen[T]): Gen[Seq[T]] = for {
    l <- Gen.choose(Math.max(0, min), Math.min(Int.MaxValue, max))
    v <- Gen.listOfN(l, gen)
  } yield v

  private[dabble] def many[T](count: Int)(gen: Gen[T]): Gen[Seq[T]] = Gen.listOfN(count, gen)

  private[dabble] def manyPairs[A, B](count: Int)(genA: Gen[A], genB: Gen[B]): Gen[Seq[(A, B)]] = {
    Gen.listOfN(count, genA.flatMap(a => genB.map(b => (a, b))))
  }

  private[dabble] implicit val resolverShrink: Shrink[Seq[Resolver]] = Shrink[Seq[Resolver]] {
    case Seq() => Stream.empty
    case Seq(one) => Stream.empty
    case Seq(one, two, _*) => Stream(Seq(one), Seq(two))
    case Seq(one, two, three, _*) => Stream(Seq(one), Seq(two), Seq(three), Seq(one, two), Seq(one, three), Seq(two, three))
  }

  //TODO: Can we replace this with suchThat ??
  private def failGen[A](error: String): A = throw new IllegalArgumentException(error)

  private[dabble] def genReplCommand: Gen[(String, String)] =
    Gen.oneOf(
              ("""println("test")""", s"""println(\\\"test\\\")"""),
              (s"""println("${newline}1${newline}2{$tab}3")""", s"""println(\\\"${escapedNewline}1${escapedNewline}2{$escapedTab}3\\\")""")
             )

 private[dabble] def genLibraryVersion: Gen[String] = for {
    major <- Gen.choose(1, 10)
    minor <- Gen.choose(1, 10)
    patch <- Gen.choose(1, 30)
 } yield s"${major}.${minor}.${patch}"

  private[dabble] def invalidDependenciesGen: Gen[Seq[String]] = for {
    l <- Gen.choose(1, 5)
    deps <- many(l)(genDependency.map(_.drop(1).mkString(" ")))
  } yield deps

  private[dabble] case class SbtTemplateContent(content: String)

  private lazy val projSbtTemplate = """name := "Dabble"

organization := "net.ssanj"

version := "0.2.0"

scalaVersion := "2.11.8"

libraryDependencies ++= Seq(
  "org.scalaz"       %% "scalaz-core"  % "7.2.2",
  "com.lihaoyi"      %% "ammonite-ops" % "0.5.7",
  "com.github.scopt" %% "scopt"        % "3.4.0",
  "org.scalatest"    %% "scalatest"    % "2.2.4"  % "test",
  "org.scalacheck"   %% "scalacheck"   % "1.12.5" % "test"
)

scalacOptions ++= Seq(
                      "-unchecked",
                      "-deprecation",
                      "-feature",
                      "-Xfatal-warnings",
                      "-Xlint:_",
                      "-Ywarn-dead-code",
                      "-Ywarn-inaccessible",
                      // "-Ywarn-unused-import",
                      "-Ywarn-infer-any",
                      "-Ywarn-nullary-override",
                      "-Ywarn-nullary-unit",
                      "-Ypatmat-exhaust-depth",
                      "40"
                     )
"""

  import DefaultTemplate._
  private[dabble] def genSbtTemplateString: Gen[String] = for {
    header       <- Gen.alphaStr
    name         <- Gen.alphaStr
    org          <- genOrg
    version      <- genVersion
    scalaVersion <- genVersion
  } yield {
    s"//generated by $header"       + newline +
    s"""name := "${name}""""        + newline +
    s"""organization := "${org}"""" + newline +
    s"""version := "${version}""""  + newline +
    s"""scalaVersion := "${scalaVersion}""""
  }

  private[dabble] def genSbtTemplateContent: Gen[SbtTemplateContent] =
    Gen.oneOf(Gen.const(projSbtTemplate), genSbtTemplateString).map(SbtTemplateContent)

  private[dabble] def genDirPath: Gen[DirPath] = for {
    pathLength <- Gen.choose(3, 10)
    path       <- Gen.listOfN(pathLength, genShortStrings)
  } yield DirPath(path.mkString("/"))

  private[dabble] def genFilePath: Gen[FilePath] = for {
    pathLength <- Gen.choose(3, 10)
    path       <- Gen.listOfN(pathLength, genShortStrings)
    filename   <- genShortStrings
  } yield FilePath(DirPath(path.mkString("/")), filename)

  private[dabble] def genDabbleHomePath: Gen[DabbleHomePath] = genDirPath.map(DabbleHomePath)

  private[dabble] case class Word(word: String)

  private[dabble] def genWord: Gen[Word] = for {
    l       <- Gen.choose(5, 10)
    letters <- Gen.listOfN(l, Gen.alphaLowerChar)
  } yield Word(letters.mkString)

  private[dabble] def genWords: Gen[Seq[Word]] = for {
    l     <- Gen.choose(5, 10)
    words <- Gen.listOfN(l, genWord)
  } yield words

  private[dabble] def md5(values: Any*): String = {
    import java.security.MessageDigest
    MessageDigest.getInstance("MD5").
      digest(values.map(_.toString).mkString.getBytes).map("%02x".format(_)).mkString
  }
}

object DabbleProps extends DabbleProps

