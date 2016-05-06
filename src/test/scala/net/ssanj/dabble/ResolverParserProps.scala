package net.ssanj.dabble

import org.scalacheck.Properties
import org.scalacheck.{Prop, Gen}
import org.scalacheck.Prop.BooleanOperators

import scalaz._

import ResolverParser._

object ResolverParserProps extends Properties("ResolverParser") with DabbleProps {

  property("returns a single valid resolver") =
    Prop.forAllNoShrink(genResolverString) { input: String =>
        val \/-(Seq(resolver)) = ResolverParser.parseResolvers(Seq(input))
        val result =
          resolver match {
            case ReleaseAndSnapshots(Sonatype    , Public)   => s"sonatype"
            case ReleaseAndSnapshots(Sonatype    , Release)  => s"sonatype:r"
            case ReleaseAndSnapshots(Sonatype    , Snapshot) => s"sonatype:s"
            case ReleaseAndSnapshots(Typesafe    , Public)   => s"typesafe"
            case ReleaseAndSnapshots(Typesafe    , Release)  => s"typesafe:r"
            case ReleaseAndSnapshots(Typesafe    , Snapshot) => s"typesafe:s"
            case ReleaseAndSnapshots(TypesafeIvy , Public)   => s"typesafeIvy"
            case ReleaseAndSnapshots(TypesafeIvy , Release)  => s"typesafeIvy:r"
            case ReleaseAndSnapshots(TypesafeIvy , Snapshot) => s"typesafeIvy:s"
            case ReleaseAndSnapshots(SbtPlugin   , Public)   => s"sbtPlugin"
            case ReleaseAndSnapshots(SbtPlugin   , Release)  => s"sbtPlugin:r"
            case ReleaseAndSnapshots(SbtPlugin   , Snapshot) => s"sbtPlugin:s"
            case Directory(Maven2)                           => "maven2"
            case Directory(Jcenter)                          => "jcenter"
            case Bintray(owner, repo)                        => s"bintray:$owner:$repo"
            case Custom(name, url)                           => s"$name@${url.toExternalForm}"
          }

        //We trim spaces, so we can't have an exact match
        if (input.contains("@")) {
          val Array(name, url) = input.split("@")
          val inputWithoutSpaces = s"${name.trim}@${url.trim}"
          (inputWithoutSpaces == result) :| labeled(result, inputWithoutSpaces)
        } else {
          (input == result) :| labeled(result, input)
        }
    }
  }