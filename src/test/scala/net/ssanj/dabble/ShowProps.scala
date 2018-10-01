package net.ssanj.dabble

import org.scalacheck.Properties
import org.scalacheck.Prop
import org.scalacheck.Prop.BooleanOperators
import scalaz._
import net.ssanj.dabble.Implicits._

object ShowProps extends Properties("Show instance for Dabble") with DabbleProps {

  property("shows Dependency") =
    Prop.forAll(genDependency) { inputs: Seq[String] =>
        val \/-(Seq(dep))     = DependencyParser.parseDependencies(inputs)
        //we take(5) here because Dependencies don't store configuration (which would be upto 7 Strings)
        val output: String    = inputs.take(5).map(i => if (!i.startsWith("%")) s""""$i"""" else i).mkString(" ")
        val genOutput: String = Show[Dependency].shows(dep)

        genOutput == output
    }

 property("shows Resolver") =
  Prop.forAllNoShrink(genResolverString) { input: String =>
    val output =
      input match {
        case "sonatype:r"                     => """Resolver.sonatypeRepo("releases")"""
        case "sonatype:s"                     => """Resolver.sonatypeRepo("snapshots")"""
        case "sonatype"                       => """Resolver.sonatypeRepo("public")"""
        case "typesafe:r"                     => """Resolver.typesafeRepo("releases")"""
        case "typesafe:s"                     => """Resolver.typesafeRepo("snapshots")"""
        case "typesafe"                       => """Resolver.typesafeRepo("releases"), Resolver.typesafeRepo("snapshots")"""
        case "typesafeIvy:r"                  => """Resolver.typesafeIvyRepo("releases")"""
        case "typesafeIvy:s"                  => """Resolver.typesafeIvyRepo("snapshots")"""
        case "typesafeIvy"                    => """Resolver.typesafeIvyRepo("releases"), Resolver.typesafeIvyRepo("snapshots")"""
        case "sbtPlugin:r"                    => """Resolver.sbtPluginRepo("releases")"""
        case "sbtPlugin:s"                    => """Resolver.sbtPluginRepo("snapshots")"""
        case "sbtPlugin"                      => """Resolver.sbtPluginRepo("releases"), Resolver.sbtPluginRepo("snapshots")"""
        case "maven2"                         => """Resolver.DefaultMavenRepository"""
        case "jcenter"                        => """Resolver.jcenterRepo"""
        case r if ((r.startsWith("bintray")) && (r.split(":").length == 3)) =>
          val Array(_, owner, repo) = r.split(":")
          s"""Resolver.bintrayRepo("$owner", "$repo")"""
        case r if (r.split("@").length == 2) =>
          val Array(name, url) = r.split("@")
          s""""${name.trim}" at "${url.trim}""""
        case r => s"unknown repository pattern: $r"
      }

    val \/-(Seq(res)) = ResolverParser.parseResolvers(Seq(input))
    val showOutput    = Show[Resolver].shows(res)

    (showOutput == output) :| labeled(showOutput, output)

  }

}