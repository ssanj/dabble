package net.ssanj

import scalaz.Show
import scalaz.Show.shows
import scalaz.ValidationNel

package object dabble {

  object Implicits {
    implicit val dependencyShows: Show[Dependency] = shows {
      case ScalaVersionSupplied(org, name, version, _) => s""""${org}" % "${name}" % "$version""""
      case ScalaVersionDerived (org, name, version, _) => s""""${org}" %% "${name}" % "$version""""
    }

    implicit val dependencyStringShows: Show[DependencyHistoryString] = shows {
      case DependencyHistoryString(ScalaVersionSupplied(org, name, version, _)) => s"""${org} % ${name} % $version"""
      case DependencyHistoryString(ScalaVersionDerived (org, name, version, _)) => s"""${org} %% ${name} % $version"""
    }

    implicit val resolverShows: Show[Resolver] = shows {
      case ReleaseAndSnapshots(Sonatype, Release)     => """Resolver.sonatypeRepo("releases")"""
      case ReleaseAndSnapshots(Sonatype, Snapshot)    => """Resolver.sonatypeRepo("snapshots")"""
      case ReleaseAndSnapshots(Sonatype, Public)      => """Resolver.sonatypeRepo("public")"""
      case ReleaseAndSnapshots(Typesafe, Release)     => """Resolver.typesafeRepo("releases")"""
      case ReleaseAndSnapshots(Typesafe, Snapshot)    => """Resolver.typesafeRepo("snapshots")"""
      case ReleaseAndSnapshots(Typesafe, Public)      => """Resolver.typesafeRepo("releases"), Resolver.typesafeRepo("snapshots")"""
      case ReleaseAndSnapshots(TypesafeIvy, Release)  => """Resolver.typesafeIvyRepo("releases")"""
      case ReleaseAndSnapshots(TypesafeIvy, Snapshot) => """Resolver.typesafeIvyRepo("snapshots")"""
      case ReleaseAndSnapshots(TypesafeIvy, Public)   => """Resolver.typesafeIvyRepo("releases"), Resolver.typesafeIvyRepo("snapshots")"""
      case ReleaseAndSnapshots(SbtPlugin, Release)    => """Resolver.sbtPluginRepo("releases")"""
      case ReleaseAndSnapshots(SbtPlugin, Snapshot)   => """Resolver.sbtPluginRepo("snapshots")"""
      case ReleaseAndSnapshots(SbtPlugin, Public)     => """Resolver.sbtPluginRepo("releases"), Resolver.sbtPluginRepo("snapshots")"""
      case Directory(Maven2)                          => """Resolver.DefaultMavenRepository"""
      case Directory(Jcenter)                         => """Resolver.jcenterRepo"""
      case Bintray(owner, repo)                       => s"""Resolver.bintrayRepo("$owner", "$repo")"""
      case Custom(name, url)                          => s""""$name" at "${url.toExternalForm}""""
    }

    implicit val resolverStringShows: Show[ResolverString] = shows {
      case ResolverString(ReleaseAndSnapshots(Sonatype, Release))     => """sonatype:r"""
      case ResolverString(ReleaseAndSnapshots(Sonatype, Snapshot))    => """sonatype:s"""
      case ResolverString(ReleaseAndSnapshots(Sonatype, Public))      => """sonatype"""
      case ResolverString(ReleaseAndSnapshots(Typesafe, Release))     => """typesafe:r"""
      case ResolverString(ReleaseAndSnapshots(Typesafe, Snapshot))    => """typesafe:s"""
      case ResolverString(ReleaseAndSnapshots(Typesafe, Public))      => """typesafe"""
      case ResolverString(ReleaseAndSnapshots(TypesafeIvy, Release))  => """typesafeIvy:r"""
      case ResolverString(ReleaseAndSnapshots(TypesafeIvy, Snapshot)) => """typesafeIvy:s"""
      case ResolverString(ReleaseAndSnapshots(TypesafeIvy, Public))   => """typesafeIvy"""
      case ResolverString(ReleaseAndSnapshots(SbtPlugin, Release))    => """sbtPlugin:r"""
      case ResolverString(ReleaseAndSnapshots(SbtPlugin, Snapshot))   => """sbtPlugin:s"""
      case ResolverString(ReleaseAndSnapshots(SbtPlugin, Public))     => """sbtPlugin"""
      case ResolverString(Directory(Maven2))                          => """maven2"""
      case ResolverString(Directory(Jcenter))                         => """jcenter"""
      case ResolverString(Bintray(owner, repo))                       => s"""bintray:${owner}:${repo}"""
      case ResolverString(Custom(name, url))                          => s"""$name@${url.toExternalForm}"""
    }
  }

//https://github.com/sbt/librarymanagement/blob/1.0/librarymanagement/src/main/scala/sbt/librarymanagement/Resolver.scala
// JavaNet1Repository This is the Maven 1 repository at http://download.java.net/maven/1/

  val newline                  = System.getProperty("line.separator")
  def newlines(n: Int): String = List.fill(n)(newline).mkString
  val userHome                 = System.getProperty("user.home")
  val tab                      = "\t"
  val escapedTab               = "\\\\t"
  val tabAsSpaces              = "  "
  val newlineAndTab            = s"${newline}${tab}"
  val escapedNewline           = newline.replace("\n", "\\\\n").replace("\r", "\\\\r")
  val defaultBuildFile         = "build.sbt"
  lazy val title               = s"${DabbleInfo.name} version: ${DabbleInfo.version}-b${DabbleInfo.buildInfoBuildNumber}"

  def log(messages: String*): Unit = println(s"${DabbleInfo.name}: ${messages.mkString(newline)}")
}
