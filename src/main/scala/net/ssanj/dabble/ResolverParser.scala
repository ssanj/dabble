package net.ssanj.dabble

import java.net.URL
import scala.util.Try
import scalaz._
import scalaz.syntax.either._


sealed trait RepoHost
case object Sonatype extends RepoHost
case object Typesafe extends RepoHost
case object TypesafeIvy extends RepoHost
case object SbtPlugin extends RepoHost

sealed trait ReleaseType
case object Snapshot extends ReleaseType
case object Release extends ReleaseType
case object Public extends ReleaseType

sealed trait DirectoryHost
case object Jcenter extends DirectoryHost
case object Maven2 extends DirectoryHost
//TODO: Add Maven1, Java1

sealed trait Resolver
case class Bintray(owner: String, repo: String) extends Resolver
case class ReleaseAndSnapshots(repoHost: RepoHost, releaseType: ReleaseType) extends Resolver
case class Directory(host: DirectoryHost) extends Resolver
case class Custom(name: String, url: URL) extends Resolver

final case class ResolverString(resolver: Resolver)

object ReleaseAndSnapshotString {
  def unapply(value: String): Option[(RepoHost, ReleaseType)] = value match {
    case "sonatype"      => Option((Sonatype,    Public))
    case "sonatype:r"    => Option((Sonatype,    Release))
    case "sonatype:s"    => Option((Sonatype,    Snapshot))
    case "typesafe"      => Option((Typesafe,    Public))
    case "typesafe:r"    => Option((Typesafe,    Release))
    case "typesafe:s"    => Option((Typesafe,    Snapshot))
    case "typesafeIvy"   => Option((TypesafeIvy, Public))
    case "typesafeIvy:r" => Option((TypesafeIvy, Release))
    case "typesafeIvy:s" => Option((TypesafeIvy, Snapshot))
    case "sbtPlugin"     => Option((SbtPlugin,   Public))
    case "sbtPlugin:r"   => Option((SbtPlugin,   Release))
    case "sbtPlugin:s"   => Option((SbtPlugin,   Snapshot))
    case _               => None
  }
}

object DirectoryStr {
  def unapply(value: String): Option[DirectoryHost] = value match {
    case "maven2"  => Option(Maven2)
    case "jcenter" => Option(Jcenter)
    case _         => None
  }
}

object BintrayStr {

  val bintrayReg = """^bintray:(.+):(.+)$""".r

  def unapply(value: String): Option[(String, String)] = value match {
    case bintrayReg(owner, repo) => Option((owner.trim, repo.trim))
    case _                       => None
  }
}

object CustomStr {

  // val customReg = """^\s*(\w+)\s*@(.+)$""".r
  val customReg = """^(.+)@(.+)$""".r

  def unapply(value: String): Option[(String, URL)] = value match {
    case customReg(name, url) => Try(new URL(url.trim)).toOption.map(u => (name.trim, u))
    case _                    => None
  }
}

trait ResolverParser {

  def parseResolvers(inputs: Seq[String]): String \/ Seq[Resolver] = {
    if (inputs.isEmpty) s"unable to derive resolvers from: $inputs".left
    else parse(inputs, Seq.empty[Resolver])
  }

  private def parse(inputs: Seq[String], resolvers: Seq[Resolver]): String \/ Seq[Resolver] = {
    inputs match {
      case Seq() => resolvers.right
      case Seq(ReleaseAndSnapshotString(host, rtype), t@_*) =>
        parse(t, resolvers :+ ReleaseAndSnapshots(host, rtype))
      case Seq(DirectoryStr(host), t@_*) =>
        parse(t, resolvers :+ Directory(host))
      case Seq(BintrayStr(owner, repo), t@_*) =>
        parse(t, resolvers :+ Bintray(owner, repo))
      case Seq(CustomStr(name, url), t@_*) =>
        parse(t, resolvers :+ Custom(name, url))
      case r => s"unknown repository type: ${r.mkString(",")}".left
    }
  }
}


object ResolverParser extends ResolverParser
