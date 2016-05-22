package net.ssanj.dabble


import scala.util.Try

import ammonite.ops._

import scalaz.NonEmptyList
import scalaz.NonEmptyList.nels
import scalaz.syntax.either._

final case class DabbleHistoryLine(dependencies: NonEmptyList[Dependency],
                                   resolvers: Seq[Resolver]  = Seq.empty,
                                   mpVersion: Option[String] = None)

trait DabbleHistory { self: DependencyParser with
                            ResolverParser =>

  //essentially we don't need to extend all the above traits. We need:
  //parser = Array[String] => Option[DabbleRunConfig]
  //depParser = Seq[String] => String \/ Seq[Dependency]
  //resolverParser = Seq[String] => String \/ Seq[Resolver]
  //can we use typeclasses here?
  def readHistory(f: Array[String] => Option[DabbleRunConfig])(lines: Seq[String]): Seq[DabbleHistoryLine] = {
    lines.
      map(line => f(line.split(" "))).
      flatten.
      map { c =>
        (for {
          deps <- parseDependencies(c.dependencies)
          res  <- (if (c.resolvers.nonEmpty) parseResolvers(c.resolvers) else Seq.empty.right[String])
        } yield (DabbleHistoryLine(nels(deps.head, deps.tail:_*), res, c.macroParadiseVersion))).
          fold(l => None, r => Option(r))
      }.
      flatten
  }
}