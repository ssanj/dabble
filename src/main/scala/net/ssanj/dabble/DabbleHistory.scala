package net.ssanj.dabble


import scala.util.Try

import ammonite.ops._

import scalaz.NonEmptyList
import scalaz.NonEmptyList.nels
import scalaz.syntax.either._

final case class DabbleHistoryLine(dependencies: NonEmptyList[Dependency],
                                   resolvers: Seq[Resolver]  = Seq.empty,
                                   mpVersion: Option[String] = None)

trait DabbleHistory extends TerminalSupport  with
                            DependencyParser with
                            ResolverParser   with
                            DabblePaths {

  def readHistory(): Vector[DabbleHistoryLine] =
    Try(read.lines(dabbleHome.work.history, "UTF-8")).
      toOption.
      getOrElse(Vector.empty[String]).
      map(line => historyParser.parse(line.split(" "), DabbleRunConfig())).
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