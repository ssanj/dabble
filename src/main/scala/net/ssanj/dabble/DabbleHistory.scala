package net.ssanj.dabble


import scala.util.Try

import ammonite.ops._

import scalaz._
import scalaz.NonEmptyList.nels
import scalaz.syntax.either._
import scalaz.std.list._
import scalaz.syntax.traverse._
import DependencyParser._
import ResolverParser._

final case class DabbleHistoryLine(dependencies: NonEmptyList[Dependency],
                                   resolvers: Seq[Resolver]  = Seq.empty,
                                   mpVersion: Option[String] = None)

trait DabbleHistory {

  type HistoryLinesOr = Seq[ValidationNel[String, DabbleHistoryLine]]

  type CommandlineParser = Array[String] => Option[DabbleRunConfig]

  type HistoryLinesAndWarnings = Seq[String] \&/ Seq[DabbleHistoryLine]

  def readHistory(cmdlnParser: CommandlineParser)(lines: Seq[String]): HistoryLinesOr = {
    val result =
      lines.
        map(line => cmdlnParser(line.split(" "))).
        flatten.
        map { c =>
          (for {
            deps <- parseDependencies(c.dependencies)
            res  <- (if (c.resolvers.nonEmpty) parseResolvers(c.resolvers) else Seq.empty.right[String])
          } yield (DabbleHistoryLine(nels(deps.head, deps.tail:_*), res, c.macroParadiseVersion)))
        }.
        map(_.validationNel[String])

    result
  }

  def readHistoryWithWarnings(cmdlnParser: CommandlineParser)(lines: Seq[String]): HistoryLinesAndWarnings = {
      import \&/._
      val validationNels = readHistory(cmdlnParser)(lines)
      val successes = validationNels.collect { case Success(dhl) => dhl }
      val warnings  = validationNels.collect { case Failure(warningsNel) => warningsNel.list.toList } flatten

      if (successes.isEmpty) This(warnings)
      else if (warnings.isEmpty) That(successes)
      else Both(warnings, successes)
    }
}


object DabbleHistory extends DabbleHistory