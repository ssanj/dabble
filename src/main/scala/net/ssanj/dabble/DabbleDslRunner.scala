package net.ssanj.dabble

import scala.util.Try

import ammonite.ops._

import scalaz.{-\/, \/-}
import scalaz.syntax.either._
import scalaz.NonEmptyList.nels
import scalaz.syntax.std.`try`._

import DabbleDslDef._
import DabblePaths._
import DabbleHistory._
import DependencyParser._
import ResolverParser._
import Banner._
import TerminalSupport._
import dsl.DependencyCommands._
import dsl.HistoryCommands._

object DabbleDslRunner extends App {

  private def loadDependencies(drc: DabbleRunConfig, dabbleHome: DabbleHome): Unit = {
      val deps = drc.dependencies
      val res  = drc.resolvers
      val mp   = drc.macroParadiseVersion

      val lineE = for {
        dependencies <- parseDependencies(deps)
        resolvers    <- if (res.nonEmpty) parseResolvers(res) else Seq.empty.right[String]
      } yield DabbleHistoryLine(nels(dependencies.head, dependencies.tail:_*), resolvers, mp)

      lineE match {
        case -\/(error) =>
          println(s"Could not parse inputs: $error.")

        case \/-(line)  =>
          val argParser: CommandlineParser = historyParser.parse(_, DabbleRunConfig())
          val historyPrinter = DabblePrinter.printHistoryLine(_)

          getBanner.foreach(println)

          Try(launchSbtConsole(dabbleHome.history.toString,
                               line,
                               argParser,
                               historyPrinter).
              foldMap(new DabbleConsoleInterpreter)).
            toDisjunction.
            fold(x => println(s"dabble failed with ${x.getMessage}"), {
              case ExecutionResult2(_, SuccessfulAction) =>
                println(s"Dabble completed successfully")
              case ExecutionResult2(Some(error), UnsuccessfulAction) =>
                println(s"Dabble failed with the following error: $error")
              case ExecutionResult2(None, UnsuccessfulAction) =>
                println(s"Dabble failed with errors.")
            })
      }
  }

  private def loadHistory(searchTerm: Option[String],
                          dabbleHome: DabbleHome): Unit = {
    val argParser: CommandlineParser = historyParser.parse(_, DabbleRunConfig())
    val historyPrinter = DabblePrinter.printHistoryLine(_)
    val hMenu: HistoryMenu = historyLines => {
      "Dabble History" +
      newline +
      historyLines.zipWithIndex.map {
        case (line, i) => s"[${i+1}] ${historyPrinter(line)}"
      }.mkString(newline)
    }

    Try(historyProgram(searchTerm,
                       dabbleHome.history.toString,
                       argParser,
                       hMenu,
                       "Please select a menu number or 'q' to quit.",
                       historyPrinter).
        foldMap(new DabbleConsoleInterpreter)).
      toDisjunction.
      fold(x => println(s"dabble failed with: ${x.getMessage}"), {
        case ExecutionResult2(_, SuccessfulAction) =>
          println(s"Dabble completed successfully")
        case ExecutionResult2(errors, UnsuccessfulAction) =>
          println(errors.
                    map(e => s"Dabble exited with the following errors: $e").
                    getOrElse("Dabble exited with errors."))
      })
  }

  parser.parse(args, DabbleRunConfig()) match {
    case Some(drc@DabbleRunConfig(deps, res, mp, None)) =>
      loadDependencies(drc, dabbleHome)

    case Some(DabbleRunConfig(_, _, _, Some(HistoryCommand(searchTerm)))) =>
      loadHistory(searchTerm, dabbleHome)

    case None => //failed to parse options. Parser will indicate error.
  }
}