package net.ssanj.dabble

import scala.util.Try

import ammonite.ops._

import scalaz.{-\/, \/-, \/}
import scalaz.syntax.either._
import scalaz.NonEmptyList.nels
import scalaz.syntax.std.`try`._

import DabbleDslDef._
import DabblePaths._
import DabblePathTypes._
import DabbleHistory._
import DependencyParser._
import ResolverParser._
import Banner._
import TerminalSupport._
import dsl.DependencyCommands._
import dsl.HistoryCommands._

object DabbleDslRunner extends App {

  private def loadDependencies(drc: DabbleRunConfig): Unit = {
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

          val result =
            Try(launchSbtConsole(dabbleHomePath(userHome),
                                 line,
                                 argParser,
                                 historyPrinter).
                foldMap(new DabbleConsoleInterpreter)).
              toDisjunction

          handleProgramResult(result)
      }
  }

  private def loadHistory(searchTerm: Option[String]): Unit = {
    val argParser: CommandlineParser = historyParser.parse(_, DabbleRunConfig())
    val historyPrinter = DabblePrinter.printHistoryLine(_)
    val hMenu: HistoryMenu = historyLines => {
      "Dabble History" +
      newline +
      historyLines.zipWithIndex.map {
        case (line, i) => s"[${i + 1}] ${historyPrinter(line)}"
      }.mkString(newline)
    }

    val result =
      Try(historyProgram(searchTerm,
                         dabbleHomePath(userHome),
                         argParser,
                         hMenu,
                         "Please select a menu number or 'q' to quit.",
                         historyPrinter).
          foldMap(new DabbleConsoleInterpreter)).
        toDisjunction

      handleProgramResult(result)
  }

  private def handleProgramResult(result: Throwable \/ DabbleResult): Unit = {
    result.
      fold(x => println(s"Dabble failed with: ${x.getMessage}"), {
        case DabbleSuccess(Seq()) =>
          println("Dabble completed successfully.")
        case DabbleSuccess(warnings) =>
          println(s"Dabble completed successfully, but had the following warnings:${newline}")
          warnings.foreach { w => println(w) }
        case DabbleFailure(failures) =>
          println(s"Dabble failed with the following error:${newline}${failures.head}")
          if (failures.tail.nonEmpty) {
            println(s"${newline}and had the following warnings:${newline}")
            failures.tail.toList.foreach { w => println(w) }
          } else {}
      })
  }

  parser.parse(args, DabbleRunConfig()) match {
    case Some(drc@DabbleRunConfig(deps, res, mp, None)) =>
      loadDependencies(drc)

    case Some(DabbleRunConfig(_, _, _, Some(HistoryCommand(searchTerm)))) =>
      loadHistory(searchTerm)

    case None => //failed to parse options. Parser will indicate error.
  }
}
