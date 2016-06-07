package net.ssanj.dabble

import scala.util.Try

import ammonite.ops._

import scalaz._
import scalaz.syntax.either._
import scalaz.Id.Id
import scalaz.NonEmptyList.nels
import scalaz.syntax.std.`try`._


import DabbleDslDef._
import DabblePaths._
import DabbleHistory._
import DependencyParser._
import ResolverParser._
import Banner._
import TerminalSupport._
import dsl.CommonCommands._
import dsl.DependencyCommands._
import dsl.HistoryCommands._

object DabbleDslRunner extends App {
  val historyFileName = dabbleHome.history.toString

  parser.parse(args, DabbleRunConfig()) match {
    case Some(DabbleRunConfig(deps, res, mp, None)) =>
      val lineE = for {
        dependencies <- parseDependencies(deps)
        resolvers    <- if (res.nonEmpty) parseResolvers(res) else Seq.empty.right[String]
      } yield DabbleHistoryLine(nels(dependencies.head, dependencies.tail:_*), resolvers, mp)

      lineE match {
        case -\/(error) =>
          s"Could not parse inputs: $error."

        case \/-(line)  =>
          val argParser: CommandlineParser = historyParser.parse(_, DabbleRunConfig())
          val historyPrinter = DabblePrinter.printHistoryLine(_)

          getBanner.foreach(println)

          Try(launchSbtConsole(historyFileName,
                               line,
                               argParser,
                               historyPrinter).
              foldMap(new DabbleConsoleInterpreter)).
            toDisjunction.
            fold(x => println(s"dabble failed with ${x.getMessage}"), r => ())
      }

    case Some(DabbleRunConfig(_, _, _, Some(HistoryCommand(searchTerm)))) => ???

    case None => //failed to parse options. Parser will indicate error.
  }
}

class DabbleConsoleInterpreter extends (DabbleDsl ~> Id) {

  def apply[A](dsl: DabbleDsl[A]): Id[A] = dsl match {

    case ReadFile(filename: String) =>
        Try(read.lines(Path(filename), "UTF-8")).
          toDisjunction.
          leftMap(x => s"Failed to read $filename due to: ${x.getMessage}")

    case WriteFile(filename: String, lines: Seq[String]) =>
      Try(write.over(Path(filename), lines.mkString(newline))).
        toDisjunction.
        leftMap(x => s"Failed to write to $filename due to ${x.getMessage}")

    case FileExists(filename: String) =>
      Try(exists(Path(filename))).toOption.fold(false)(identity)

    case Log(message: String) => println(message)

    case ReadInput(prompt: String) =>
      println(prompt)
      scala.io.StdIn.readLine

    case SystemProp(key: String) =>
      Try(System.getProperty(key)).
        toDisjunction.
        leftMap(x => s"Could not read system property: $key due to ${x.getMessage}")

    case CallProcess(filename: String, arguments: String, workingDir: String) =>
      Try(%(Path(filename), arguments)(Path(workingDir))).
      toDisjunction.
      map(code =>
          ExecutionResult2(None,
                          if (code == 0) SuccessfulAction
                          else UnsuccessfulAction)).
      leftMap(x => s"Could not run dabble due to ${x.getMessage}. See sbt log for details.")

    case Exit(er: ExecutionResult2) => System.exit(er.code.code)

    case  NoOp => ???

  }
}