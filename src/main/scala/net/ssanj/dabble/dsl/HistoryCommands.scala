package net.ssanj.dabble
package dsl

import scala.util.Try

import scalaz._
import scalaz.NonEmptyList.nels
import scalaz.syntax.bind._

import DabbleResult._
import DabbleHistory._
import DabblePathTypes._


import DabbleDslDef._
import CommonCommands._
import DependencyCommands._

object HistoryDsl {

  final case class OneBased private (value: Int) {
    val toZeroBased: Int = Math.max(0, value - 1)
    def includes(n: Int): Boolean = n >= OneBased.MIN && n <= value
  }

  object OneBased {
    val MIN = 1

    def fromZeroBased(value: Int): OneBased = fromOneBased(value + 1)
    def fromOneBased(value: Int): OneBased = OneBased(Math.max(MIN, value))
  }

  sealed trait HistoryOption
  final case object QuitHistory extends HistoryOption
  final case class HistorySelection(line: DabbleHistoryLine) extends HistoryOption
}

object HistoryCommands {

  import HistoryDsl._

  //3. Compose functions
  def getUserChoice(prompt: String,
                    hLines: NonEmptyList[DabbleHistoryLine]): DabbleScript[HistoryOption] = {

    readInput(prompt).
      flatMap {
            case "q" => liftDS[HistoryOption](QuitHistory)
            case in =>
              import OneBased._

              val maxLines = fromZeroBased(hLines.list.length)

              Try(in.toInt).
                filter(maxLines.includes).
                map(line => liftDS[HistoryOption](HistorySelection(hLines.list.toList(fromOneBased(line).toZeroBased)))).
                getOrElse(getUserChoice(prompt, hLines))
      }
  }

  def noHistory: DabbleScript[HistoryOption] = for {
    _ <- log("You have not made history.")
    _ <- log("Dabble writes out a history line when you successfully load a dependency and exit.")
  } yield QuitHistory

  def chooseHistory(searchTerm: Option[String],
                    prompt: String,
                    hlaw: HistoryLinesAndWarnings,
                    hMenu: HistoryMenu): DabbleScript[HistoryOption] = {

     val hLines = hlaw.onlyThat.getOrElse(Seq.empty)
     for {
       ho <- if (hLines.isEmpty) noHistory
             else {
               val nelHlines: NonEmptyList[DabbleHistoryLine] = nels(hLines.head, hLines.tail:_*)
               searchTerm match {
                 case Some(term) =>
                   val matchedLines = findBySearchTerm(nelHlines, term)
                   if (matchedLines.isEmpty) showAlternateOptions(prompt, hMenu, nelHlines)(term)
                   else showHistoryMenuAndPromptUser(prompt, hMenu, nels(matchedLines.head, matchedLines.tail:_*))

                 case None => showHistoryMenuAndPromptUser(prompt, hMenu, nelHlines)
               }
             }
     } yield ho
  }

  def showAlternateOptions(prompt: String,
                           hMenu: HistoryMenu,
                           hLines: NonEmptyList[DabbleHistoryLine])(term: String): DabbleScript[HistoryOption] =
    log(s"Could not find matches for: $term.") >>
      promptUserToShowFullHistoryOrQuit(prompt, hMenu, hLines)

  def findBySearchTerm(hLines: NonEmptyList[DabbleHistoryLine], term: String): Seq[DabbleHistoryLine] = {
    hLines.list.filter {
        case DabbleHistoryLine(deps, _, _) =>
          deps.list.toList.exists {
            case ScalaVersionSupplied(org, name, _, _) =>
              org.contains(term) || name.contains(term)
            case ScalaVersionDerived(org, name, _, _) =>
              org.contains(term) || name.contains(term)
          }
    }.toList
  }

  def promptUserToShowFullHistoryOrQuit(fullHistoryPrompt: String,
                                        hMenu: HistoryMenu,
                                        fullHistory: NonEmptyList[DabbleHistoryLine]): DabbleScript[HistoryOption] = for {
    input <- readInput("Select 'f' for full history or 'q' to quit.")
    ho <- input match {
      case "f" => showHistoryMenuAndPromptUser(fullHistoryPrompt, hMenu, fullHistory)
      case "q" => liftDS(QuitHistory)
      case _ => promptUserToShowFullHistoryOrQuit(fullHistoryPrompt, hMenu, fullHistory)
    }
  } yield ho

  def showHistoryMenuAndPromptUser(prompt: String,
                                   hMenu: HistoryMenu,
                                   hLines: NonEmptyList[DabbleHistoryLine]): DabbleScript[HistoryOption] = {
    log(hMenu(hLines.list.toList)) >> getUserChoice(prompt, hLines)
  }

  // //4. Program
  def historyProgram(searchTerm: Option[String],
                     dabbleHomePath: DabbleHomePath,
                     argParser: CommandlineParser,
                     hMenu: HistoryMenu,
                     prompt: String,
                     historyPrinter: DabbleHistoryLine => String): DabbleScript[DabbleResult] = for {
    //TODO: Simplify readHistoryFile to return Seq.empty if the history file is not found.
    //TODO: readHistoryFile is going to be common. Pull it out.
    //TODO: Do we need to distinguish between an empty history file and an absent one?
    hasHistoryFile <- fileExists(dabbleHomePath.history.path.file)
    result <- {
      if (!hasHistoryFile) noHistory.map(_ => dabbleSuccess)
      else readHistoryFile(dabbleHomePath.history.path.file, argParser).flatMap {
        case -\/(error) =>
          liftDS(dabbleFailure(s"could not read history file: ${dabbleHomePath.history.path.file} due to: $error"))
        case \/-(hlaw) =>
          chooseHistory(searchTerm, prompt, hlaw, hMenu).flatMap {
            case QuitHistory => liftDS(dabbleSuccess)
            case HistorySelection(line) =>
              launchSbtConsole(dabbleHomePath, line, argParser, historyPrinter).map { launchResult =>
                val warnings = getWarnings(hlaw)
                launchResult match {
                  case DabbleSuccess(w) => DabbleSuccess(w ++ warnings)
                  case DabbleFailure(errors) => DabbleFailure(errors :::> IList.fromList(warnings.toList))
                }
              }
          }
     }
    }
  } yield result
}
