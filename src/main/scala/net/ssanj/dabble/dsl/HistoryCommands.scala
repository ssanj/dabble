package net.ssanj.dabble
package dsl

import scala.util.Try

import scalaz._
import scalaz.syntax.either._
import scalaz.syntax.bind._

import ExecutionResult2._
import DabbleResult._
import DabbleHistory._


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
                    hLines: Seq[DabbleHistoryLine]): DabbleScript[HistoryOption] = {

    readInput(prompt).
      flatMap {
            case "q" => liftDS[HistoryOption](QuitHistory)
            case in =>
              import OneBased._

              val maxLines = fromZeroBased(hLines.length)

              Try(in.toInt).
                filter(maxLines.includes).
                map(line => liftDS[HistoryOption](HistorySelection(hLines(fromOneBased(line).toZeroBased)))).
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
               searchTerm match {
                 case Some(term) =>
                   val matchedLines = findBySearchTerm(hLines, term)
                   if (matchedLines.isEmpty) showAlternateOptions(prompt, hMenu, hLines)(term)
                   else showHistoryMenuAndPromptUser(prompt, hMenu, matchedLines)

                 case None => showHistoryMenuAndPromptUser(prompt, hMenu, hLines)
               }
             }
     } yield ho
  }

  def showAlternateOptions(prompt: String,
                           hMenu: HistoryMenu,
                           hLines: Seq[DabbleHistoryLine])(term: String): DabbleScript[HistoryOption] =
    log(s"Could not find matches for: $term.") >>
      promptUserToShowFullHistoryOrQuit(prompt, hMenu, hLines)

  def findBySearchTerm(hLines: Seq[DabbleHistoryLine], term: String): Seq[DabbleHistoryLine] = {
    hLines.filter {
        case DabbleHistoryLine(deps, _, _) =>
          !deps.list.filter {
            case ScalaVersionSupplied(org, name, _, _) =>
              org.contains(term) || name.contains(term)
            case ScalaVersionDerived(org, name, _, _) =>
              org.contains(term) || name.contains(term)
          }.isEmpty
    }
  }

  def promptUserToShowFullHistoryOrQuit(fullHistoryPrompt: String,
                                        hMenu: HistoryMenu,
                                        fullHistory: Seq[DabbleHistoryLine]): DabbleScript[HistoryOption] = for {
    input <- readInput(s"Select 'f' for full history or 'q' to quit.")
    ho <- input match {
      case "f" => showHistoryMenuAndPromptUser(fullHistoryPrompt, hMenu, fullHistory)
      case "q" => liftDS(QuitHistory)
      case _ => promptUserToShowFullHistoryOrQuit(fullHistoryPrompt, hMenu, fullHistory)
    }
  } yield ho

  def showHistoryMenuAndPromptUser(prompt: String,
                                   hMenu: HistoryMenu,
                                   hLines: Seq[DabbleHistoryLine]): DabbleScript[HistoryOption] = {
    log(hMenu(hLines)) >> getUserChoice(prompt, hLines)
  }

  // //4. Program
  def historyProgram(searchTerm: Option[String],
                     historyFileName: String,
                     argParser: CommandlineParser,
                     hMenu: HistoryMenu,
                     prompt: String,
                     historyPrinter: DabbleHistoryLine => String): DabbleScript[DabbleResult] = for {
    //TODO: Simplify readHistoryFile to return Seq.empty if the history file is not found.
    //TODO: readHistoryFile is going to be common. Pull it out.
    //TODO: Do we need to distinguish between an empty history file and an absent one?
    hasHistoryFile <- fileExists(historyFileName)
    result <- if (!hasHistoryFile) noHistory.map(_ => dabbleSuccess)
              else readHistoryFile(historyFileName, argParser).flatMap {
              case -\/(error) =>
                liftDS(dabbleFailure(s"could not read history file: $historyFileName due to: $error"))
              case \/-(hlaw) =>
                chooseHistory(searchTerm, prompt, hlaw, hMenu).flatMap {
                  case QuitHistory => liftDS(dabbleSuccess)
                  case HistorySelection(line) =>
                    val warnings = hlaw.fold(identity, _ => Seq.empty[String], (l, _) => l)
                    launchDabbleAndSaveHistory(historyFileName, line, hlaw, historyPrinter).
                      map(_.fold(l => dabbleFailure(l, warnings:_*),
                                 _ => dabbleSuccess(warnings)))

                }
           }
  } yield result
}