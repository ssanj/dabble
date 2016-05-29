package net.ssanj.dabble

import scala.util.Try

import scalaz._
import scalaz.Free._

//Free only encapsulates side effects. Not logic. Logic is performed in the interpreter.
//Free Scipts have to return a Free[DabbleHistory, ?]

object DabbleHistoryDslDef {

  //1. Dsl
  final case class OneBased(value: Int)

  sealed trait HistoryChoice
  final case object ExitHistory extends HistoryChoice //q
  final case class UnhandledInput(value: String) extends HistoryChoice //q
  final case class HistoryLine(line: OneBased) extends HistoryChoice //valid menu index

  sealed trait DabbleHistoryDsl[A]
  final case class ReadHistoryFile(filename: String) extends DabbleHistoryDsl[Seq[String]]
  final case class PrintItem(message: String) extends DabbleHistoryDsl[Unit]
  final case class ReadUserInput(prompt: String) extends DabbleHistoryDsl[String]
  final case class Exit(code: Int) extends DabbleHistoryDsl[Unit]

  type DabbleScript[A] = Free[DabbleHistoryDsl, A]

  //2. Lift functions
  def readHistoryFile(filename: String): DabbleScript[Seq[String]] =
    liftF(ReadHistoryFile(filename))

  def printItem(message: String): DabbleScript[Unit] = liftF(PrintItem(message))

  def readUserInput(prompt: String): DabbleScript[String] = liftF(ReadUserInput(prompt))

  def exit(code: Int): DabbleScript[Unit] = liftF(Exit(code))

  //3. Compose functions
  def getUserChoice(prompt: String, maxLines: OneBased): DabbleScript[HistoryChoice] =
    readUserInput(prompt).
      map {
            case "q" => ExitHistory
            case in =>
              Try(in.toInt).
                filter(n => n >= 1 && n <= maxLines.value).
                map(line => HistoryLine(OneBased(line))).
                getOrElse(UnhandledInput(in))
      }

  def performUserChoice(choice: HistoryChoice, lines: Seq[DabbleHistoryLine], launchDabble: DabbleHistoryLine => Unit): DabbleScript[Unit] = {
    choice match {
      case HistoryLine(OneBased(line)) =>
        launchDabble(lines(line - 1))
        exit(0)

      case UnhandledInput(_) => for {
        _ <- printItem("invalid input")
        _ <- exit(1)
      } yield ()

      case ExitHistory => exit(0)
    }
  }

  def noHistory: DabbleScript[Unit] = for {
    _ <- printItem("You have not made history.")
    _ <- printItem("Dabble writes out a history line when you successfully load a dependency and exit.")
    _ <- exit(0)
  } yield ()

  def hasHistory(prompt: String, hLines: Seq[DabbleHistoryLine], menu: String, launchDabble: DabbleHistoryLine => Unit): DabbleScript[Unit] = {
     val maxLines = OneBased(hLines.length + 1)
     for {
       _      <- printItem(menu)
       choice <- getUserChoice(prompt, maxLines)
       _      <- performUserChoice(choice, hLines, launchDabble)
     } yield ()
  }

  //4. Program
  def historyProgram(historyFileName: String,
                     hParser: String => DabbleHistoryLine,
                     hMenu: Seq[DabbleHistoryLine] => String,
                     prompt: String,
                     launchDabble: DabbleHistoryLine => Unit): DabbleScript[Unit] = for {
    lines <- readHistoryFile(historyFileName)
    hLines = lines.map(hParser)
    _ <-   if (hLines.nonEmpty) hasHistory(prompt, hLines, hMenu(hLines), launchDabble) else noHistory
  } yield ()
}