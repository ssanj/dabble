package net.ssanj.dabble

import scala.util.Try

import scalaz._
import scalaz.Free._


object DabbleHistoryDslDef {

  //1. Dsl
  final case class OneBased(value: Int)

  sealed trait HistoryChoice
  final case object ExitHistory extends HistoryChoice //q
  final case class Unhandled(input: String) extends HistoryChoice //unhandled input
  final case class HistoryLine(line: OneBased) extends HistoryChoice //valid menu index

  sealed trait DabbleHistoryDsl[A]
  final case class ReadHistoryFile(filename: String) extends DabbleHistoryDsl[Seq[DabbleHistoryLine]]
  final case class PrintItem(message: String) extends DabbleHistoryDsl[Unit]
  final case class ReadUserInput(prompt: String, maxLines: OneBased) extends DabbleHistoryDsl[HistoryChoice]
  final case class LaunchDabble(line: DabbleHistoryLine) extends DabbleHistoryDsl[Unit]
  final case class Exit(code: Int) extends DabbleHistoryDsl[Unit]

  type DabbleScript[A] = Free[DabbleHistoryDsl, A]

  //2. Lift functions
  def readHistoryFile(filename: String): DabbleScript[Seq[DabbleHistoryLine]] =
    liftF(ReadHistoryFile(filename))

  def printItem(message: String): DabbleScript[Unit] = liftF(PrintItem(message))

  def readUserInput(prompt: String, maxLines: OneBased): DabbleScript[HistoryChoice] =
    liftF(ReadUserInput(prompt, maxLines))

  def launchDabble(line: DabbleHistoryLine): DabbleScript[Unit] = liftF(LaunchDabble(line))

  def exit(code: Int): DabbleScript[Unit] = liftF(Exit(code))

  //3. Compose functions
  def getUserChoice(maxLines: OneBased, hLines: Seq[DabbleHistoryLine]): DabbleScript[Unit] = for {
    choice <- readUserInput(
                  s"Please select a number between 1 and ${maxLines.value} or type q to exit",
                  maxLines)
    _      <- choice match {
              case ExitHistory                 => exit(0)
              case Unhandled(_)                => getUserChoice(maxLines, hLines)
              case HistoryLine(OneBased(line)) => launchDabble(hLines(line - 1))
            }
  } yield ()

  //4. Program
  def historyProgram(historyFileName: String): DabbleScript[Unit] = for {
    hLines <- readHistoryFile(historyFileName)
    _ <-   if (hLines.nonEmpty) {
            val menu = "~menu~"
            val maxLines = OneBased(hLines.length + 1)
            for {
              _ <- printItem(menu)
              _ <- getUserChoice(maxLines, hLines)
            } yield ()
          } else {
            for {
              _ <- printItem("You have not made history.")
              _ <- printItem("Dabble writes out a history line when you successfully load a dependency and exit.")
            } yield ()
            exit(0)
          }
  } yield ()

  // def askForHistoryLine(prompt: String, maxLines: OneBased): DabbleScript[HistoryChoice] = for {
  //   input  <- readUserInput(prompt)
  //   choice <- if (input == "q") ExitHistory
  //             else {
  //               Try(input.toInt).toOption.fold(askForHistoryLine(prompt, maxLines)){ln =>
  //                 if (ln >= 1 && ln <= maxLines.value) HistoryLine(OneBased(ln))
  //                 else askForHistoryLine(prompt, maxLines)
  //               }
  //             }
  // } yield choice
}