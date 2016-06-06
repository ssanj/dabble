package net.ssanj.dabble
package dsl

import scalaz._
import scalaz.syntax.either._

import DabbleHistoryDslDef._
import DabbleHistory._

object CommonCommands {
    //TODO: Do we need this or can we leave this to the interpreter?
  def newlinesDS(n: Int): DabbleScript[String] =
    systemProp("line.separator") map {
      case -\/(error) => "\n"
      case \/-(nl) => List.fill(n)(nl).mkString
    }

  def readHistoryFile(historyFileName: String, argParser: CommandlineParser): DabbleScript[ErrorOr[HistoryLinesAndWarnings]] = for {
    linesE <- readFile(historyFileName)
    hfStatus = linesE match {
      case -\/(e) => e.left
      case \/-(lines) => (readHistoryWithWarnings(argParser)(lines)).right
    }

  } yield hfStatus

}

