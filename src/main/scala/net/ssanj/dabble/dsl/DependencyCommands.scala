package net.ssanj.dabble
package dsl

import DabbleHistory._
import DabbleHistoryDslDef._

object DependencyCommands {

  //TODO: We need the HLAW (history lines read)
  //TODO: This should actually return a ExecutionResult2. If it succeeds then we can save history.
  def launchDabble(line: DabbleHistoryLine): DabbleScript[ExecutionResult2] = ???

  def launchDabbleAndSaveHistory(historyFileName: String,
                                 line: DabbleHistoryLine,
                                 hlaw: HistoryLinesAndWarnings,
                                 historyPrinter: DabbleHistoryLine => String):DabbleScript[ExecutionResult2] = {
     combineEV(launchDabble(line),
               saveHistoryFile(historyFileName,
                               line,
                               hlaw.onlyThat.getOrElse(Seq.empty),
                               historyPrinter))
  }

  def saveHistoryFile(filename: String, selection: DabbleHistoryLine, hLines: Seq[DabbleHistoryLine],
    historyPrinter: DabbleHistoryLine => String):
    DabbleScript[ErrorOr[Unit]] = {
      import scala.collection.mutable.LinkedHashSet
      val uniqueHLines = LinkedHashSet() ++ (selection +: hLines)
      writeFile(filename, uniqueHLines.map(historyPrinter).toSeq)
    }

}

