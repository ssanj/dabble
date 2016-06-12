package net.ssanj.dabble

import org.scalacheck.Properties
import org.scalacheck.{Prop, Gen}

import scalaz._
import DabbleProps._
import DabbleHistory._
import TerminalSupport._

object DabbleHistoryProps extends Properties("DabbleHistory file parsing") {

  def removeSpaces(value: String): String = value.replace(" ", "")

  property("generate valid HistoryLinesAndWarnings") =
    Prop.forAllNoShrink(many(2)(genDependency)) { deps =>
        //This represents a String read from the history file.
        //as such it should be the String as it appears on the commandline.
        val validLines = deps.map(_.mkString(" "))
        val hParser = historyParser.parse(_: Array[String], DabbleRunConfig())
        val validHLinesOrErrors: HistoryLinesOr = readHistory(hParser)(validLines)
        val allValidProps = validHLinesOrErrors.length == deps.length && validHLinesOrErrors.forall(_.isSuccess)

        val invalidLines = deps.map(_.map(_.replace("%", "#")).mkString(" "))
        val invalidHLinesOrErrors: HistoryLinesOr = readHistory(hParser)(invalidLines)
        val allInvalidProps = invalidHLinesOrErrors.length == deps.length && invalidHLinesOrErrors.forall(_.isFailure)

        val mixedLines = validLines.take(1) ++ invalidLines.take(1)
        val mixedHLinesOrErrors: HistoryLinesOr = readHistory(hParser)(mixedLines)

        val validMixedLines   = mixedHLinesOrErrors.filter { _.isSuccess }
        val invalidMixedLines = mixedHLinesOrErrors.filter { _.isFailure }

        val validParsedLines = validLines.
                                take(1).
                                map(l => historyParser.parse(l.split(" ").toSeq, DabbleRunConfig())).
                                flatten.
                                map(c => parseHistoryLine(c.dependencies,
                                                          c.resolvers,
                                                          c.macroParadiseVersion).validationNel[String])

        val mixedProps = mixedLines.length == 2              &&
                         validMixedLines.length == 1         &&
                         validMixedLines == validParsedLines &&
                         invalidMixedLines.length == 1

        allValidProps && allInvalidProps && mixedProps
    }
}