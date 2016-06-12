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
    Prop.forAllNoShrink(manyPairs(5)(genDependencyList, genResolverStrings)) {
      case pairs =>
        //This represents a String read from the history file.
        //as such it should be the String as it appears on the commandline.
        val validLines = pairs.map {
          case (deps, resolvers) =>
            //Remove all whitespace from the resolvers. When the ResolverParser processes resolvers, it
            //trims the whitespace on custom resolvers (those with a custom url). We do something similar
            //here to ensure that we can load back any generated resolvers even those which have whitespace.
            //The whitespace trimmer here is more general and removes all whitespace as opposed to that in
            //the resolver parser which only removes whitespace around the name and url of a custom resolver.
            ((deps :+ "-r") :+ removeSpaces(resolvers)).mkString(" ")
        }

        val hParser = historyParser.parse(_: Array[String], DabbleRunConfig())
        val validHLinesOrErrors: HistoryLinesOr = readHistory(hParser)(validLines)

        val allValidProps = validHLinesOrErrors.length == pairs.length && validHLinesOrErrors.forall(_.isSuccess)

        val invalidLines = pairs.map {
          case (deps, resolvers) =>
            //remove dependency concatenation operator '+' and replace resolver concatenation operation ',' with ':'
            ((deps.map(_.replace("+", "")) :+ "-r") :+ resolvers.replace(",", ":")).mkString(" ")
        }

        val invalidHLinesOrErrors: HistoryLinesOr = readHistory(hParser)(invalidLines)

        val allInvalidProps = invalidHLinesOrErrors.length == pairs.length && invalidHLinesOrErrors.forall(_.isFailure)

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
                         invalidMixedLines.length == 1

        allValidProps && allInvalidProps && mixedProps
    }
}