package net.ssanj.dabble

import org.scalacheck.Properties
import org.scalacheck.Prop.BooleanOperators
import org.scalacheck.{Prop, Gen}

import scalaz._
import scalaz.NonEmptyList.nels
import DabbleProps._
import DabbleHistory._
import TerminalSupport._
import ScalaCheckSupport._

object DabbleHistoryProps extends Properties("DabbleHistory file parsing") {

  type Line = Seq[String]

  //as it would appear after parsing on the commandline
  private def genMultipleDependencies: Gen[Seq[Line]] = for {
    n    <- Gen.choose(2, 5)
    deps <- Gen.listOfN(n, genDependency)
  } yield deps

  //as it would appear after parsing on the commandline
  private def genMultipleInvalidDependencies: Gen[Seq[Line]] =
    genMultipleDependencies.map(_.map(_.map(_.replace("%", "#"))))

  //as it would appear on the commandline
  private def genMultipleDependencyLines: Gen[Seq[String]] =
    genMultipleDependencies.map(_.map(_.mkString(" ")))

 //as it would appear on the commandline
 private def genMultipleInvalidDependencyLines: Gen[Seq[String]] = for {
  invalid <- genMultipleInvalidDependencies.map(_.map(_.mkString(" ")))
  empty   <- genEmptyLines
 } yield scala.util.Random.shuffle(invalid ++ empty)

 private def genEmptyLines: Gen[Seq[String]] = Gen.containerOf[List, String](Gen.const(""))

  property("generate valid HistoryLinesOr") =
    Prop.forAllNoShrink(genMultipleDependencyLines, genMultipleInvalidDependencyLines) {
      case (validLines, invalidLines) =>
        val hParser = historyParser.parse(_: Array[String], DabbleRunConfig())

        val validAndInvalidLines = invalidLines ++ validLines
        val parsedLines: HistoryLinesOr = readHistory(hParser)(validAndInvalidLines)

        val lineLengthProp       = lengthProp("line")(parsedLines, validAndInvalidLines)

        val successesLengthProp  = lengthProp("successes")(parsedLines.filter(_.isSuccess), validLines)

        val failuresLengthProp   = lengthProp("failures")(parsedLines.filter(_.isFailure), invalidLines)

        val parsedDependencies   = validAndInvalidLines.map(line =>
                                     DependencyParser.parseDependencies(line.split(" ").toSeq))

        val expectedHistoryLines = parsedDependencies.collect { case \/-(deps) =>  deps }

        val actualHistoryLines   = parsedLines.collect { case Success(dhl) => dhl.dependencies.list.toList }

        val historyLineSuccessContentProp = contentProp("history line")(
          actualHistoryLines, expectedHistoryLines)

        val expectedFailureStrings = parsedDependencies.collect { case -\/(fails) =>  fails }

        val actualFailureStrings   = parsedLines.
                                          collect { case Failure(fails) => fails.list.toList }.
                                          flatten

        val failureStringsContentProp = contentProp("failure Strings")(actualFailureStrings, expectedFailureStrings)

        lineLengthProp &&
        successesLengthProp &&
        failuresLengthProp &&
        historyLineSuccessContentProp &&
        failureStringsContentProp
    }

  property("generate valid HistoryLinesOrWarnings for Both") =
    Prop.forAllNoShrink(genMultipleDependencyLines, genMultipleInvalidDependencyLines) {
        case (validLines, invalidLines) =>

        import \&/._

        val hParser = historyParser.parse(_: Array[String], DabbleRunConfig())

        val validAndInvalidLines = validLines ++ invalidLines
        val hlaw: HistoryLinesAndWarnings = readHistoryWithWarnings(hParser)(validAndInvalidLines)

        val isBothProp = booleanProp("Both")(hlaw.isBoth, true)

        val (parsedThis, parsedThat) = hlaw.onlyBoth.get

        val thisLengthProp = lengthProp("This")(parsedThis, invalidLines)

        val thatLengthProp = lengthProp("That")(parsedThat, validLines)

        val bothLengthProp = lengthProp("Both")(parsedThis ++ parsedThat, validAndInvalidLines)

        val thisContent =
          invalidLines.map(line => DependencyParser.parseDependencies(line.split(" ").toSeq)) collect {
            case -\/(error) => error
          }

        val thatContent =
          validLines.map(line => DependencyParser.parseDependencies(line.split(" ").toSeq)).collect {
            case \/-(deps) => DabbleHistoryLine(nels(deps.head, deps.tail:_*))
          }

        val thisContentProp = contentProp(propName = "This")(thisContent, parsedThis)

        val thatContentProp = contentProp(propName = "That")(thatContent, parsedThat)

        isBothProp      &&
        thisLengthProp  &&
        thatLengthProp  &&
        bothLengthProp  &&
        thisContentProp &&
        thatContentProp
    }

  property("generate valid HistoryLinesOrWarnings for This") =
    Prop.forAllNoShrink(genMultipleInvalidDependencyLines) { invalidLines =>
        import \&/._

        val hParser = historyParser.parse(_: Array[String], DabbleRunConfig())

        val hlaw: HistoryLinesAndWarnings = readHistoryWithWarnings(hParser)(invalidLines)

        val isThisProp = booleanProp("This")(hlaw.isThis, true)

        val parsedThis = hlaw.onlyThis.get

        val thisLengthProp = lengthProp("This")(parsedThis, invalidLines)

        val thisContent =
          invalidLines.map(line => DependencyParser.parseDependencies(line.split(" ").toSeq)) collect {
            case -\/(error) => error
          }

        val thisContentProp = contentProp(propName = "This")(thisContent, parsedThis)

        isThisProp      &&
        thisLengthProp  &&
        thisContentProp
    }

  property("generate valid HistoryLinesOrWarnings for That") =
    Prop.forAllNoShrink(genMultipleDependencyLines) { validLines =>
        import \&/._

        val hParser = historyParser.parse(_: Array[String], DabbleRunConfig())

        val hlaw: HistoryLinesAndWarnings = readHistoryWithWarnings(hParser)(validLines)

        val isThatProp = booleanProp("That")(hlaw.isThat, true)

        val parsedThat = hlaw.onlyThat.get

        val thatLengthProp = lengthProp("That")(parsedThat, validLines)

        val thatContent =
          validLines.map(line => DependencyParser.parseDependencies(line.split(" ").toSeq)) collect {
            case \/-(deps) => DabbleHistoryLine(nels(deps.head, deps.tail:_*))
          }

        val thatContentProp = contentProp(propName = "That")(thatContent, parsedThat)

        isThatProp      &&
        thatLengthProp  &&
        thatContentProp
    }
}