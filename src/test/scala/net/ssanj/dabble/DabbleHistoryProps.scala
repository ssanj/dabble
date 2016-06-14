package net.ssanj.dabble

import org.scalacheck.Properties
import org.scalacheck.Prop.BooleanOperators
import org.scalacheck.{Prop, Gen}

import scalaz._
import scalaz.NonEmptyList.nels
import DabbleProps._
import DabbleHistory._
import TerminalSupport._

object DabbleHistoryProps extends Properties("DabbleHistory file parsing") {

  // def removeSpaces(value: String): String = value.replace(" ", "")

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
 private def genMultipleInvalidDependencyLines: Gen[Seq[String]] =
  genMultipleInvalidDependencies.map(_.map(_.mkString(" ")))

  def commaS(values: Seq[Any]): String = values.mkString(",")

  property("generate valid HistoryLinesOr") =
    Prop.forAllNoShrink(genMultipleDependencyLines, genMultipleInvalidDependencyLines) {
      case (validLines, invalidLines) =>
        val hParser = historyParser.parse(_: Array[String], DabbleRunConfig())

        val validAndInvalidLines = invalidLines ++ validLines
        val parsedLines: HistoryLinesOr = readHistory(hParser)(validAndInvalidLines)

        val expectedLineLength = validLines.length + invalidLines.length
        val actualLineLength   = parsedLines.length
        val lineLengthProp     = (actualLineLength == expectedLineLength) :|
          labeled("line length")(expectedLineLength.toString, actualLineLength.toString)

        val expectedSuccessesLength = validLines.length
        val actualSuccessesLength   = parsedLines.filter(_.isSuccess).length
        val successesLengthProp     = (actualSuccessesLength == expectedSuccessesLength) :|
          labeled("no. successes")(actualSuccessesLength.toString, expectedSuccessesLength.toString)

        val expectedFailuresLength = invalidLines.length
        val actualFailuresLength   = parsedLines.filter(_.isFailure).length
        val failuresLengthProp     = (actualFailuresLength == expectedFailuresLength) :|
          labeled("no. failures")(actualFailuresLength.toString, expectedFailuresLength.toString)

        val parsedDependencies = validAndInvalidLines.map(line =>
                                  DependencyParser.parseDependencies(line.split(" ").toSeq))

        val expectedHistoryLines          = parsedDependencies.
                                              collect { case \/-(deps) =>  deps }
        val actualHistoryLines            = parsedLines.
                                              collect { case Success(dhl) => dhl.dependencies.list.toList }
        val historyLineSuccessContentProp = (actualHistoryLines == expectedHistoryLines) :|
          labeled("history line content")(commaS(actualHistoryLines), commaS(expectedHistoryLines))

        val expectedFailureStrings    = parsedDependencies.collect { case -\/(fails) =>  fails }
        val actualFailureStrings      = parsedLines.
                                          collect { case Failure(fails) => fails.list.toList }.
                                          flatten
        val failureStringsContentProp = (actualFailureStrings == expectedFailureStrings) :|
          labeled("failure strings")(commaS(actualFailureStrings), commaS(expectedFailureStrings))

        lineLengthProp &&
        successesLengthProp &&
        failuresLengthProp &&
        historyLineSuccessContentProp &&
        failureStringsContentProp
    }

 //TODO: Test for empty lines

 private def lengthProp[A, B](propName: String)(actual: Seq[A], expected: Seq[B]): Prop = {
  val actualLength   = actual.length
  val expectedLength = expected.length

  (actualLength == expectedLength) :|
    labeled(s"$propName Length")(actualLength.toString, expectedLength.toString)
 }

 private def contentProp[A, B](propName: String, format: Seq[Any] => String = commaS)(
  actual: Seq[A], expected: Seq[B]): Prop = {

  (actual == expected) :|
    labeled(s"$propName Content")(format(actual), format(expected))
 }

 private def booleanProp(propName: String)(actual: Boolean, expected: Boolean): Prop = {
  (actual == expected) :| labeled(s"Is $propName")(actual.toString, expected.toString)
}


//TODO:Simplify this test
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
    //   val validParsedLines = validLines.
    //                     take(5).
    //                     map(l => historyParser.parse(l.split(" ").toSeq, DabbleRunConfig())).
    //                     flatten.
    //                     map(c => parseHistoryLine(c.dependencies,
    //                                               c.resolvers,
    //                                               c.macroParadiseVersion)).
    //                     collect { case \/-(dhl) => dhl }

    //  val thatProp =
    //    validHlaw match {
    //      case That(values) => (values == validParsedLines) :|
    //        labeled("That")(s"${values.mkString(",")}", s"${validParsedLines.mkString(",")}")
    //      case This(values) => false :| labeled("That")(s"This: ${values.mkString(",")}", s"${validParsedLines.mkString(",")}")
    //      case Both(thisValues, thatValues) => false :|
    //       labeled("That")(s"Both: This:${thisValues.mkString(",")} " +
    //                       s"That:${thatValues.mkString(",")}", validParsedLines.mkString(","))
    //    }

    // val invalidLines = deps.map(_.map(_.replace("%", "#")))
    // val invalidLinesString = invalidLines.map(_.mkString(" "))
    // val invalidHlaw: HistoryLinesAndWarnings = readHistoryWithWarnings(hParser)(invalidLinesString)
    // val expectedErrors = invalidLines.map(l => s"unable to derive dependencies from: ${l.mkString(",")}")

    // val thisProp =
    //   invalidHlaw match {
    //     case This(values) => (values == expectedErrors) :|
    //       labeled("This")(values.mkString(","), expectedErrors.mkString(","))
    //     case That(values) => false :| labeled("valid")(s"That: ${values.mkString(",")}", expectedErrors.mkString(","))
    //     case Both(thisValues, thatValues) => false :|
    //       labeled("This")(s"Both: This:${thisValues.mkString(",")} " +
    //                       s"That:${thatValues.mkString(",")}", expectedErrors.mkString(","))
    //   }

    //   val validBothParsedLines = validLines.
    //               drop(2).
    //               map(l => historyParser.parse(l.split(" ").toSeq, DabbleRunConfig())).
    //               flatten.
    //               map(c => parseHistoryLine(c.dependencies,
    //                                         c.resolvers,
    //                                         c.macroParadiseVersion)).
    //               collect { case \/-(dhl) => dhl }



    //   val invalidBothLines = deps.take(2).map(_.map(_.replace("%", "#")))
    //   val invalidBothLinesString: Seq[String] = invalidBothLines.map(_.mkString(" "))
    //   val bothHlaw: HistoryLinesAndWarnings = readHistoryWithWarnings(hParser)(validLines.drop(2) ++ invalidBothLinesString)

    //   val expectedBothWarnings = invalidBothLines.map(l => s"unable to derive dependencies from: ${l.mkString(",")}")
    //   val expectedBothString   = s"Both(this=${expectedBothWarnings.mkString(",")}, that=${validBothParsedLines.mkString(",")})"
    //   val bothProp =
    //     bothHlaw match {
    //       case Both(warnings, dhls) =>
    //         (warnings == expectedBothWarnings && dhls == validBothParsedLines) :|
    //           labeled("Both Full")(s"Both(this=${warnings.mkString(",")}, that=${dhls.mkString(",")})", expectedBothString)
    //       case This(warnings) => false :| labeled("Both Full")(s"This: ${warnings.mkString(",")}", expectedBothString)
    //       case That(dhls) => false :| labeled("Both Full")(s"That: ${dhls.mkString(",")}", expectedBothString)
    //     }

    //   val emptyLines = Seq.empty[String]
    //   val emptyHlaw: HistoryLinesAndWarnings = readHistoryWithWarnings(hParser)(emptyLines)
    //   val bothEmptyExpectedString = "Both(this=Seq.empty, that=Seq.empty)"

    //   val emptyProp =
    //     emptyHlaw match {
    //       case b@Both(warnings, dhls) =>
    //         (warnings == Seq.empty && dhls == Seq.empty) :| labeled("Both")(s"$b", bothEmptyExpectedString)
    //       case This(warnings) => false :| labeled("Both")(s"This(${warnings.mkString(",")})", bothEmptyExpectedString)
    //       case That(dhls) => false :| labeled("Both Empty")(s"That(${dhls.mkString(",")})", bothEmptyExpectedString)
    //     }

    //   emptyProp && thisProp && thatProp && bothProp
  // }
}