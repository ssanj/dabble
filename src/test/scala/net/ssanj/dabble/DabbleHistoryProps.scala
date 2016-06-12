package net.ssanj.dabble

import org.scalacheck.Properties
import org.scalacheck.Prop.BooleanOperators
import org.scalacheck.{Prop, Gen}

import scalaz._
import DabbleProps._
import DabbleHistory._
import TerminalSupport._

object DabbleHistoryProps extends Properties("DabbleHistory file parsing") {

  def removeSpaces(value: String): String = value.replace(" ", "")

  property("generate valid HistoryLinesOr") =
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

//TODO:Simplify this test
property("generate valid HistoryLinesOrWarnings") =
  Prop.forAllNoShrink(many(5)(genDependency)) { deps =>
      //This represents a String read from the history file.
      //as such it should be the String as it appears on the commandline.
      import \&/._
      val validLines: Seq[String] = deps.map(_.mkString(" "))
      val hParser = historyParser.parse(_: Array[String], DabbleRunConfig())
      val validHlaw: HistoryLinesAndWarnings = readHistoryWithWarnings(hParser)(validLines)

      val validParsedLines = validLines.
                        take(5).
                        map(l => historyParser.parse(l.split(" ").toSeq, DabbleRunConfig())).
                        flatten.
                        map(c => parseHistoryLine(c.dependencies,
                                                  c.resolvers,
                                                  c.macroParadiseVersion)).
                        collect { case \/-(dhl) => dhl }

     val thatProp =
       validHlaw match {
         case That(values) => (values == validParsedLines) :|
           labeled("That")(s"${values.mkString(",")}", s"${validParsedLines.mkString(",")}")
         case This(values) => false :| labeled("That")(s"This: ${values.mkString(",")}", s"${validParsedLines.mkString(",")}")
         case Both(thisValues, thatValues) => false :|
          labeled("That")(s"Both: This:${thisValues.mkString(",")} " +
                          s"That:${thatValues.mkString(",")}", validParsedLines.mkString(","))
       }

    val invalidLines = deps.map(_.map(_.replace("%", "#")))
    val invalidLinesString = invalidLines.map(_.mkString(" "))
    val invalidHlaw: HistoryLinesAndWarnings = readHistoryWithWarnings(hParser)(invalidLinesString)
    val expectedErrors = invalidLines.map(l => s"unable to derive dependencies from: ${l.mkString(",")}")

    val thisProp =
      invalidHlaw match {
        case This(values) => (values == expectedErrors) :|
          labeled("This")(values.mkString(","), expectedErrors.mkString(","))
        case That(values) => false :| labeled("valid")(s"That: ${values.mkString(",")}", expectedErrors.mkString(","))
        case Both(thisValues, thatValues) => false :|
          labeled("This")(s"Both: This:${thisValues.mkString(",")} " +
                          s"That:${thatValues.mkString(",")}", expectedErrors.mkString(","))
      }

      val validBothParsedLines = validLines.
                  drop(2).
                  map(l => historyParser.parse(l.split(" ").toSeq, DabbleRunConfig())).
                  flatten.
                  map(c => parseHistoryLine(c.dependencies,
                                            c.resolvers,
                                            c.macroParadiseVersion)).
                  collect { case \/-(dhl) => dhl }



      val invalidBothLines = deps.take(2).map(_.map(_.replace("%", "#")))
      val invalidBothLinesString: Seq[String] = invalidBothLines.map(_.mkString(" "))
      val bothHlaw: HistoryLinesAndWarnings = readHistoryWithWarnings(hParser)(validLines.drop(2) ++ invalidBothLinesString)

      val expectedBothWarnings = invalidBothLines.map(l => s"unable to derive dependencies from: ${l.mkString(",")}")
      val expectedBothString   = s"Both(this=${expectedBothWarnings.mkString(",")}, that=${validBothParsedLines.mkString(",")})"
      val bothProp =
        bothHlaw match {
          case Both(warnings, dhls) =>
            (warnings == expectedBothWarnings && dhls == validBothParsedLines) :|
              labeled("Both Full")(s"Both(this=${warnings.mkString(",")}, that=${dhls.mkString(",")})", expectedBothString)
          case This(warnings) => false :| labeled("Both Full")(s"This: ${warnings.mkString(",")}", expectedBothString)
          case That(dhls) => false :| labeled("Both Full")(s"That: ${dhls.mkString(",")}", expectedBothString)
        }

      val emptyLines = Seq.empty[String]
      val emptyHlaw: HistoryLinesAndWarnings = readHistoryWithWarnings(hParser)(emptyLines)
      val bothEmptyExpectedString = "Both(this=Seq.empty, that=Seq.empty)"

      val emptyProp =
        emptyHlaw match {
          case b@Both(warnings, dhls) =>
            (warnings == Seq.empty && dhls == Seq.empty) :| labeled("Both")(s"$b", bothEmptyExpectedString)
          case This(warnings) => false :| labeled("Both")(s"This(${warnings.mkString(",")})", bothEmptyExpectedString)
          case That(dhls) => false :| labeled("Both Empty")(s"That(${dhls.mkString(",")})", bothEmptyExpectedString)
        }

      emptyProp && thisProp && thatProp && bothProp
  }
}