package net.ssanj.dabble

import scala.collection.mutable.{Map => MMap}
import scalaz._
import scalaz.Id.Id
import scalaz.syntax.either._
import \&/._
import org.scalacheck.Properties
import org.scalacheck.{Prop, Gen}
import org.scalacheck.Prop.BooleanOperators

import DabbleProps._
import DabbleDslDef._
import DabbleHistory.HistoryLinesAndWarnings
import dsl.DependencyCommands.saveHistoryFile

import ScalaCheckSupport._

object SaveHistoryFileProps extends Properties("Saving a history file") {

  class SaveHistoryFileInterpreter(world: MMap[String, Seq[String]]) extends (DabbleDsl ~> Id) {
    def apply[A](value: DabbleDsl[A]): Id[A] = value match {
      case NoOp => //do nothing
      case SystemProp("line.separator") => newline.right[String]
      case Log(message) => {
        world.get("log").
          fold(world += ("log" -> Seq(message)))(
               lgs => world += ("logs" -> (lgs :+ message)))
          ()
      }
      case WriteFile(filename, lines) =>
        if (filename.endsWith("error")) {
          s"Could not write to: $filename".left[Unit]
        }else {
          world += (filename -> lines)
          ().right
        }

      case x => throw new IllegalArgumentException(s"unhandled command: $x")
    }
  }

  val filename = s"dabble.history"

  def hPrinter(line: DabbleHistoryLine): String = md5(line)

  property("should save valid lines and warn about invalid lines") = {
    Prop.forAll(many(2)(genSimpleDabbleHistoryLine)) { validHistoryLines =>

      val selection                     = genSimpleDabbleHistoryLine.sample.get
      val linesInError                  = invalidDependenciesGen.sample.get
      val hlaw: HistoryLinesAndWarnings = Both(linesInError, validHistoryLines)

      val world = MMap[String, Seq[String]]()

      val result =
        saveHistoryFile(filename, selection, hlaw, hPrinter).
          foldMap(new SaveHistoryFileInterpreter(world))

      val actualHistoryContent = world(filename)
      val expectedHistoryContent = (selection +: validHistoryLines).map(hPrinter(_))

      val historyContentProp =
      (actualHistoryContent == expectedHistoryContent) :|
        labeled(actualHistoryContent.mkString(","), expectedHistoryContent.mkString(","))

      val actualWarningLogs = world("log")
      val expectedWarningLogs =
        Seq(s"Dabble could not parse the following history lines:${newline}${tabAsSpaces}${linesInError.mkString(newline + tabAsSpaces)}")

      val warningLogsProp = (actualWarningLogs == expectedWarningLogs) :|
        labeled(actualWarningLogs.mkString(newline), expectedWarningLogs.mkString(newline))

      historyContentProp && warningLogsProp && (result.isRight)
    }
  }

  property("should give an additional warning if all history lines are invalid") = {
    Prop.forAll(genSimpleDabbleHistoryLine) {  selection =>
      val linesInError = invalidDependenciesGen.sample.get
      val hlaw: HistoryLinesAndWarnings = This(linesInError)

      val world = MMap[String, Seq[String]]()

      val result =
          saveHistoryFile(filename, selection, hlaw, hPrinter).
            foldMap(new SaveHistoryFileInterpreter(world))

      val actualHistoryContent   = world(filename)
      val expectedHistoryContent = hPrinter(selection)

      val historyContentProp =
        (actualHistoryContent == Seq(expectedHistoryContent)) :| labeled(
          "history file")(actualHistoryContent.mkString(","), expectedHistoryContent)

      val actualWarningLogs = world("log")
      val expectedWarningLogs =
        Seq(s"Dabble has the following errors:${newline}${tabAsSpaces}All history lines are in error. $filename will be truncated.")

      val warningLogsProp = (actualWarningLogs == expectedWarningLogs) :| labeled(
        "warnings")(actualWarningLogs.mkString(newline).replace(s"${tabAsSpaces}", "<tab>"),
                    expectedWarningLogs.mkString(newline).replace(s"${tabAsSpaces}", "<tab>"))

        historyContentProp && warningLogsProp && (result.isRight)
    }
  }

  property("should accept an empty history file") = {
    Prop.forAll(genSimpleDabbleHistoryLine) {  selection =>
      val hlaw: HistoryLinesAndWarnings = Both(Seq.empty, Seq.empty)

      val world = MMap[String, Seq[String]]()

      val result =
          saveHistoryFile(filename, selection, hlaw, hPrinter).
            foldMap(new SaveHistoryFileInterpreter(world))

      val actualHistoryContent   = world(filename)
      val expectedHistoryContent = Seq(hPrinter(selection))

      val historyContentProp =
        (actualHistoryContent == expectedHistoryContent) :| labeled(
          "history file")(actualHistoryContent.mkString(","), expectedHistoryContent.mkString(","))

      val actualWarningLogs   = world.get("log").map(_.mkString(",")).getOrElse("<no content>")
      val expectedWarningLogs = "<no content>"

      val warningLogsProp = (actualWarningLogs == expectedWarningLogs) :| labeled(
        "warnings")(actualWarningLogs, expectedWarningLogs)

        historyContentProp && warningLogsProp && (result.isRight)
    }
  }


  property("should return an error if the history file can't be written to") = {
      Prop.forAll(genSimpleDabbleHistoryLine) {  selection =>
        val hlaw: HistoryLinesAndWarnings = Both(Seq.empty, Seq.empty)

        val world = MMap[String, Seq[String]]()
        val unwritableFile = s"${filename}.error"

        val result =
          saveHistoryFile(unwritableFile, selection, hlaw, hPrinter).
            foldMap(new SaveHistoryFileInterpreter(world))

        val isLeftProp = booleanProp("isLeft")(result.isLeft, true)

        val content = result.swap.toOption.get

        val messageProp = contentProp("message")(Seq(content), Seq(s"Could not write to: $unwritableFile"))

        isLeftProp && messageProp
      }
  }
}