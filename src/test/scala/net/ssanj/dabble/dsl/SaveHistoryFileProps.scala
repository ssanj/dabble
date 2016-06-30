package net.ssanj.dabble
package dsl

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
import DependencyCommands.saveHistoryFile

import ScalaCheckSupport._

object SaveHistoryFileProps extends Properties("Saving a history file") {

  val filename = s"dabble.history"

  def hPrinter(line: DabbleHistoryLine): String = md5(line)

  property("should save valid lines") = {
    Prop.forAll(many(2)(genSimpleDabbleHistoryLine)) { validHistoryLines =>

      val selection                     = genSimpleDabbleHistoryLine.sample.get
      val world = MMap[String, Seq[String]]()

      val result =
        saveHistoryFile(filename, selection, validHistoryLines, hPrinter).
          foldMap(new SaveHistoryFileInterpreter(world))

      val actualHistoryContent = world(filename)
      val expectedHistoryContent = (selection +: validHistoryLines).map(hPrinter(_))

      val historyContentProp =
      (actualHistoryContent == expectedHistoryContent) :|
        labeled(actualHistoryContent.mkString(","), expectedHistoryContent.mkString(","))

      historyContentProp && (result.isRight)
    }
  }

  property("should accept an empty history file") = {
    Prop.forAll(genSimpleDabbleHistoryLine) {  selection =>
      val world = MMap[String, Seq[String]]()

      val result =
          saveHistoryFile(filename, selection, Seq.empty[DabbleHistoryLine], hPrinter).
            foldMap(new SaveHistoryFileInterpreter(world))

      val actualHistoryContent   = world(filename)
      val expectedHistoryContent = Seq(hPrinter(selection))

      val historyContentProp =
        (actualHistoryContent == expectedHistoryContent) :| labeled(
          "history file")(actualHistoryContent.mkString(","), expectedHistoryContent.mkString(","))

        historyContentProp && (result.isRight)
    }
  }


  property("should return an error if the history file can't be written to") = {
      Prop.forAll(genSimpleDabbleHistoryLine) {  selection =>

        val world = MMap[String, Seq[String]]()
        val unwritableFile = s"${filename}.error"

        val result =
          saveHistoryFile(unwritableFile, selection, Seq.empty[DabbleHistoryLine], hPrinter).
            foldMap(new SaveHistoryFileInterpreter(world))

        val isLeftProp = booleanProp("isLeft")(result.isLeft, true)

        val content = result.swap.toOption.get

        val messageProp = contentProp("message")(Seq(content), Seq(s"Could not write to: $unwritableFile"))

        isLeftProp && messageProp
      }
  }
}