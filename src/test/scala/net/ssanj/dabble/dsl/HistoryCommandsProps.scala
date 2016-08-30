package net.ssanj.dabble
package dsl

import scala.collection.mutable.{Map => MMap}

import org.scalacheck.Properties
import org.scalacheck.{Prop, Gen}

import scalaz.NonEmptyList.nels

import DabblePrinter._
import DabbleProps._
import HistoryCommands._
import net.ssanj.dabble.dsl.HistoryDsl.QuitHistory
import net.ssanj.dabble.dsl.HistoryDsl.HistorySelection
import ScalaCheckSupport._

object HistoryCommandsProps extends Properties("HistoryCommands") {

  property("should findBySearchTerm") =
    Prop.forAll(genMatchingHistorySearchCombo) {
      case HistorySearchCombo(dhls, SearchTerm(term), matched) =>

        val lines = nels(dhls.head, dhls.tail:_*)
        val result = findBySearchTerm(lines, term)

        Prop.collect(s"$term -> deps:${lines.list.length} matches:${matched.size}") { contentProp("searchTerm")(
          result.sortBy(_.toString), matched.sortBy(_.toString)) }
  }

  property("should getUserChoice when the user chooses to quit") = {
      Prop.forAll(genSimpleDabbleHistoryLine) {  dhl =>
        val script      = getUserChoice("select_an_option", nels(dhl))
        val interpreter = new SaveHistoryFileInterpreter(MMap("ReadInput.select_an_option" -> Seq("q")))
        val result      = script.foldMap(interpreter)

        contentProp("getUserChoice:quit")(Seq(result), Seq(QuitHistory))
      }
  }

  property("should getUserChoice when the user chooses a selection") = {
      Prop.forAllNoShrink(many(3)(genSimpleDabbleHistoryLine), Gen.choose(1, 3)) {  (dhls, index) =>
        val script      = getUserChoice("select_an_option", nels(dhls.head, dhls.tail:_*))
        val interpreter = new SaveHistoryFileInterpreter(MMap("ReadInput.select_an_option" -> Seq(index.toString)))
        val result      = script.foldMap(interpreter)

        //The user chooses in a one-based index (because it's more natural to start counting from 1)
        //the program selects in a zero-based index (because thast's how collections are indexed.)
        contentProp("getUserChoice:selection")(Seq(result), Seq(HistorySelection(dhls(index-1))))
      }
  }

  property("should getUserChoice when the user chooses an invalid selection") = {

      def genInvalidUserChoices: Gen[Seq[String]] = for {
         selections <- Gen.listOfN(2, Gen.choose(4, 10))
         chars      <- Gen.listOfN(2, Gen.alphaChar.map(c => if (c == 'q') 'r' else c))
         values     <- Gen.someOf(selections.map(_.toString) ++ chars.map(_.toString))
      } yield values

      Prop.forAllNoShrink(many(3)(genSimpleDabbleHistoryLine), genInvalidUserChoices) {  (dhls, choices) =>
        val script      = getUserChoice("select_an_option", nels(dhls.head, dhls.tail:_*))
        val world       = MMap("ReadInput.select_an_option" -> (choices :+ "q"))
        val interpreter = new SaveHistoryFileInterpreter(world)
        val result      = script.foldMap(interpreter)

        Prop.collect(s"""choice: ${choices.mkString(" ")}""") {
          contentProp("getUserChoice:quit")(Seq(result), Seq(QuitHistory)) &&
          booleanProp("no more input")(world("ReadInput.select_an_option").isEmpty, true)
        }
      }
  }
}