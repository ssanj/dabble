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

object SaveHistoryFileProps extends Properties("Saving a history file") {

  class SaveHistoryFileInterpreter(world: MMap[String, Seq[String]]) extends (DabbleDsl ~> Id) {
    def apply[A](value: DabbleDsl[A]): Id[A] = value match {
      case NoOp => //do nothing
      case SystemProp("line.separator") => "\n".right[String]
      case Log(message) => {
        world.get("log").
          fold(world += ("log" -> Seq(message)))(
               lgs => world += ("logs" -> (lgs :+ message)))
          ()
      }
      case WriteFile(filename, lines) =>
        world += (filename -> lines)
        ().right
      case x => throw new IllegalArgumentException(s"unhandled command: $x")
    }
  }

  private def md5(values: Any*): String = {
    import java.security.MessageDigest
    MessageDigest.getInstance("MD5").
      digest(values.map(_.toString).mkString.getBytes).map("%02x".format(_)).mkString
  }

  property("should save valid lines and warn about invalid lines") =
    Prop.forAll(many(2)(genSimpleDabbleHistoryLine)) {
      case (validHistoryLines) =>

      val selection = genSimpleDabbleHistoryLine.sample.get
      val linesInError = many(3)(genDependency.map(_.drop(1).mkString(" "))).sample.get

      val hlaw: HistoryLinesAndWarnings = Both(linesInError, validHistoryLines)
      val filename = s"dabble.history"
      def printer(line: DabbleHistoryLine): String = md5(line)

      val world = MMap[String, Seq[String]]()

      val result =
        saveHistoryFile(filename, selection, hlaw, printer).
          foldMap(new SaveHistoryFileInterpreter(world))

      val actualHistoryContent = world(filename)
      val expectedHistoryContent = (selection +: validHistoryLines).map(printer(_))

      val historyContentProp =
      (actualHistoryContent == expectedHistoryContent) :|
        labeled(actualHistoryContent.mkString(","), expectedHistoryContent.mkString(","))

      val actualWarningLogs = world("log")
      val expectedWarningLogs =
        Seq(s"Dabble could not parse the following history lines:${newline}${linesInError.mkString(tabAsSpaces + newline)}")

      val warningLogsProp = (actualWarningLogs == expectedWarningLogs) :|
        labeled(actualWarningLogs.mkString(newline), expectedWarningLogs.mkString(newline))

      historyContentProp && warningLogsProp && (result.isRight)
    }
  }