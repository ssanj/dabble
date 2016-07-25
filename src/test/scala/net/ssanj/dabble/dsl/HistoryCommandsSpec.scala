package net.ssanj.dabble
package dsl

import scala.collection.mutable.{Map => MMap}

import org.scalatest.{Matchers, FlatSpec}

import HistoryDsl._
import HistoryCommands._

final class HistoryCommandsSpec extends FlatSpec with Matchers {

  "A HistoryCommand" should "return a message and quit when there is no history" in {
    val world = MMap[String, Seq[String]]()
    val interpreter = new SaveHistoryFileInterpreter(world)

    val result = noHistory.foldMap(interpreter)

    result should be (QuitHistory)

    val Some(logs) = world.get("log")
    logs should have size (2)
    logs(0) should be ("You have not made history.")
    logs(1) should be ("Dabble writes out a history line when you successfully load a dependency and exit.")
  }
}