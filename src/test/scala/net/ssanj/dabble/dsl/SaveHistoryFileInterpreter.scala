package net.ssanj.dabble
package dsl

import scala.collection.mutable.{Map => MMap}

import scalaz._
import scalaz.Id.Id
import scalaz.syntax.either._
import scalaz.syntax.std.option._

import DabbleDslDef._

final class SaveHistoryFileInterpreter(world: MMap[String, Seq[String]]) extends (DabbleDsl ~> Id) {

  def apply[A](value: DabbleDsl[A]): Id[A] = value match {

    case NoOp => //do nothing

    case SystemProp("line.separator") => newline.right[String]

    case Log(message) => {
      world.get("log").
        fold(world += ("log" -> Seq(message)))(
             lgs => world += ("log" -> (lgs :+ message)))
        ()
    }

    case WriteFile(filename, lines) =>
      if (filename.endsWith("error")) {
        s"Could not write to: $filename".left[Unit]
      }else {
        world += (filename -> lines)
        ().right
      }

    case ReadFile(filename) =>
      world.get(s"$filename") \/>(s"could not read filename: $filename")

    case x => throw new IllegalArgumentException(s"unhandled command: $x")
  }
}