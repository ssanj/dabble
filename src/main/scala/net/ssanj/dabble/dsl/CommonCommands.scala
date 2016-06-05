package net.ssanj.dabble
package dsl

import scalaz._

import DabbleHistoryDslDef._

object CommonCommands {
    //TODO: Do we need this or can we leave this to the interpreter?
  def newlinesDS(n: Int): DabbleScript[String] =
    systemProp("line.separator") map {
      case -\/(error) => "\n"
      case \/-(nl) => List.fill(n)(nl).mkString
    }

}

