package net.ssanj.dabble
package dsl

import scala.collection.mutable.{Map => MMap}

import org.scalacheck.Properties
import org.scalacheck.{Prop, Gen}

import HistoryCommands._
import ScalaCheckSupport._
import DabbleProps._
import DabblePrinter._

object HistoryCommandsProps extends Properties("HistoryCommands") {

  property("should findBySearchTerm") =
    Prop.forAll(genMatchingHistorySearchCombo) {
      case HistorySearchCombo(dhls, SearchTerm(term), matched) =>

        val result = findBySearchTerm(dhls, term)

        Prop.collect(s"$term -> deps:${dhls.size} matches:${matched.size}") { contentProp("searchTerm")(
          result.sortBy(_.toString), matched.sortBy(_.toString)) }
  }
}