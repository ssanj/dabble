package net.ssanj.dabble

import org.scalacheck._
import org.scalacheck.Prop.BooleanOperators

import DabbleProps._

trait ScalaCheckSupport {

  def lengthProp[A, B](propName: String)(actual: Seq[A], expected: Seq[B]): Prop = {
    val actualLength   = actual.length
    val expectedLength = expected.length

    (actualLength == expectedLength) :|
      labeled(s"$propName Length")(actualLength.toString, expectedLength.toString)
  }

  def contentProp[A, B](propName: String, format: Seq[Any] => String = commaS)(
    actual: Seq[A], expected: Seq[B]): Prop = {

    (actual == expected) :|
       labeled(s"$propName Content")(format(actual), format(expected))
  }

  def booleanProp(propName: String)(actual: Boolean, expected: Boolean): Prop = {
    (actual == expected) :| labeled(s"Is $propName")(actual.toString, expected.toString)
  }

  def commaS(values: Seq[Any]): String = values.mkString(",")

}

object ScalaCheckSupport extends ScalaCheckSupport