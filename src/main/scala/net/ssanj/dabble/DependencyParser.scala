package net.ssanj.dabble

import scalaz._
import scalaz.syntax.either._

/**
 * ADT to represent the two types of dependencies we care about:
 *  1. Those specified with a org '''%''' name % version => where the scala version is appended to name.
 *  1. Those specified with a org '''%%''' name % version => where the scala version is derived by SBT.
 */
sealed trait Dependency
final case class ScalaVersionSupplied(org: String, name: String, version: String) extends Dependency
final case class ScalaVersionDerived (org: String, name: String, version: String) extends Dependency

/**
 * Parses the inputs into a Seq[Dependency. Dependencies can be specified in two forms:
 *  1. org %% name % version [[ScalaVersionDerived]]
 *  1. org  % name % version [[ScalaVersionSupplied]]
 *
 * Multiple dependencies should be separated by "+".
 *
 * A dangling "+" at the very end is ignored. This is by design.
 *
 * Example:
 * {{{
 *     val input1 = Seq("org.scalaz", "%", "scalaz-core_2.11", "%", "7.1.4", "+")
 *     val input2 = Seq("com.chuusai", "%%", "shapeless", "%", "2.3.0", "+")
 *     val input3 = Seq("net.databinder.dispatch", "%", "dispatch-core_2.11", "%", "0.11.2", "+")
 *     val inputs = Seq(input1, input2, input3).flatten
 *
 *     DependencyParser.parse(inputs)
 * }}}
 *
 */
trait DependencyParser  {

  /**
   * Parsers inputs into a Seq[Dependency].
   * @param inputs The inputs representing dependencies.
   * @return A -\/ with an error  or a \/- with a Seq[Dependency].
   */
  def parse(inputs: Seq[String]): String \/ Seq[Dependency] = {
    if (inputs.isEmpty) s"unable to derive dependencies from: $inputs".left
    else parse(inputs, Seq.empty[Dependency])
  }

  private def parse(inputs: Seq[String], dependencies: Seq[Dependency]): String \/ Seq[Dependency] = inputs match {
    case Seq() => dependencies.right
    case Seq(org, "%%", name, "%", version, t@_*) =>
      parse(t, dependencies :+ ScalaVersionDerived(org, name, version))
    case Seq(org, "%", name, "%", version, t@_*) =>
      parse(t, dependencies :+ ScalaVersionSupplied(org, name, version))
    case Seq("+", org, "%%", name, "%", version, t@_*) =>
      parse(t, dependencies :+ ScalaVersionDerived(org, name, version))
    case Seq("+", org, "%", name, "%", version, t@_*) =>
      parse(t, dependencies :+ ScalaVersionSupplied(org, name, version))
    case Seq("+") if (!dependencies.isEmpty) => dependencies.right
    case xs =>
      s"unable to derive dependencies from: $xs".left
  }
}

object DependencyParser extends DependencyParser