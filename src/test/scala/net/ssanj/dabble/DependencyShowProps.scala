package net.ssanj.dabble

import org.scalacheck.Properties
import org.scalacheck.{Prop, Gen}
import org.scalacheck.Gen.{posNum, negNum}
import scalaz._
import scalaz.std.list._
import net.ssanj.dabble.Implicits._

object DependencyShowProps extends Properties("Show instance for Dependency") with DabbleProps {

  property("show instances of Dependency") =
    Prop.forAll(genDependency) { inputs: Seq[String] =>

        val \/-(Seq(dep))     = DependencyParser.parse(inputs)
        val output: String    = inputs.map(i => if (!i.startsWith("%")) s""""$i"""" else i).mkString(" ")
        val genOutput: String = Show[Dependency].shows(dep)
        genOutput == output
    }
}