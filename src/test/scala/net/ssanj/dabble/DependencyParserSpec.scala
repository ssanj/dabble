package net.ssanj.dabble

import org.scalatest.{AppendedClues, Matchers, WordSpecLike}
import scalaz._

final class DependencyParserSpec extends Matchers with WordSpecLike with AppendedClues {

  "The dependency parser" should {
    "return dependencies" when {
      "dependencies are separated with a + and have a dangling +" in {
        val input1 = Seq("org.scalaz", "%", "scalaz-core_2.11", "%", "7.1.4", "+")
        val input2 = Seq("com.chuusai", "%%", "shapeless", "%", "2.3.0", "+")
        val input3 = Seq("net.databinder.dispatch", "%", "dispatch-core_2.11", "%", "0.11.2", "+")
        val input  = Seq(input1, input2, input3).flatten

        val \/-(dependencies) = DependencyParser.parseDependencies(input)

        dependencies should have size (3)
        dependencies(0) should be (ScalaVersionSupplied("org.scalaz", "scalaz-core_2.11", "7.1.4"))
        dependencies(1) should be (ScalaVersionDerived("com.chuusai", "shapeless", "2.3.0"))
        dependencies(2) should be (ScalaVersionSupplied("net.databinder.dispatch", "dispatch-core_2.11", "0.11.2"))
      }
    }

    "not return a dependency" when {
      "it is malformed" in {
        val input1 = Seq("org.scalaz") //missing 4 parts
        val input2 = Seq("org.scalaz", "%", "scalaz-core_2.11") //missing 3 parts
        val input3 = Seq("org.scalaz", "scalaz-core_2.11", "%", "7.1.4") //missing 1 part at between org and name
        val input4 = Seq("org.scalaz", "%", "scalaz-core_2.11", "7.1.4") //missing 1 part at between name and version
        val input5 = Seq("org.scalaz", "%", "scalaz-core_2.11", "%%", "7.1.4") //invalid %% part between name and version
        val input6 = Seq("+") //separator with nothing to separate
        val input7 = Seq("org.scalaz", "%", "scalaz-core_2.11", "%", "7.1.4", "+", "+") //double dangling +
        val input  = Seq(input1, input2, input3, input4, input5, input6, input7)

        input.foreach { i =>
          DependencyParser.parseDependencies(i).isLeft should be (true) withClue(s"Expected parser error for: $i")
        }
      }
    }
  }
}