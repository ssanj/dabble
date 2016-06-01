package net.ssanj.dabble

import scalaz._
import scalaz.Id._

import DabbleHistoryDslDef._

class ConsoleInterpreter(historyFileContents: Seq[String]) extends (DabbleHistoryDsl ~> Id ) {

  def apply[A](dh: DabbleHistoryDsl[A]): Id[A] = dh match {

    case ReadHistoryFile(filename: String) =>
      println(s"reading $filename")
      historyFileContents

    case PrintItem(message: String) => println(message)

    case ReadUserInput(prompt: String) =>
      println(prompt)
      scala.io.StdIn.readLine

    case Exit(code: Int) => System.exit(code)

    case _ => ???
  }
}

import scala.collection.mutable.ListBuffer

class TestInterpreter(val world: Map[String, Any], val actions: ListBuffer[String]) extends (DabbleHistoryDsl ~> Id) {

  def apply[A](dsl: DabbleHistoryDsl[A]): Id[A] = dsl match {
    case ReadHistoryFile(filename: String) =>
      actions += s"reading file: $filename"
      world(filename).asInstanceOf[Seq[String]]

    case PrintItem(message: String) =>
      actions += s"printing item: $message"
      ()

    case ReadUserInput(prompt: String) =>
      actions += s"prompt: $prompt"
      val f = world("user.input").asInstanceOf[Function0[String]]
      val choice = f()
      actions += s"user chose: $choice"
      choice

    case Exit(code: Int) => actions += s"exiting with code: $code"
    ()

    case _ => ???
  }
}

object DabbleHistoryRunner extends App with DabblePrinter with DependencyParser {

  private def withConsoleInterpreter(): Unit = {
    val historyLines = Seq("org.scalaz %% scalaz-core % 7.2.3")
    val drawMenu: Seq[DabbleHistoryLine] => String = lines => {
      s"Dabble History${newlines(2)}" ++
      (lines.zipWithIndex.map {
        case (line, i) => s"[${i+1}] ${printHistoryLine(line)}"
      }).mkString(newline)
    }

    historyProgram("/some/history/file",
                   _ => DabbleHistoryLine(NonEmptyList(ScalaVersionSupplied("org.scalaz", "scalaz", "7.2.2"))),
                   drawMenu,
                   "please choose a numbered value or press 'q' to exit",
                   line => println(s"launching dabble with ${printHistoryLine(line)}"))
    .foldMap(new ConsoleInterpreter(historyLines))
  }

  private def withTestInterpreter(): Unit = {
    val buffer = ListBuffer[String]()
    val inter = new TestInterpreter(
        Map(("user.input", () => {
              val n = scala.util.Random.nextInt(10)
              if (n == 0) "q" else s"$n"
            }),
            "/some/history/file" -> Seq("line1")),
        buffer)
    historyProgram("/some/history/file",
      _ => DabbleHistoryLine(NonEmptyList(ScalaVersionSupplied("org.xala", "xala", "1.0.1"))),
      _ => s"~menu~${newline}[1] org.xala % xala_2.11 % 1.0.1",
      "choose an option",
      line => {
        buffer += s"launching dabble with: ${printHistoryLine(line)}"
       ()
      }).
    foldMap(inter)

    println(s"world: ${inter.world}")
    println(s"actions: ${inter.actions.mkString(newline)}")
  }

  withTestInterpreter()
}