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
  }
}

object DabbleHistoryRunner extends App with DabblePrinter with DependencyParser {
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