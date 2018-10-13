package net.ssanj.dabble

import scala.util.Try

import ammonite.ops._

import scalaz._
import scalaz.Id.Id
import scalaz.syntax.std.`try`._

import DabbleDslDef._

class DabbleConsoleInterpreter extends (DabbleDsl ~> Id) {

  def apply[A](dsl: DabbleDsl[A]): Id[A] = dsl match {

    case ReadFile(filename: String) =>
        Try(read.lines(Path(filename), "UTF-8")).
          toDisjunction.
          leftMap(x => s"Failed to read $filename due to: ${x.getMessage}")

    case WriteFile(filename: String, lines: Seq[String]) =>
      Try(write.over(Path(filename), lines.mkString(newline))).
        toDisjunction.
        leftMap(x => s"Failed to write to $filename due to: ${x.getMessage}")

    case FileExists(filename: String) =>
      Try(exists(Path(filename))).toOption.fold(false)(identity)

    case Log(message: String) => println(message)

    case ReadInput(prompt: String) =>
      println(prompt)
      scala.io.StdIn.readLine

    case SystemProp(key: String) =>
      Try(System.getProperty(key)).
        toDisjunction.
        leftMap(x => s"Could not read system property: $key due to: ${x.getMessage}")

    //This could return an IoError of type CallError(errorCode, throwable)
    case CallProcess(filename: String, arguments: String, workingDir: String) =>
      Try(%(filename, arguments)(Path(workingDir))).
      toDisjunction.
      leftMap(x => s"Could not run dabble due to: ${x.getMessage}. See sbt log for details.")

    case Exit(er: ExecutionResult2) => System.exit(er.code.code)

    case  NoOp =>
  }
}
