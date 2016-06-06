package net.ssanj.dabble

import scalaz.{\/, \/-, -\/, Free}
import scalaz.Free._

//Free only encapsulates side effects. Not logic. Logic is performed in the interpreter.
//Free Scipts have to return a Free[DabbleHistory, ?]

//TODO: Figure out how to modularise this class. It's too long. We need to combine
//classes to get this much information, alternatively import them.
object DabbleHistoryDslDef {

  type ErrorOr[A] = String \/ A

  sealed trait DabbleHistoryDsl[A]
  final case class ReadFile(filename: String) extends DabbleHistoryDsl[ErrorOr[Seq[String]]]
  final case class WriteFile(filename: String, lines: Seq[String]) extends DabbleHistoryDsl[ErrorOr[Unit]]
  final case class FileExists(filename: String) extends DabbleHistoryDsl[Boolean]

  final case class Log(message: String) extends DabbleHistoryDsl[Unit]
  // final case class LogInfo(message: String) extends DabbleHistoryDsl[Unit]
  // final case class LogWarning(message: String) extends DabbleHistoryDsl[Unit]
  // final case class LogError(message: String, errorOp: Option[Throwable]) extends DabbleHistoryDsl[Unit]

  final case class ReadInput(prompt: String) extends DabbleHistoryDsl[String]
  final case class SystemProp(key: String) extends DabbleHistoryDsl[ErrorOr[String]]
  final case class CallProcess(filename: String, arguments: String, workingDir: String) extends DabbleHistoryDsl[ErrorOr[ExecutionResult2]]
  final case class Exit(er: ExecutionResult2) extends DabbleHistoryDsl[Unit]
  final case object NoOp extends DabbleHistoryDsl[Unit]

  type DabbleScript[A] = Free[DabbleHistoryDsl, A]

  //2. Lift functions
  def readFile(filename: String): DabbleScript[ErrorOr[Seq[String]]] =
    liftF(ReadFile(filename))

  def writeFile(filename: String, lines: Seq[String]): DabbleScript[ErrorOr[Unit]] =
    liftF(WriteFile(filename, lines))

  def fileExists(filename: String): DabbleScript[Boolean] =
    liftF(FileExists(filename))

  def log(message: String): DabbleScript[Unit] = liftF(Log(message))

  def readInput(prompt: String): DabbleScript[String] = liftF(ReadInput(prompt))

  //TODO: Do we need this or can we leave this to the interpreter?
  def systemProp(key: String): DabbleScript[ErrorOr[String]] = liftF(SystemProp(key))

  def callProcess(filename: String, arguments: String, workingDir: String):
    DabbleScript[ErrorOr[ExecutionResult2]] = liftF(CallProcess(filename, arguments, workingDir))

  def exit(er: ExecutionResult2): DabbleScript[Unit] = liftF(Exit(er))

  val noOp: DabbleScript[Unit] = liftF(NoOp)

  def liftDS[A](value: A): DabbleScript[A] = noOp.map(_ => value)

  type HistoryMenu = Seq[DabbleHistoryLine] => String

  def dsBind[A, B](ds: DabbleScript[A], f: A => DabbleScript[B]): DabbleScript[B] = ds.flatMap(f)

  def erBind(er1: DabbleScript[ExecutionResult2], f: ExecutionResult2 => DabbleScript[ExecutionResult2]) =
    dsBind[ExecutionResult2, ExecutionResult2](er1, {
        case er@ExecutionResult2(_, SuccessfulAction) => f(er)
        case er => er1
      })

 /** Combines a [[DabbleScript]] that contains an [[ExecutionResult2]] with another that contains an
   * [[ErrorOr[A]]] to finally return an [[ExecutionResult2]].
   *
   * {{{
   * DabbleScript[ExecutionResult2] =>
   *   DabbleScript[ErrorOr[A]] =>
   *     DabbleScript[ExecutionResult2]
   * }}}
   * The success combination rules are as follows:
   * 1. If dser is a [[SuccessfulAction]] then run dseo.
   * 1. If dseo returns an error then create an [[ExecutionResult2]] with the error and an [[UnsuccessfulAction]].
   * 1. If dseo return a success then return the original [[ExecutionResult2]].
   * 1. If dser is a [[UnsuccessfulAction]] then don't return [[ExecutionResult2]] and don't run dseo.
   *
   * The failure combination rules are as follows:
   * 1. If dser results in an [[UnsuccessfulAction]] then return the unsuccessful [[ExecutionResult2]].
   */
 def combineEV[A](dser: DabbleScript[ExecutionResult2], dseo: => DabbleScript[ErrorOr[A]]):
  DabbleScript[ExecutionResult2] = for {
   er1 <- dser
   er2 <- er1 match {
    case er@ExecutionResult2(_, SuccessfulAction) => dseo map {
      case -\/(error) => er1.copy(message = Option(error))
      case \/-(_) => er1
    }
    case _ => dser
   }
 } yield er2
}