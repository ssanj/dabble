package net.ssanj.dabble

import scala.language.implicitConversions

import scalaz._
import scalaz.Free._
import scalaz.syntax.validation._

//Free only encapsulates side effects. Not logic. Logic is performed in the interpreter.
//Free Scipts have to return a Free[DabbleHistory, ?]

//TODO: Figure out how to modularise this class. It's too long. We need to combine
//classes to get this much information, alternatively import them.
object DabbleDslDef {

  type ErrorOr[A] = String \/ A

  // final case class ErrorOrAOps[A](value: ErrorOr[A]) {
  //   def toDabbleResult: DabbleResult[A] = value.fold(l =>  l.failureNel[SuccessResult[String, A]],
  //                                                    r => SuccessResult(Seq.empty, r).successNel[String])
  // }

  // final case class SuccessResult[W, S](warnings: Seq[W], success: S)

  // object ErrorOr {
  //   implicit def toErrorOrOpsFromErrorOr[A](value: ErrorOr[A]): ErrorOrAOps[A] = ErrorOrAOps[A](value)
  // }

  // type FailureNelOrSuccessResult[F, W, S] = ValidationNel[F, SuccessResult[W, S]]

  // type DabbleResult[A] = FailureNelOrSuccessResult[String, String, A]


  sealed trait DabbleDsl[A]
  final case class ReadFile(filename: String) extends DabbleDsl[ErrorOr[Seq[String]]]
  final case class WriteFile(filename: String, lines: Seq[String]) extends DabbleDsl[ErrorOr[Unit]]
  final case class FileExists(filename: String) extends DabbleDsl[Boolean]

  final case class Log(message: String) extends DabbleDsl[Unit]
  // final case class LogInfo(message: String) extends DabbleDsl[Unit]
  // final case class LogWarning(message: String) extends DabbleDsl[Unit]
  // final case class LogError(message: String, errorOp: Option[Throwable]) extends DabbleDsl[Unit]

  final case class ReadInput(prompt: String) extends DabbleDsl[String]
  final case class SystemProp(key: String) extends DabbleDsl[ErrorOr[String]]
  final case class CallProcess(filename: String, arguments: String, workingDir: String) extends DabbleDsl[ErrorOr[Unit]]
  final case class Exit(er: ExecutionResult2) extends DabbleDsl[Unit]
  final case object NoOp extends DabbleDsl[Unit]

  type DabbleScript[A] = Free[DabbleDsl, A]

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
    DabbleScript[ErrorOr[Unit]] = liftF(CallProcess(filename, arguments, workingDir))

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