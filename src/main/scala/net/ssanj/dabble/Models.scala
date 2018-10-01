package net.ssanj.dabble

import scalaz._

sealed trait ExitCode { val code: Int }
case object SuccessfulAction extends ExitCode { val code = 0}
case object UnsuccessfulAction extends ExitCode { val code = -1 }

//TODO: Replace with ER2
//TODO: Make this a list of messages
case class ExecutionResult(message: Option[String], code: Int)

//type X[F, W, S] ValidationNel[F, SuccessResult[W, S]]
//type ExecutionResult[A] = X[String, String, A]
//String \/ A => X[String, SuccessResult[W, A]]
case class ExecutionResult2(message: Option[String], code: ExitCode)

object ExecutionResult2 {
  def withResult(ec: ExitCode): ExecutionResult2 = ExecutionResult2(None, ec)
}

sealed trait DabbleResult extends Product with Serializable
final case class DabbleSuccess(warnings: Seq[String]) extends DabbleResult
final case class DabbleFailure(failures: NonEmptyList[String]) extends DabbleResult

//We could encode the errors in the type system instead of using Strings.
//sealed trait DabbleError
//sealed trait IoErrorType
//final case class FileNotFoundError(filename: String, message: String) extends IoErrorType
//final case class ReadError(filename: String, message: String, e: Option[Throwable]) extends IoErrorType
//final case class WriteError(filename: String, content: Seq[String], message: String, e: Option[Throwable]) extends IoErrorType
//final case class CallError(executable: String, arguments: String, message: String, e: Option[Throwable]) extends IoErrorType
//final case class IoError(error: IoErrorType) extends DabbleError
object DabbleResult {
  def dabbleSuccess(warnings: Seq[String]): DabbleResult = DabbleSuccess(warnings)
  val dabbleSuccess: DabbleResult = DabbleSuccess(Seq.empty)

  def dabbleFailure(failure: String, others: String *): DabbleResult =
    DabbleFailure(NonEmptyList(failure, others:_*))
}
