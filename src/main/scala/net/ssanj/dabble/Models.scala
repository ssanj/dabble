package net.ssanj.dabble

import scalaz._
import scala.language.implicitConversions

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