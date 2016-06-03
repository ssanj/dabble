package net.ssanj.dabble

sealed trait ExitCode { val code: Int }
case object SuccessfulAction extends ExitCode { val code = 0}
case object UnsuccessfulAction extends ExitCode { val code = -1 }

//TODO: Replace with ER2
//TODO: Make this a list of messages
case class ExecutionResult(message: Option[String], code: Int)

case class ExecutionResult2(message: Option[String], code: ExitCode)

object ExecutionResult2 {
  def withResult(ec: ExitCode): ExecutionResult2 = ExecutionResult2(None, ec)
}