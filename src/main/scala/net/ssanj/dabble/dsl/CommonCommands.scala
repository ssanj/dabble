package net.ssanj.dabble
package dsl

import scalaz._
import scalaz.syntax.either._

import DabbleDslDef._
import DabbleHistory._

object CommonCommands {

  /** Dsl for formatting content, typically with newlines without the need for accessing the
    * platform-specific newline character. The Dsl caters for Plain text characters, newlines and
    * composite content with newlines and/or content.
    *
    * @example {{{
    *  Plain("line1") + NL + Plain("line2") + NL + Plain("line3")
    * }}}
    */
  sealed trait FormattedContent {
    def +(other: FormattedContent): Composite = Composite(this, other)
  }

  object FormattedContent {
    def nl(n: Int): FormattedContent = {
      n match {
        case 2 => Composite(NL, NL)

        case x if x > 2 => {
          val Seq(one, two, t@_*) = List.fill(x)(NL)
          t.foldLeft(Composite(one, two))(_ + _)
        }

        case _ => NL
      }
    }
  }

  final case class Plain(value: String) extends FormattedContent
  case object NL extends FormattedContent
  final case class Composite(fc1: FormattedContent, fc2: FormattedContent) extends FormattedContent

    //TODO: Do we need this or can we leave this to the interpreter?
  def newlinesDS(n: Int): DabbleScript[String] =
    systemProp("line.separator") map {
      case -\/(error) => "\n"
      case \/-(nl) => List.fill(n)(nl).mkString
    }

  def readHistoryFile(historyFileName: String, argParser: CommandlineParser): DabbleScript[ErrorOr[HistoryLinesAndWarnings]] = for {
    linesE <- readFile(historyFileName)
    hfStatus = linesE match {
      case -\/(e) => e.left
      case \/-(lines) => (readHistoryWithWarnings(argParser)(lines)).right
    }

  } yield hfStatus

  def loadHistoryFile(historyFileName: String, argParser: CommandlineParser): DabbleScript[ErrorOr[HistoryLinesAndWarnings]] = for {
    hasHistoryFile <- fileExists(historyFileName)
    hlawE <- if (!hasHistoryFile) liftDS(\&/.That(Seq.empty).right[String])
             else readHistoryFile(historyFileName, argParser)
  } yield hlawE

  def getHistoryLines(hlaw: HistoryLinesAndWarnings): Seq[DabbleHistoryLine] = {
     hlaw.onlyThat.orElse(
      hlaw.onlyBoth.map(_._2)
     ).getOrElse(Seq.empty[DabbleHistoryLine])
  }

  def getWarnings(hlaw: HistoryLinesAndWarnings): Seq[String] = {
    hlaw.fold(identity, _ => Seq.empty[String], (l, _) => l)
  }
}

