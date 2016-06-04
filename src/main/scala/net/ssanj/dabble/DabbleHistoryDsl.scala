package net.ssanj.dabble

import scala.util.Try

import scalaz._
import scalaz.syntax.either._
import scalaz.syntax.bind._
import scalaz.Free._

import ExecutionResult2._
import DabbleHistory._

//Free only encapsulates side effects. Not logic. Logic is performed in the interpreter.
//Free Scipts have to return a Free[DabbleHistory, ?]

//TODO: Figure out how to modularise this class. It's too long. We need to combine
//classes to get this much information, alternatively import them.
object DabbleHistoryDslDef {

  //1. Dsl
  final case class OneBased private (value: Int) {
    val toZeroBased: Int = Math.max(0, value - 1)
    def includes(n: Int): Boolean = n >= OneBased.MIN && n <= value
  }

  object OneBased {
    val MIN = 1

    def fromZeroBased(value: Int): OneBased = fromOneBased(value + 1)
    def fromOneBased(value: Int): OneBased = OneBased(Math.max(MIN, value))
  }

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

  sealed trait HistoryOption
  final case object QuitHistory extends HistoryOption
  final case class HistorySelection(line: DabbleHistoryLine) extends HistoryOption

  //3. Compose functions
  def getUserChoice(prompt: String,
                    hLines: Seq[DabbleHistoryLine]): DabbleScript[HistoryOption] = {

    readInput(prompt).
      flatMap {
            case "q" => liftDS[HistoryOption](QuitHistory)
            case in =>
              import OneBased._

              val maxLines = fromZeroBased(hLines.length)

              Try(in.toInt).
                filter(maxLines.includes).
                map(line => liftDS[HistoryOption](HistorySelection(hLines(fromOneBased(line).toZeroBased)))).
                getOrElse(getUserChoice(prompt, hLines))
      }
  }

  //TODO: We need the HLAW (history lines read)
  //TODO: This should actually return a ExecutionResult2. If it succeeds then we can save history.
  def launchDabble(line: DabbleHistoryLine): DabbleScript[ExecutionResult2] = ???

  def noHistory: DabbleScript[HistoryOption] = for {
    _ <- log("You have not made history.")
    _ <- log("Dabble writes out a history line when you successfully load a dependency and exit.")
  } yield QuitHistory

  def chooseHistory(searchTerm: Option[String],
                    prompt: String,
                    hlaw: HistoryLinesAndWarnings,
                    hMenu: HistoryMenu): DabbleScript[HistoryOption] = {

     val hLines = hlaw.onlyThat.getOrElse(Seq.empty)
     for {
       ho <- if (hLines.isEmpty) noHistory
             else {
               searchTerm match {
                 case Some(term) =>
                   val matchedLines = findBySearchTerm(hLines, term)
                   if (matchedLines.isEmpty) showAlternateOptions(prompt, hMenu, hLines)(term)
                   else showHistoryMenuAndPromptUser(prompt, hMenu, matchedLines)

                 case None => showHistoryMenuAndPromptUser(prompt, hMenu, hLines)
               }
             }
     } yield ho
  }

  def showAlternateOptions(prompt: String,
                           hMenu: HistoryMenu,
                           hLines: Seq[DabbleHistoryLine])(term: String): DabbleScript[HistoryOption] =
    log(s"Could not find matches for: $term.") >>
      promptUserToShowFullHistoryOrQuit(prompt, hMenu, hLines)

  def findBySearchTerm(hLines: Seq[DabbleHistoryLine], term: String): Seq[DabbleHistoryLine] = {
    hLines.filter {
        case DabbleHistoryLine(deps, _, _) =>
          !deps.list.filter {
            case ScalaVersionSupplied(org, name, _, _) =>
              org.contains(term) || name.contains(term)
            case ScalaVersionDerived(org, name, _, _) =>
              org.contains(term) || name.contains(term)
          }.isEmpty
    }
  }

  //TODO: Do we need this or can we leave this to the interpreter?
  def newlines(n: Int): DabbleScript[String] =
    systemProp("line.separator") map {
      case -\/(error) => "\n"
      case \/-(nl) => List.fill(n)(nl).mkString
    }

  def promptUserToShowFullHistoryOrQuit(fullHistoryPrompt: String,
                                        hMenu: HistoryMenu,
                                        fullHistory: Seq[DabbleHistoryLine]): DabbleScript[HistoryOption] = for {
    input <- readInput(s"Select 'f' for full history or 'q' to quit.")
    ho <- input match {
      case "f" => showHistoryMenuAndPromptUser(fullHistoryPrompt, hMenu, fullHistory)
      case "q" => liftDS(QuitHistory)
      case _ => promptUserToShowFullHistoryOrQuit(fullHistoryPrompt, hMenu, fullHistory)
    }
  } yield ho

  def showHistoryMenuAndPromptUser(prompt: String,
                                   hMenu: HistoryMenu,
                                   hLines: Seq[DabbleHistoryLine]): DabbleScript[HistoryOption] = {
    log(hMenu(hLines)) >> getUserChoice(prompt, hLines)
  }

  def readHistoryFile(historyFileName: String, argParser: CommandlineParser): DabbleScript[ErrorOr[HistoryLinesAndWarnings]] = for {
    linesE <- readFile(historyFileName)
    hfStatus = linesE match {
      case -\/(e) => e.left
      case \/-(lines) => (readHistoryWithWarnings(argParser)(lines)).right
    }

  } yield hfStatus

  def exitWithError(message: String): DabbleScript[ExecutionResult2] = {
    liftDS(ExecutionResult2(Option(message), UnsuccessfulAction))
  }

  def saveHistoryFile(filename: String, selection: DabbleHistoryLine, hLines: Seq[DabbleHistoryLine],
    historyPrinter: DabbleHistoryLine => String):
    DabbleScript[ErrorOr[Unit]] = {
      import scala.collection.mutable.LinkedHashSet
      val uniqueHLines = LinkedHashSet() ++ (selection +: hLines)
      writeFile(filename, uniqueHLines.map(historyPrinter).toSeq)
    }


  def dsBind[A, B](ds: DabbleScript[A], f: A => DabbleScript[B]): DabbleScript[B] = ds.flatMap(f)

  def erBind(er1: DabbleScript[ExecutionResult2], f: ExecutionResult2 => DabbleScript[ExecutionResult2]) =
    dsBind[ExecutionResult2, ExecutionResult2](er1, {
        case er@ExecutionResult2(_, SuccessfulAction) => f(er)
        case er => er1
      })

 /** Combines a [[DabbleScript]] that contains an [[ExecutionResult2]] with another that contains an
   * [[ErrorOr[A]]].
   * The success combination rules are as follows:
   * 1. If dser is a [[SuccessfulAction]] then run dseo.
   * 1. If dseo returns an error then create an [[ExecutionResult2]] with the error and an [[UnsuccessfulAction]].
   * 1. If dseo return a success then return the original [[ExecutionResult2]].
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

 def launchDabbleAndSaveHistory(historyFileName: String,
                                line: DabbleHistoryLine,
                                hlaw: HistoryLinesAndWarnings,
                                historyPrinter: DabbleHistoryLine => String):DabbleScript[ExecutionResult2] = {
    combineEV(launchDabble(line),
              saveHistoryFile(historyFileName,
                              line,
                              hlaw.onlyThat.getOrElse(Seq.empty),
                              historyPrinter))
 }

  // //4. Program
  def historyProgram(searchTerm: Option[String],
                     historyFileName: String,
                     argParser: CommandlineParser,
                     hMenu: HistoryMenu,
                     prompt: String,
                     historyPrinter: DabbleHistoryLine => String): DabbleScript[Unit] = for {
    //TODO: Simplify readHistoryFile to return Seq.empty if the history file is not found.
    //TODO: readHistoryFile is going to be common. Pull it out.
    //TODO: Do we need to distinguish between an empty history file and an absent one?
    hasHistoryFile <- fileExists(historyFileName)
    er2 <- if (!hasHistoryFile) noHistory.map(_ => withResult(SuccessfulAction))
           else readHistoryFile(historyFileName, argParser).map {
              case -\/(error) => exitWithError(s"could not read history file: $historyFileName due to: $error")
              case \/-(hlaw) =>
                chooseHistory(searchTerm, prompt, hlaw, hMenu).map {
                  case QuitHistory => withResult(SuccessfulAction)
                  case HistorySelection(line) =>
                    launchDabbleAndSaveHistory(historyFileName, line, hlaw, historyPrinter)
                }
           }
    _ <- er2 match {
          case ExecutionResult2(_, SuccessfulAction) => noOp
          case ExecutionResult2(errors, UnsuccessfulAction) =>
            errors.map(e => log(s"Dabble exited with the following errors: $e")).
                   getOrElse(log("Dabble exited with errors."))
        }
  } yield ()
}