package net.ssanj.dabble

object DabbleApp extends DependencyParser  with
                         DependencyPrinter with
                         DefaultTemplate   with
                         Executor          with
                         Banner            with
                         TerminalSupport {


  def main(args: Array[String]) {
    parser.parse(args, DabbleRunConfig()) match {
      case Some(DabbleRunConfig(deps)) =>
        getBanner.foreach(println)
        val result = parse(deps)fold(processingFailed, build)
        exit(result)
      case None =>
        exit(processingFailed)
    }
  }
}
