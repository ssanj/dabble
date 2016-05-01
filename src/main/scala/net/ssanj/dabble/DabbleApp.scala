package net.ssanj.dabble

object DabbleApp extends DependencyParser  with
                         DependencyPrinter with
                         DefaultTemplate   with
                         Executor          with
                         TerminalSupport {


  def main(args: Array[String]) {
    parser.parse(args, DabbleRunConfig()) match {
      case Some(DabbleRunConfig(deps)) =>
        val result = parse(deps)fold(processingFailed, build)
        exit(result)
      case None => exit(processingFailed("Could not parse dependencies. Please run with --help for usage."))
    }
  }
}
