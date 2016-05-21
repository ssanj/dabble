package net.ssanj.dabble

import scalaz.syntax.either._

object DabbleApp extends DependencyParser  with
                         ResolverParser    with
                         DabblePrinter     with
                         DefaultTemplate   with
                         Executor          with
                         DabblePaths       with
                         DabbleHistory     with
                         Banner            with
                         TerminalSupport {


  def main(args: Array[String]) {
    parser.parse(args, DabbleRunConfig()) match {
      case Some(DabbleRunConfig(deps, res, mp)) =>
        getBanner.foreach(println)
        //read history file here
        val result =
              (for {
                d <- parseDependencies(deps)
                r <- (if (res.nonEmpty) parseResolvers(res)
                      else Seq.empty.right[String])
                //TODO: Create a DabbleConfig from DabbleRunConfig which has valid types.
              } yield (d, r, mp)).fold(processingFailed, (build _).tupled)

        //if successful write history file here
        exit(result)
      case None =>
        exit(processingFailed)
    }
  }
}
