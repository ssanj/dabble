package net.ssanj.dabble

import scalaz.syntax.either._

object DabbleApp extends DependencyParser  with
                         ResolverParser    with
                         DabblePrinter     with
                         DefaultTemplate   with
                         Executor          with
                         Banner            with
                         TerminalSupport {


  def main(args: Array[String]) {
    parser.parse(args, DabbleRunConfig()) match {
      case Some(DabbleRunConfig(deps, res)) =>
        getBanner.foreach(println)

        val result =
              (for {
                d <- parseDependencies(deps)
                r <- (if (res.nonEmpty) parseResolvers(res)
                      else Seq.empty.right[String])
              } yield (d, r)).fold(processingFailed, (build _).tupled)

        exit(result)
      case None =>
        exit(processingFailed)
    }
  }
}
