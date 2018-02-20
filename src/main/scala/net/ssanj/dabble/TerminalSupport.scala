package net.ssanj.dabble

final case class HistoryCommand(term: Option[String])

final case class DabbleRunConfig(dependencies: Seq[String] = Seq.empty,
                                 resolvers: Seq[String] = Seq.empty,
                                 macroParadiseVersion: Option[String] = None,
                                 historyCommand: Option[HistoryCommand] = None) {
  def %(dep: String): DabbleRunConfig = this.copy(dependencies = dependencies :+ dep)
}

trait TerminalSupport {

  private def dependencies(op: scopt.OptionParser[DabbleRunConfig]): Unit = {
    op.arg[String]("<dep1> + <dep2> + ... <depn>").
      unbounded().
      optional().
      action { (dep, config) => config % dep }.
      text("""The list of dependencies to include.""" + newlineAndTab +
           """Multiple dependencies should be separated by a + sign.""" + newline + newlineAndTab +
           "Format is one of:" + newlineAndTab +
           """"org1" %  "name1" % "version1"""" + newlineAndTab +
           """"org2" %% "name2" % "version2"""" + newlineAndTab +
           """"org3" %% "name3" % "version3 % "config""""" + newlineAndTab +
           """"org1" %% "name1" % "version1" + "org2" %% "name2" % "version2"""" + newline + newlineAndTab +
           """Example:""" + newlineAndTab +
           """"com.github.scopt" %% "scopt" % "3.4.0" + "org.scalaz" %% "scalaz-core" % "7.2.2"""" + newline
          )
  }

  private def resolvers(op: scopt.OptionParser[DabbleRunConfig]): Unit = {
    op.opt[Seq[String]]('r', "resolvers").
      valueName(""""<res1>,<res2>, .... <resn>"""").
      action { (resolvers, config) => config.copy(resolvers = resolvers.map(_.trim)) }.
      text("""The list of additional repositories to resolve dependencies from.""" + newlineAndTab +
           """Multiple dependencies should be separated by commas.""" + newline + newlineAndTab +
           "Format is one of:" + newlineAndTab +
           """(sonatype|typesafe|typesafeIvy|sbtPlugin):[s|r]""" + newlineAndTab +
           """(maven2|jcenter)""" + newlineAndTab +
           """bintray(owner:repo)""" + newlineAndTab +
           """name@repo_url""" + newline + newlineAndTab +
           """sonatype:s -- loads only snapshot repo""" + newlineAndTab +
           """sonatype:r -- loads only release repo""" + newlineAndTab +
           """sonatype   -- loads both snapshot and release repos""" + newlineAndTab +
           """maven2     -- loads the maven2 resolver""" + newlineAndTab +
           """bintray:user:repo  -- loads the bintray resolver for user/repo""" + newlineAndTab +
           """your repo name @ https://your.repo.com/release/maven -- loads a custom resolver""" + newline + newlineAndTab +
           """Example:""" + newlineAndTab +
           """"bintray:oncue:releases, sonatype:r"""" + newline
     )
  }

  private def macroParadise(op: scopt.OptionParser[DabbleRunConfig]): Unit = {
    op.opt[String]("macro-paradise").
    abbr("mp").
    action { (version, config) => config.copy(macroParadiseVersion = Option(version)) }.
    valueName("""<version>""").
    text("""Includes the macro paradise compiler plugin with the supplied version.""" + newline + newlineAndTab +
         s"""Example:${newlineAndTab}""" +
         """2.1.0"""
   )
  }

  private def toggle(name: String, shortName: Option[String] = None)(f: DabbleRunConfig => Unit)(op: scopt.OptionParser[DabbleRunConfig]): Unit = {
    val opDef = op.opt[Unit](name)
    shortName.map(opDef.abbr).getOrElse(opDef) action { (_, c) =>
      f(c)
      op.terminate(Right(()))
      c
    }
  }

  private def historyCommand(op: scopt.OptionParser[DabbleRunConfig]): Unit = {
    op.cmd("history").
    abbr("hi").
    action { (_, c) => c.copy(historyCommand = Option(HistoryCommand(None))) }.
    children {
      op.opt[String]("term").
      abbr("t").
      valueName("""<search term>""").
      text("""The term to search through history for""").
      action { (term, config) =>
        if (term.trim.nonEmpty) config.copy(historyCommand = Option(HistoryCommand(Option(term))))
        else config
      }
    }.
    text("command history.")
  }

  lazy val parser = new scopt.OptionParser[DabbleRunConfig]("Dabble") {
    head(s"$title")
    toggle("help", Option("h"))(_ => showUsage)(this)
    toggle("version", Option("v"))(_ => println(s"$title"))(this)
    dependencies(this)
    resolvers(this)
    macroParadise(this)
    showUsageOnError
    note(s"${newline}Please see https://github.com/ssanj/dabble for more examples.")
    checkConfig{ c =>
      if (c.dependencies.length < 5 && c.historyCommand.isEmpty) {
        failure("Invalid format for dependencies. Please see accepted formats below.")
      } else success
    }
    historyCommand(this)
  }

  //We turn off all checks, and documentation as we don't need them if this fails
  lazy val historyParser = new scopt.OptionParser[DabbleRunConfig]("Dabble-History") {
    dependencies(this)
    resolvers(this)
    macroParadise(this)
  }
}

object TerminalSupport extends TerminalSupport

object Terminal extends App with TerminalSupport {
  parser.parse(args, DabbleRunConfig()) match {
    case Some(conf) => println(s"you got conf: $conf")
    case None => println("Could not parse arguments")
  }
}
