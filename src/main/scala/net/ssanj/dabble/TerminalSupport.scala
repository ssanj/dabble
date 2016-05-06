package net.ssanj.dabble

trait TerminalSupport {

  case class DabbleRunConfig(dependencies: Seq[String] = Seq.empty, resolvers: Seq[String] = Seq.empty) {
    def %(dep: String) = this.copy(dependencies = dependencies :+ dep)
  }

  private def dependencies(op: scopt.OptionParser[DabbleRunConfig]): Unit = {
    op.arg[String]("<dep1> + <dep2> + ... <depn>").
      // minOccurs(5).
      unbounded().
      action { (dep, config) => config % dep }.
      text(s"Format is one of:" + newlineAndTab +
           """"org1" %  "name1" % "version1"""" + newlineAndTab +
           """"org2" %% "name2" % "version2"""" + newlineAndTab +
           """"org3" %% "name3" % "version3 % "config""""" + newlineAndTab +
           """"org1" %% "name1" % "version1" + "org2" %% "name2" % "version2"""" + newline
          )
  }

  private def resolvers(op: scopt.OptionParser[DabbleRunConfig]): Unit = {
    op.opt[Seq[String]]('r', "resolvers").
      valueName(""""<res1>,<res2>, .... <resn>"""").
      action { (resolvers, config) => config.copy(resolvers = resolvers.map(_.trim)) }.
      text("Format is one of:" + newlineAndTab +
           """(sonatype|typesafe|typesafeIvy|sbtPlugin):[s|r]""" + newlineAndTab +
           """(maven2|jcenter)""" + newlineAndTab +
           """bintray(owner:repo)""" + newlineAndTab +
           """name@repo_url""" + newline + newlineAndTab +
           """Example:""" + newlineAndTab +
           """sonatype:s -- loads only snapshot repo""" + newlineAndTab +
           """sonatype:r -- loads only release repo""" + newlineAndTab +
           """sonatype   -- loads both snapshot and release repos""" + newlineAndTab +
           """maven2     -- loads the maven2 resolver""" + newlineAndTab +
           """bintray:user:repo  -- loads the bintray resolver for user/repo""" + newlineAndTab +
           """your repo name @ https://your.repo.com/release/maven -- loads a custom resolver""" + newline
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

  val parser = new scopt.OptionParser[DabbleRunConfig]("Dabble") {
    head(s"$title")
    toggle("help", Option("h"))(_ => showUsage)(this)
    toggle("version", Option("v"))(_ => println(s"$title"))(this)
    dependencies(this)
    resolvers(this)
    showUsageOnError
    checkConfig{ c =>
      if (c.dependencies.length < 5)
        failure("Invalid format for dependencies. Please see accepted formats below.")
      else success
    }
  }
}

object Terminal extends App with TerminalSupport {
  parser.parse(args, DabbleRunConfig()) match {
    case Some(conf) => println(s"you got conf: $conf")
    case None => println("Could not parse arguments")
  }
}