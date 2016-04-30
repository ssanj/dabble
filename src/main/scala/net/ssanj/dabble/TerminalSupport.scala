package net.ssanj.dabble

trait TerminalSupport {

  case class DabbleRunConfig(dependencies: Seq[String] = Seq.empty) {
    def %(dep: String) = this.copy(dependencies = dependencies :+ dep)
  }

  private def dependencies(op: scopt.OptionParser[DabbleRunConfig]): Unit = {
    op.arg[String]("<dep1> + <dep2> + ... <depn>").
      minOccurs(5).
      unbounded().
      action { (dep, config) => config % dep }.
      text("Format:" + newline + tab +
           """"org1" %  "name1" % "version1"""" + newline + tab +
           """"org2" %% "name2" % "version2"""" + newline + tab +
           """"org1" %% "name1" % "version1" + "org2" %% "name2" % "version2"""""
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
    head("dabble", DabbleInfo.version)
    help("help") abbr("h")
    toggle("version", Option("v"))(_ => println(s"dabble version ${DabbleInfo.version}"))(this)
    dependencies(this)
  }
}

object Terminal extends App with TerminalSupport {
  parser.parse(args, DabbleRunConfig()) match {
    case Some(conf) => println(s"you got conf: $conf")
    case None => println("Could not parse arguments")
  }
}