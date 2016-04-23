package net.ssanj.dabble

import ammonite.ops._

object DabbleApp {

  val templateSBTFile = """name := "Dabble"

organization := "biz.awesome"

version := "0.0.1"

scalaVersion := "2.11.7""""

  def main(args: Array[String]) {
    val dabbleHome = Path(System.getProperty("user.home"))/".dabble"
    val dabbleWork = dabbleHome/'work
    val newLine = System.getProperty("line.separator")
    val parsedArgs = args.map(a => if (!a.startsWith("%")) s""""$a"""" else a)
    write.over (dabbleWork/"build.sbt", templateSBTFile + newLine + newLine + "libraryDependencies += " + parsedArgs.mkString(" "))
    val result = %('sbt, "console")(dabbleWork)
    println(s"exit result: $result")
  }
}

