package net.ssanj.dabble

import scala.io.Source
import scala.util.Try

trait Banner {

  def getBanner: Option[String] = Try {
    Source.fromInputStream(this.getClass.getResourceAsStream("/banner.txt"), "UTF-8").getLines.toList.mkString(newline)
  }.toOption
}

object Banner extends Banner
