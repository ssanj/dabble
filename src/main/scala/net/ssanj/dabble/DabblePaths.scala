package net.ssanj.dabble

import ammonite.ops.Path

trait DabblePaths {
  val userHomePath = Path(userHome)

  case class DabbleWork(path: Path)

  case class DabbleTemplates(path: Path)

  case class DabbleHome(path: Path) {
    def work      = DabbleWork(path/'work)
    def templates = DabbleTemplates(path/'templates)
    def history = path/"dabble.history"
  }

  val dabbleHome = DabbleHome(userHomePath/".dabble")

  val dabbleWork = dabbleHome.work
}