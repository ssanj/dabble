package net.ssanj.dabble


trait DabblePathTypes {

  object DabblePathTypes {
    def dirPath(dir: String): DirPath = DirPath(dir)

    def filePath(dir: DirPath, file: String): FilePath = FilePath(dir, file)
  }

  case class DirPath(dir: String) {
    def /(subdir: DirPath): DirPath = DirPath(s"${dir}/${subdir.dir}")
    def /(filePath: String): FilePath  = FilePath(this, filePath)
  }

  case class FilePath(dir: DirPath, filename: String) {
    val file: String = s"${dir.dir}/$filename"
  }

  import DabblePathTypes._

  case class DabbleWorkPath(path: DirPath) {
    val defaultBuildFile: DabbleDefaultWorkBuildFile = DabbleDefaultWorkBuildFile(path/"build.sbt")
  }

  case class DabbleTemplatesPath(path: DirPath)
  case class DabbleHistoryFilePath(path: FilePath)
  case class DabbleDefaultBuildFile(path: FilePath)
  case class DabbleDefaultWorkBuildFile(path: FilePath)

  case class DabbleHomePath(path: DirPath) {
    val work      = DabbleWorkPath(path/dirPath("work"))
    val templates = DabbleTemplatesPath(path/dirPath("templates"))
    val history   = DabbleHistoryFilePath(path/"dabble.history")
    val defaultBuildFile = DabbleDefaultBuildFile(path/"build.sbt")
  }

  def dabbleHomePath(userHome: String): DabbleHomePath = DabbleHomePath(dirPath(userHome))
}

object DabblePathTypes extends DabblePathTypes
