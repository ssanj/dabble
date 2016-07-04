package net.ssanj.dabble
package dsl

import org.scalacheck.Properties
import org.scalacheck.{Prop, Gen}

import scala.collection.mutable.{Map => MMap}

import DependencyCommands._
import DabblePathTypes._
import ScalaCheckSupport._

object DependencyCommandsProps extends Properties("DependencyCommands") {

  private case class OsName(name: String)

  private def genOsName: Gen[Option[OsName]] =
    Gen.option(
      Gen.oneOf("Windows 8.1",
                "Mac OS X",
                "Linux",
                "Mac OS",
                "HP-UX",
                "Windows 2000",
                "Some other Os").
        map(OsName)
    )

  private def genDabbleHomePath: Gen[DabbleHomePath] = for {
    n <- Gen.choose(1, 5)
    paths <- Gen.listOfN(n, Gen.alphaLowerChar)
  } yield DabbleHomePath(DirPath(paths.mkString("/")))

  private val world = MMap[String, Seq[String]]()

  property("should return sbt executable for a given OS") =
    Prop.forAll(genOsName) { osOp =>
      //doesn't add the os.name for the None case
      //which simulates an environment where os.name is not defined
      osOp.foreach { os =>
        world += ("os.name" -> Seq(os.name))
      }

      val executable = getSBTExec.foldMap(new SaveHistoryFileInterpreter(world))

      val windowsProp     = booleanProp("isWindows")(osOp.forall(_.name.toLowerCase.contains("windows")), true)
      val windowsExecProp = contentProp("sbt executable")(Seq(executable), Seq("sbt.bat"))
      val defaultExecProp = contentProp("sbt executable")(Seq(executable), Seq("sbt"))

      Prop.collect(osOp.map(_.name).getOrElse("*Not Defined*")) {
        (windowsProp && windowsExecProp) || defaultExecProp
      }
    }

 property("should run the sbt executable for a given OS") =
  Prop.forAll(genOsName, genDabbleHomePath) { (osOp, homePath) =>
    osOp.foreach { os =>
      world += ("os.name" -> Seq(os.name))
    }

    val execName = if (world.get("os.name").
                        flatMap(_.headOption).
                        filter(_.toLowerCase.startsWith("windows")).isDefined) "sbt.bat"
                   else "sbt"

    val result = executeSbt(homePath).foldMap(new SaveHistoryFileInterpreter(world))

    val isRightProp = booleanProp("isRight")(result.isRight, true)

    val callProcProp = {
      val Some(Seq(args, workingDir)) = world.get(execName)
      val argsProp = contentProp("arguments")(Seq(args), Seq("console-quick"))
      val workingDirProp = contentProp("workingDir")(
        Seq(homePath.work.path.dir), Seq(s"${homePath.path.dir}/work"))

      argsProp && workingDirProp
    }

    isRightProp && callProcProp
  }
}