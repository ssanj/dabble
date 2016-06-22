package net.ssanj.dabble
package dsl

import scala.collection.mutable.{Map => MMap}

import org.scalatest.{BeforeAndAfterEach, FlatSpec, Matchers}

import DependencyCommands._
import DefaultTemplate.inMemSbtTemplate

final class DependencyCommandSpec extends FlatSpec with Matchers with BeforeAndAfterEach {

  var world: MMap[String, Seq[String]] = _
  var interpreter: SaveHistoryFileInterpreter = _

  override def beforeEach(): Unit = {
    world = MMap[String, Seq[String]]()
    interpreter = new SaveHistoryFileInterpreter(world)
  }

  "logDabbleVersion" should "log dabble version and build" in {
    logDabbleVersion.foldMap(interpreter)

    val expectedVersion = s"${DabbleInfo.version}-b${DabbleInfo.buildInfoBuildNumber}"
    val Some(logs) = world.get("log")
    logs should have size 1
    logs.head should be (expectedVersion)
  }

  "useInMemoryTemplate" should "return formatted memory template" in {
    val template = useInMemoryTemplate.foldMap(interpreter)

    template should be (inMemSbtTemplate(newlines(2)))

    val Some(logs) = world.get("log")
    logs should have size 1
    val expectedLog = "Using in-memory sbt template. Create a build.sbt file in ~/.dabble/ to override."
    logs.head should be (expectedLog)
  }

  //TODO" These two can be written as props.
  "readSbtTemplateOrDefault" should "read sbt template" in {
    val sbtTemplate = "/some/path/build.sbt"
    val templateContent = Seq("line1", "line2", "line3")
    world += (sbtTemplate -> templateContent)


    val content = readSbtTemplateOrDefault(sbtTemplate).foldMap(interpreter)

    val expectedContent = templateContent.mkString(newline)
    content should be (expectedContent)

    val expectedLog = "Using default sbt template at: /some/path/build.sbt"
    world.get("log").get.head should be (expectedLog)
  }

  it should "fallback to in memory template" in {
    val sbtTemplate = "/some/path/build.sbt"

    val content = readSbtTemplateOrDefault(sbtTemplate).foldMap(interpreter)

    val expectedContent = inMemSbtTemplate(newlines(2))
    content should be (expectedContent)

    val expectedTemplate = "Using default sbt template at: /some/path/build.sbt"
    val expectedError    = "could not load template file due to: could not read filename: /some/path/build.sbt"
    val expectedFallback = "Using in-memory sbt template. Create a build.sbt file in ~/.dabble/ to override."

    val Some(log) = world.get("log")
    log should have size 3

    log(0) should be (expectedTemplate)
    log(1) should be (expectedError)
    log(2) should be (expectedFallback)
  }
}
