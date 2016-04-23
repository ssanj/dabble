name := "Dabble"

organization := "net.ssanj"

version := "0.0.1"

scalaVersion := "2.11.7"

libraryDependencies ++= Seq(
  "org.scalaz"     %% "scalaz-core"  % "7.1.4",
  "com.lihaoyi"    %% "ammonite-ops" % "0.5.7",
  "org.scalatest"  %% "scalatest"    % "2.2.4"  % "test",
  "org.scalacheck" %% "scalacheck"   % "1.12.5" % "test"
)

scalacOptions ++= Seq(
                      "-unchecked",
                      "-deprecation",
                      "-feature",
                      "-Xfatal-warnings",
                      "-Xlint:_",
                      "-Ywarn-dead-code",
                      "-Ywarn-inaccessible",
                      // "-Ywarn-unused-import",
                      "-Ywarn-infer-any",
                      "-Ywarn-nullary-override",
                      "-Ywarn-nullary-unit"
                     )

assemblyJarName in assembly := "dabble.jar"


