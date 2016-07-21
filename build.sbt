name := "Dabble"

organization := "net.ssanj"

version := "0.2.0"

scalaVersion := "2.11.8"

libraryDependencies ++= Seq(
  "org.scalaz"       %% "scalaz-core"  % "7.2.2",
  "com.lihaoyi"      %% "ammonite-ops" % "0.5.7",
  "com.github.scopt" %% "scopt"        % "3.4.0",
  "org.scalatest"    %% "scalatest"    % "2.2.4"  % "test",
  "org.scalacheck"   %% "scalacheck"   % "1.12.5" % "test"
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
                      "-Ywarn-nullary-unit",
                      "-Ypatmat-exhaust-depth",
                      "40"
                     )