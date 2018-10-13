name := "Dabble"

organization := "net.ssanj"

version := "0.3.0"

scalaVersion := "2.12.7"


libraryDependencies ++= Seq(
  "org.scalaz"       %% "scalaz-core"  % "7.2.26",
  "com.lihaoyi"      %% "ammonite-ops" % "1.2.1",
  "com.github.scopt" %% "scopt"        % "3.7.0",
  "org.scalatest"    %% "scalatest"    % "3.0.5"  % "test",
  "org.scalacheck"   %% "scalacheck"   % "1.14.0" % "test"
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

addCommandAlias("precheckin", ";clean;scalastyle;scapegoat;test")