//handles the Exception cases as well as the null case.
def isPlainJar() = scala.util.Try(System.getProperty("plain.jar")).toOption.flatMap(Option(_)).isDefined

assemblyJarName in assembly := {
  val nameLower = name.value.toLowerCase
  if (isPlainJar) s"${nameLower}.jar" else s"${nameLower}"
}

mainClass in assembly := Some("net.ssanj.dabble.DabbleDslRunner")

import sbtassembly.AssemblyPlugin.defaultShellScript

assemblyOption in assembly := {
  val ao = (assemblyOption in assembly).value
  if (isPlainJar) ao.copy(prependShellScript = None)
  else ao.copy(prependShellScript = Some(defaultShellScript))
}

