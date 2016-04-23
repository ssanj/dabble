# Dabble #

Ever wanted to play around with a new Scala library but wished there was an easier way to put it on the classpath of the Scala repl? Tired of manually copying lengthy file paths to specify the classpath?

Dabble's got your back. Simply run dabble with the libraries of your choosing, similar to how you would see it on the project page or on mvncentral. Then sit back while dabble downloads any dependencies you need, adds it to the classpath of a Scala repl and launches you in.

Goals

1. Easy copy-paste of sbt dependencies onto dabble to get something working quickly.
  1. This implies sbt format: "org" %% "name" % "ver.si.on"
2. use SBT to do the heavy lifting.
3. store temp files in ~/.dabble/work, or specify working dir
   dabble -w|--working-dir /path/to/working/dir
4. Ability to specify a template.sbt file. Default one in ~/.dabble/template.sbt
   dabble -t|--template /path/to/template
5. Ability to specify information about dabble -> working dir, template in use etc.
   Eg: dabble -i|--info
   working directory: /some/path
   template: some template
   config: /path/to/config
   named dependencies:
    [name1] values1
    [name2] values2
6. Create a named section within the config for dependencies.
   Eg: dabble -n|--named scalaz (looks up scalaz section in config and loads dependencies from there)

   In config file:
   [scalaz-7.1.2]
   org.scalaz %% scalaz-core % 7.1.2

   [scalaz-all-]
   org.scalaz %% scalaz-core % 7.20
   org.scalaz %% scalaz-concurrent % 7.20
   org.scalaz %% scalaz-effect % 7.20

   It would be cool to replace the version of a name section with what was supplied.

   Eg. dabble -n scalaz 7.1.4 (replaces stored version of 7.1.2 -> 7.1.4)

# How show we accept multiple imports?

## space-separated
```dabble "io.argonaut" %% "argonaut" % "6.1" "org.scalaz" %% "scalaz-core" % "7.1.2"```

## double-dash-separated
```dabble "io.argonaut %% argonaut % 6.1 -- org.scalaz %% scalaz-core % 7.1.2"```

## plus-separated
```dabble "io.argonaut" %% "argonaut" % "6.1" + "org.scalaz" %% "scalaz-core" % "7.1.2"```


### Scratch

sealed trait Dependency
final case class ScalaSpecified(org: String, name: String, version: String)
final case class ScalaDerived(org: String, name: String, version: String)
)

case Array() => return collected dependencies
case (org, %%, name %, version, t@_*) =>
    //collect dependency
    //recurse with t
case (org, %,  name %, version, t@_*) =>
case ("+", org, %%, name %, version, t@_*) =>
case xs => //error


