[![Build Status](https://travis-ci.org/ssanj/dabble.svg?branch=master)](https://travis-ci.org/ssanj/dabble)

# Dabble

Ever wanted to play around with a new Scala library but wished there was an easier way to put it on the classpath of the REPL? Tired of manually copying lengthy file paths to specify the classpath?

Let Dabble handle the details for you. Simply run __dabble__ with the libraries of your choosing, similar to how you would see it on its project page on Github or on maven central. Then sit back while dabble downloads any dependencies you need, adds it to the classpath of a Scala repl and launches you in.

## Goals

1. Easy copy-paste of sbt dependencies onto dabble to get something working quickly.
2. Use SBT to do the heavy lifting.

## Requirements

SBT 0.13.x

## Example

How can we use dabble to play around with the scalaz-core library in the REPL?

Go to the [scalaz github page](https://github.com/scalaz/scalaz) to see what the latest dependency is:

```
libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.2.2"
```

Simply grab the part after the __+=__ sign and call dabble:

```
dabble "org.scalaz" %% "scalaz-core" % "7.2.2"
```

This results in the following output:

```
  _____        _     _     _
 |  __ \      | |   | |   | |
 | |  | | __ _| |__ | |__ | | ___
 | |  | |/ _` | '_ \| '_ \| |/ _ \
 | |__| | (_| | |_) | |_) | |  __/
 |_____/ \__,_|_.__/|_.__/|_|\___|

Dabble: 0.0.1-b325
Dabble: Using default sbt template at: /Users/sanj/.dabble/build.sbt
[info] Loading global plugins from /Users/sanj/.sbt/0.13/plugins
[info] Set current project to Willow (in build file:/Users/sanj/.dabble/work/)
[info] Starting scala interpreter...
[info]

Dabble injected the following libraries:
[1] org.scalaz %% scalaz-core % 7.2.2

Welcome to Scala version 2.11.7 (Java HotSpot(TM) 64-Bit Server VM, Java 1.8.0_65).
Type in expressions to have them evaluated.
Type :help for more information.
```

Now we can use scalaz as per usual:

```
scala> import scalaz._, syntax.either._
import scalaz._
import syntax.either._

scala> 5.right[String]
res1: scalaz.\/[String,Int] = \/-(5)

scala>
```

## Download

### Linux or Macosx

To get a executable on Linux or Macosx run:

```
curl -L -o dabble https://git.io/vr8B3; chmod +x dabble; ./dabble -h
```

To get a plain jar file run:

```
curl -L -o dabble.jar https://git.io/vr8BH; java -jar dabble.jar -h
```

### Windows

Download the [dabble.jar](https://git.io/vr8BH) file and run:

```
java -jar dabble.jar -h
```

Alternatively, also download the [dabble.bat](https://git.io/vr80p) file into the same directory and run it via:

```
dabble.bat -h
```

## Build

### Linux or Macosx

Build the __dabble__ executable with:

```
sbt assembly
```

Once that completes, you can add dabble to your path and make it executable:

```
cp target/scala_version/dabble ~/bin/
chmod +x ~/bin/dabble
```

You can run dabble as:

```
dabble <dependencies>
```

### Windows

Build dabble.jar with:

```
sbt -Dplain.jar assembly
```

Once that completes, you can run dabble as:

```
java -jar dabble.jar <dependencies>
```

You can also download the sample [dabble.bat](https://git.io/vr80p) file and run it as:

```
dabble.bat <dependencies>
```

## Install on OSX using with [brew](https://github.com/mxcl/homebrew)

edwardsmatt created a [brew](https://github.com/mxcl/homebrew) tap for installing the latest version (or `HEAD`) using [brew](https://github.com/mxcl/homebrew). The instructions are below, (or you can checkout the recipe for further details [here](https://github.com/edwardsmatt/homebrew-dabble)):

First, add this tap to your Brew.

    brew tap edwardsmatt/homebrew-dabble

Now you will be able to install the latest dabble release:

    brew install dabble

or the latest HEAD using:

    brew install dabble --HEAD

or, if you want more information about the install you can use:

    brew install dabble --HEAD --debug --verbose

## Running

```
dabble <dependencies> <resolvers> <macro-paradise-version>
```

With a single dependency:

```
dabble "org.scalaz" %% "scalaz-core" % "7.2.2"
```

With a single dependency and config:

```
dabble "org.scalatest" %% "scalatest" % "2.2.6" % "test"
```

With multiple dependencies separated by a __+__ sign:

```
dabble "org.scalaz" %% "scalaz-core" % "7.2.2" + "org.scalatest" %% "scalatest" % "2.2.6" % "test"
```

With a single resolver specified by __-r__:

```
dabble -r "Oncue Bintray Repo @ http://dl.bintray.com/oncue/releases" "oncue.knobs" %% "core" % "3.6.1"
```

or

```
dabble "oncue.knobs" %% "core" % "3.6.1" -r "Oncue Bintray Repo @ http://dl.bintray.com/oncue/releases"
```

_Resolvers can be specified before or after the dependencies._

With multiple resolvers separated by a __,__:

```
dabble -r "bintray:bmjames:maven, sonatype" "net.bmjames" %% "scala-optparse-applicative" % "0.3"
```

or

```
dabble "net.bmjames" %% "scala-optparse-applicative" % "0.3" -r "bintray:bmjames:maven, sonatype"
```

With a specific version of the [macro paradise](http://docs.scala-lang.org/overviews/macros/paradise.html) compiler plugin:

```
dabble "com.github.mpilquist" %% "simulacrum" % "0.7.0" -mp "2.1.0"
```

## Configuration

Dabble operates out of the ~/.dabble directory. When you run dabble, a minimal build.sbt file is generated and written to ~/.dabble/build.sbt. The ~/.dabble/work directory is then used to run the build and contains all artefacts. You can override the default build.sbt by placing your own build.sbt file under ~/.dabble/.

You can get a full list of arguments to dabble by running it with -h or --help:

```
Dabble version: 0.0.3-b892
Usage: Dabble [options] <dep1> + <dep2> + ... <depn>

  -h | --help

  -v | --version

  <dep1> + <dep2> + ... <depn>
  The list of dependencies to include.
  Multiple dependencies should be separated by a + sign.

  Format is one of:
  "org1" %  "name1" % "version1"
  "org2" %% "name2" % "version2"
  "org3" %% "name3" % "version3 % "config""
  "org1" %% "name1" % "version1" + "org2" %% "name2" % "version2"

  Example:
  "com.github.scopt" %% "scopt" % "3.4.0" + "org.scalaz" %% "scalaz-core" % "7.2.2"

  -r "<res1>,<res2>, .... <resn>" | --resolvers "<res1>,<res2>, .... <resn>"
  The list of additional repositories to resolve dependencies from.
  Multiple dependencies should be separated by commas.

  Format is one of:
  (sonatype|typesafe|typesafeIvy|sbtPlugin):[s|r]
  (maven2|jcenter)
  bintray(owner:repo)
  name@repo_url

  sonatype:s -- loads only snapshot repo
  sonatype:r -- loads only release repo
  sonatype   -- loads both snapshot and release repos
  maven2     -- loads the maven2 resolver
  bintray:user:repo  -- loads the bintray resolver for user/repo
  your repo name @ https://your.repo.com/release/maven -- loads a custom resolver

  Example:
  "bintray:oncue:releases, sonatype:r"

  -mp <version> | --macro-paradise <version>
  Includes the macro paradise compiler plugin with the supplied version.

  Example:
  2.1.0
```
