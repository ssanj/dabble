[![Build Status](https://travis-ci.org/ssanj/dabble.svg?branch=master)](https://travis-ci.org/ssanj/dabble)

# Dabble

Ever wanted to play around with a new Scala library but wished there was an easier way to put it on the classpath of the Scala repl? Tired of manually copying lengthy file paths to specify the classpath?

Let Dabble handle the details for you. Simply run __dabble__ with the libraries of your choosing, similar to how you would see it on its project page on Github or on maven central. Then sit back while dabble downloads any dependencies you need, adds it to the classpath of a Scala repl and launches you in.

## Goals

1. Easy copy-paste of sbt dependencies onto dabble to get something working quickly.
2. Use SBT to do the heavy lifting.

## Build

To build dabble.jar, use:

```
sbt assembly
```

## Configuration

Dabble operates out of the ~/.dabble directory. When you run dabble, a minimal build.sbt file is generated and written to ~/.dabble/build.sbt. The ~/.dabble/work directory is then used to run the build and contains all artefacts.

## Usage

Example usage:

```
java -jar dabble.jar "com.chuusai" %% "shapeless" % "2.3.0"
```