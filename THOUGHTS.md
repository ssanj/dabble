# Design Thoughts

## How show we accept multiple imports?

### space-separated
```dabble "io.argonaut" %% "argonaut" % "6.1" "org.scalaz" %% "scalaz-core" % "7.1.2"```

### double-dash-separated
```dabble "io.argonaut %% argonaut % 6.1 -- org.scalaz %% scalaz-core % 7.1.2"```

### plus-separated
```dabble "io.argonaut" %% "argonaut" % "6.1" + "org.scalaz" %% "scalaz-core" % "7.1.2"```

## Some additional features

1. store temp files in ~/.dabble/work, or specify working dir
   dabble -w|--working-dir /path/to/working/dir
2. Ability to specify a template.sbt file. Default one in ~/.dabble/template.sbt
   dabble -t|--template /path/to/template
3. Ability to specify information about dabble -> working dir, template in use etc.
   Eg: dabble -i|--info
   working directory: /some/path
   template: some template
   config: /path/to/config
   named dependencies:
    [name1] values1
    [name2] values2
4. Create a named section within the config for dependencies.
   Eg: dabble -n|--named scalaz (looks up scalaz section in config and loads dependencies from there)

   In config file:
   ```
   [scalaz-7.1.2]
   org.scalaz %% scalaz-core % 7.1.2

   [scalaz-all-]
   org.scalaz %% scalaz-core % 7.20
   org.scalaz %% scalaz-concurrent % 7.20
   org.scalaz %% scalaz-effect % 7.20
   ```

   It would be cool to replace the version of a name section with what was supplied.

   Eg. dabble -n scalaz 7.1.4 (replaces stored version of 7.1.2 -> 7.1.4)
5. Create UUID for project name.
6. Create UUID for org.
7. Add more information about run configuration:
  1. Mention that the default template can be overridden with a -t parameter or by creating a template.sbt file in the ~/.dabble directory
  2. Specify home and working directory and whether the default template is being used.
8. Changed WrappedArray(...) output to something nicer.
9. Possibly create history file to store previous executions. Then allow using a previous execution from history.