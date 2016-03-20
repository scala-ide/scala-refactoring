# The Scala Refactoring Library

The Scala Refactoring Library implements IDE independent refactoring support
for Scala. It is currently used by both [ENSIME](https://github.com/ensime)
and [ScalaIDE](http://scala-ide.org/) and supports Scala-2.10 and Scala-2.11.

## Information for Contributors

### Help Needed

We are always looking for helping hands. As you may have already found out,
there are quite a few bugs to work on. Pull requests are usually reviewed and
merged quickly, and we try to help new contributors wherever we can. We are
currently focusing our efforts on 

* Rename
* Organize Imports
* Add Import
* Move Class

All other refactorings, like

* Extract Method
* Extract Value
* Extract Trait
* Merge Parameter Lists
* Split Parameter Lists
* Change Parameter Order

are effectively unmaintained. If you are particularly fond of an operation from
this list, this might be a very good place to start contributing!

### Overall Architecture

Refactorings are implemented on top of 
[ASTs](http://docs.scala-lang.org/overviews/reflection/symbols-trees-types) 
from the Scala Presentation Compiler. If you plan to work on the library, you
should definitely take a look at
[Mirko Sockers Master Theses](http://scala-refactoring.org/wp-content/uploads/scala-refactoring.pdf).
Although somewhat outdated, this document is very helpful for
understanding the inner workings of the library. Another thing to look at is
the [presentation](https://youtu.be/Josjt_awx08) about the Refactoring Library given
by [@mlangc](https://github.com/mlangc) at [ScalaSphere](http://scalasphere.org/). The slides are available
[here](http://scalasphere.org/wp-content/uploads/2016/02/Matthias_langer.pdf).

### Testing

All changes to the library are expected to be thoroughly covered by unit tests,
unless there is a very good reason for violating this rule. Often however, you
also want to see the effect of your changes in an interactive environment. If
you are using
[ScalaIDE Nightly Builds](http://scala-ide.org/download/nightly.html), there
are two ways to run the IDE with your custom modifications, both with their
own drawbacks and advantages:

* You can run ScalaIDE from within Eclipse, as outlined 
   [here](http://scala-ide.org/docs/dev/setup/setup.html#run-the-scala-ide-within-eclipse).
   The [Equinox Weaving Launcher](https://github.com/scala-ide/equinox-weaving-launcher)
   will automatically pick up the local version of the Refactoring Library, if
   the library is properly configured as an Eclipse project.

   * Advantages:
      * Fast turnaround
      * Works even your changes break binary compatibility
      * The only option if the IDE and the Refactoring Library are modified together

   * Disadvantages: 
      * Not suitable if you want to see the effect of your changes in a production environment
* You can patch your
   [ScalaIDE Nightly](http://scala-ide.org/download/nightly.html) installation
   with a local build of the refactoring library, generated with 
   `$ sbt package`. Take a look at [patch-ide.bash](patch-ide.bash) to see
   how this is done.
   * Advantages:
      * Perfect for testing the effects of your changes in a production environment

   * Disadvantages: 
      * Slow turnaround
      * Works only if binary compatibility is maintained
      * Might break your Eclipse installation if you are not careful

### Internals

#### Parsing Scala Source Code

Unfortunately the typechecked ASTs the library gets from the Scala Presentation
Compiler don't always provide enough information to properly perform
refactorings. In these cases the library has to extract the missing information
from the source code under consideration. Often this is more tricky than you
might initially think. Using the APIs centered around 
[SourceWithMarker](src/main/scala/scala/tools/refactoring/util/SourceWithMarker.scala)
should make your life a lot easier in these cases.

#### Tracing

Make sure to
[switch to DebugTracing](src/main/scala/scala/tools/refactoring/common/package.scala)
when debugging the library. Sometimes it useful to redirect tracing output
to a file. This can be done by setting the system property
`scala.refactoring.traceFile`.

### Building

Use `$ sbt package` to build the library for Scala-2.11, or `$ sbt +package` to
build it both for Scala-2.11 and Scala-2.10.

### Publishing

In order to publish sbt is used. The project is cross compiled against Scala 2.10
and 2.11. To test a release run sbt and type:
```
> + publishLocalSigned
```

If everything looks good the release can be uploaded:
```
> + publishSigned
```

For the upload the file `~/.m2/credentials` is needed. It should contain:
```
realm=Sonatype Nexus Repository Manager
host=oss.sonatype.org
user=USERNAME
password=PASSWORD
```
where `USERNAME` and `PASSWORD` are the ones from the Sonatype account, which has the
necessary permissions to do a publish.

## License

The project is licensed under the Scala license, see the LICENSE file for details.
