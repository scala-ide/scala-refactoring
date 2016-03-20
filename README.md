# The Scala Refactoring Library

The Scala Refactoring Library implements IDE independent refactoring support
for Scala. It is currently used by both [ENSIME](https://github.com/ensime)
and [ScalaIDE](http://scala-ide.org/) and supports Scala-2.10 and Scala-2.11.

## Information for Developers

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
