# The Scala Refactoring Library

The Scala Refactoring Library implements IDE independent refactoring support
for Scala. It is currently used by both [ENSIME](https://github.com/ensime)
and [ScalaIDE](http://scala-ide.org/) and supports Scala-2.10 and Scala-2.11.


## Building

Use `$ sbt package` to build the library for Scala-2.11, or `$ sbt +package` to
build it both for Scala-2.11 and Scala-2.10.

## Publishing

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
