[The Scala Refactoring Project](http://scala-refactoring.org)
================================================================================

Welcome to the Scala Refactoring project!

The project requires Scala 2.10 but also supports 2.11.

1. Building
--------------------------------------------------------------------------------

We use Maven to build the project:

```bash
$ ./build-2.11.sh
```

Creates all the artifacts, except for the documentation, which needs to be
built separately. For different versions of Scala, take a look at the build.sh
script.

2. Publishing
--------------------------------------------------------------------------------

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

3. License
--------------------------------------------------------------------------------

The project is licensed under the Scala license, see the LICENSE file for details.
