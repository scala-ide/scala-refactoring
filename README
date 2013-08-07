================================================================================
  The Scala Refactoring Project -- www.scala-refactoring.org
================================================================================

Welcome to the Scala Refactoring project!

The project requires Scala 2.10 but also supports 2.11. For older versions of
Scala, please take a look at the various branches.

1. Project Layout
--------------------------------------------------------------------------------

The project is organized as follows: the main project is the library, the
feature and update-site projects are only needed when the library is used as a
plug-in for Eclipse.

The examples directory contains two demos:
 - diy is an example of how the library can be used to build new refactorings.
 - editor is a simple swing editor that integrates the refactorings.

├── pom.xml
├── build-2.10.sh
├── build-2.11.sh
├── org.scala-refactoring.library
│   ├── build.properties
│   ├── META-INF
│   ├── pom.xml
│   └── src
│       ├── doc
│       │   ├── latex
│       │   │   └── ...
│       │   └── svg
│       │       └── ...
│       ├── main
│       │   └── scala
│       │       └── ...
│       └── test
│           └── scala
│               └── ...
├── examples
│   ├── org.scala-refactoring.diy
│   │   ├── pom.xml
│   │   ├── README
│   │   └── src
│   │       └── main
│   │           └── scala
│   │               └── org
│   │                   └── scala-refactoring
│   │                       └── ExplicitGettersSetters.scala
│   └── org.scala-refactoring.editor
│       ├── pom.xml
│       ├── README
│       └── src
│           └── main
│               └── scala
│                   └── org
│                       └── scala-refactoring
│                           ├── EditorUi.java
│                           └── RefactoringEditor.scala
├── org.scala-refactoring.feature
│   ├── build.properties
│   ├── feature.xml
│   └── pom.xml
└── org.scala-refactoring.update-site
    ├── index.html
    ├── pom.xml
    ├── site.xml
    └── web

2. Building
--------------------------------------------------------------------------------

We use Maven to build the project:

> ./build-2.10.sh

creates all the artifacts, except for the documentation, which needs to be
built separately. For different versions of Scala, take a look at the build.sh
script.

A separate P2 repository can be built using the build_p2_repo.sh script.

3. License
--------------------------------------------------------------------------------

The project is licensed under the Scala license, see the LICENSE file for details.
