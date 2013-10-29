package scala.tools.refactoring.tests.implementations.modules

import org.junit.Assert._
import scala.tools.refactoring.tests.util.TestHelper
import scala.tools.refactoring.common.InteractiveScalaCompiler
import scala.tools.refactoring.Refactoring

trait TestModule extends TestHelper { outer =>
  abstract class ModuleTest(fs: FileSet) extends InteractiveScalaCompiler with Refactoring with GlobalIndexes {
    val global = outer.global
    lazy val selection = outer.selection(outer, fs).asInstanceOf[Selection]

    override val index = global.ask { () =>
      val trees = fs.sources map (x => addToCompiler(fs.fileName(x), x)) map (global.unitOfFile(_).body)
      val cuIndexes = trees map (_.pos.source.file) map { file =>
        global.unitOfFile(file).body
      } map CompilationUnitIndex.apply
      GlobalIndex(cuIndexes)
    }
  }
}