package scala.tools.refactoring.implementations.modules

import scala.tools.refactoring.common.Selections

object ExtractionSources {
  trait ExtractionSource extends RefactoringModule with Selections {
    import global._

    def selection: Selection

    val extractedCode: List[Tree]

    lazy val extractedSymbols: List[Symbol] =
      extractedCode flatMap { expr =>
        expr.collect {
          case t: SymTree => t.symbol
        }
      }

    def replaceBy(replacement: Tree): Transformation[Tree, Tree]
  }

  trait FromExpressions extends ExtractionSource {
    import global._

    val extractedCode = selection.selectedTopLevelTrees

    private val definesNonLocal = extractedCode.exists {
      case t: MemberDef => !t.symbol.isLocal
      case _ => false
    }

    private val definesNonValue = extractedCode.exists {
      case t: MemberDef => !t.isInstanceOf[ValDef]
      case _ => false
    }

    override def preparationError =
      if (definesNonLocal)
        Some("Can't extract expression that defines non local fields.")
      else if (definesNonValue)
        Some("Can't extract expression that defines non-value symbols.")
      else if (extractedCode.size == 0)
        Some("No expression or statement selected.")
      else
        super.preparationError

    def replaceBy(replacement: Tree) = {
      val replaceSingleExpression =
        replaceTree(extractedCode.head, replacement)

      val replaceSequence =
        transform {
          case block @ Block(stats, expr) =>
            val newStats = (stats :+ expr).replaceSequence(extractedCode, replacement :: Nil)
            mkBlock(newStats) replaces block
          case template @ Template(_, _, body) =>
            val newBody = body.replaceSequence(extractedCode, replacement :: Nil)
            template copy (body = newBody) replaces template
        }

      topdown {
        matchingChildren {
          if (extractedCode.size == 1)
            replaceSingleExpression
          else
            replaceSequence
        }
      }
    }
  }
}