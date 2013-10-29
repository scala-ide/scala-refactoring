package scala.tools.refactoring.implementations.modules

import scala.tools.refactoring.analysis.Indexes

object Dependencies {

  trait Dependencies extends RefactoringModule{
    import global._
    
    def undefinedDepsInScope(scope: Tree): List[Symbol]
    def definedDepsInScope(scope: Tree): List[Symbol]
    val outboundDeps: List[Symbol]
  }
  
  trait Values extends Dependencies with Indexes { self: ExtractionSources.ExtractionSource with InsertionPoints.InsertionPoint =>
    import global._
    
    lazy val inboundDeps =
      (for {
        selected <- extractedSymbols
        declaration <- index.declaration(selected)
        if !selection.pos.includes(declaration.pos)
      } yield selected)
        .distinct

    def undefinedDepsInScope(scope: Tree) = {
      inboundDeps.filter(!symbolKnownAtInsertionPoint(scope, _))
    }

    def definedDepsInScope(scope: Tree) =
      inboundDeps.filter(symbolKnownAtInsertionPoint(scope, _))

    lazy val outboundDeps = {
      val selectedDeclarations =
        for {
          t <- extractedCode
          decl <- t.filter {
            case decl: MemberDef => true
            case _ => false
          }
        } yield decl.symbol

      val usesOfSelectedDeclarations = selectedDeclarations.flatMap(index.occurences)

      usesOfSelectedDeclarations.filterNot(selection.contains).map(_.symbol).distinct
    }

    override def preparationError = {
      val outboundTypeDeps = !outboundDeps.filter(_.isType).isEmpty

      if (outboundTypeDeps)
        Some("Can't extract expressions that define types used elsewhere.")
      else
        super.preparationError
    }
  }
  
}