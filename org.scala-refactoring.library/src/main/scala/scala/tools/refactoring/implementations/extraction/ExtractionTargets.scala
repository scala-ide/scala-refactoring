package scala.tools.refactoring.implementations.extraction

import scala.tools.refactoring.implementations.modules.RefactoringModule
import scala.reflect.internal.Flags

object ExtractionTargets {
  trait NewVal extends RefactoringModule {
    import global._
    
    def abstraction(name: String, stats: List[Tree]) = {
      val body =
        stats match {
          case (b: Block) :: Nil => b
          case stats => mkBlock(stats)
        }
      mkValDef(name, body)
    }

    def call(name: String) = {
      Ident(name)
    }
  }
  
  trait NewDef extends RefactoringModule {
    import global._

    def abstraction(scope: Tree, name: String, params: List[Symbol], returns: List[Symbol], stats: List[Tree]) = {
      val body = {
        val returnStatement = if (returns.isEmpty) Nil else mkReturn(returns) :: Nil
        stats match {
          // a single block tree could be unpacked
          case Block(stats, expr) :: Nil => stats ::: expr :: returnStatement
          case stats => stats ::: returnStatement
        }
      }

      val mods = scope match {
        case _: Template => NoMods withPosition (Flags.PRIVATE, NoPosition)
        case _ => NoMods
      }

      mkDefDef(
        mods,
        name,
        if (params.isEmpty) Nil else params :: Nil,
        body)
    }

    def call(name: String, params: List[Symbol], returns: List[Symbol]) = {
      mkCallDefDef(name, params :: Nil, returns)
    }
  }
}