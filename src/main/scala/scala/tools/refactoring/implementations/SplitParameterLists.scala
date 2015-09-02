package scala.tools.refactoring
package implementations

/**
 * Refactoring to split parameter lists of a method.
 */
abstract class SplitParameterLists extends MethodSignatureRefactoring {

  import global._

  type SplitPositions = List[Int]
  /**
   * Split positions must be provided for every parameter list of the method (though they can be Nil)
   */
  type RefactoringParameters = List[SplitPositions]

  override def checkRefactoringParams(prep: PreparationResult, affectedDefs: AffectedDefs, params: RefactoringParameters) = {
    def checkRefactoringParamsHelper(vparamss: List[List[ValDef]], sectionss: List[SplitPositions]): Boolean = {
      val sortedSections = sectionss.map(Set(_: _*).toList.sorted)
      if(sortedSections != sectionss || vparamss.size != sectionss.size) {
        false
      } else {
        val emptyRange = 1 to 0
        val sectionRanges = sectionss map { case Nil => emptyRange ; case s => s.head to s.last }
        val vparamsRanges = vparamss.map(1 until _.size)
        (vparamsRanges zip sectionRanges).foldLeft(true)((b, ranges) => b && (ranges._1 containsSlice ranges._2))
      }
    }

    checkRefactoringParamsHelper(prep.defdef.vparamss, params)
  }

  def splitSingleParamList[T](origVparams: List[T], positions: SplitPositions): List[List[T]] = {
    val nrParamsPerList = (positions:::List(origVparams.length) zip 0::positions) map (t => t._1 - t._2)
    nrParamsPerList.foldLeft((Nil: List[List[T]] , origVparams))((acc, nrParams) => {
      val (currentCurriedParamList, remainingOrigParams) = acc._2 splitAt nrParams
      (acc._1:::List(currentCurriedParamList), remainingOrigParams)
    })._1
  }

  def makeSplitApply(baseFun: Tree, vparamss: List[List[Tree]]) = {
    val firstApply = Apply(baseFun, vparamss.headOption.getOrElse(throw new IllegalArgumentException("can't handle empty vparamss")))
    vparamss.tail.foldLeft(firstApply)((fun, vparams) => Apply(fun, vparams))
  }

  override def defdefRefactoring(params: RefactoringParameters) = transform {
    case orig @ DefDef(mods, name, tparams, vparamss, tpt, rhs) => {
      val split = (vparamss zip params) flatMap (l => splitSingleParamList(l._1, l._2))
      DefDef(mods, name, tparams, split, tpt, rhs) replaces orig
    }
  }

  override def applyRefactoring(params: RefactoringParameters) = transform {
    case apply @ Apply(fun, args) => {
      val originalTree = findOriginalTree(apply)
      val pos = paramListPos(originalTree) - 1
      val splitParamLists = splitSingleParamList(apply.args, params(pos))
      val splitApply = makeSplitApply(fun, splitParamLists)
      splitApply replaces apply
    }
  }

  override def traverseApply(t: ⇒ Transformation[X, X]) = bottomup(t)

  override def prepareParamsForSingleRefactoring(originalParams: RefactoringParameters, selectedMethod: DefDef, toRefactor: DefInfo): RefactoringParameters = {
    val toDrop = originalParams.size - toRefactor.nrParamLists
    val preparedParams = originalParams.drop(toDrop)
    val noSplitters = (1 to toRefactor.nrUntouchableParamLists).toList
    noSplitters match {
      case Nil => preparedParams
      case _ => noSplitters.map(_ => Nil):::preparedParams
    }


  }

}