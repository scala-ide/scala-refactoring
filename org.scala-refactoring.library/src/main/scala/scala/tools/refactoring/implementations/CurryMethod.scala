package scala.tools.refactoring
package implementations



abstract class CurryMethod extends MethodSignatureRefactoring {

  import global._
  
  type SplitPositions = List[Int]
  type RefactoringParameters = List[SplitPositions]
  
  override def checkRefactoringParams(prep: PreparationResult, params: RefactoringParameters) = {
    def checkRefactoringParamsHelper(vparamss: List[List[ValDef]], sectionss: List[SplitPositions]): Boolean = {
      val sortedSections = sectionss.map(Set(_: _*).toList.sorted)
      if(sortedSections != sectionss || vparamss.size != sectionss.size) {
        false
      } else {
        val emptyRange = 1 to 0
        val sectionRanges = sectionss.map {case Nil => emptyRange ; case s => s.head to s.last}
        val vparamsRanges = vparamss.map(1 until _.size)
        (vparamsRanges zip sectionRanges).foldLeft(true)((b, ranges) => b && (ranges._1 containsSlice ranges._2))
      }
    }
    
    checkRefactoringParamsHelper(prep._1.vparamss, params)
  }
  
  def currySingleParamList[T](origVparams: List[T], positions: SplitPositions): List[List[T]] = {
    val nrParamsPerList = (positions:::List(origVparams.length) zip 0::positions) map (t => t._1 - t._2)
    nrParamsPerList.foldLeft((Nil: List[List[T]] , origVparams))((acc, nrParams) => {
      val (currentCurriedParamList, remainingOrigParams) = acc._2 splitAt(nrParams)
      (acc._1:::List(currentCurriedParamList), remainingOrigParams)
    })._1
  }
    
  def makeCurriedApply(baseFun: Tree, vparamss: List[List[Tree]]): Apply = {
    val firstApply = Apply(baseFun, vparamss.headOption.getOrElse(throw new IllegalArgumentException("can't handle empty vparamss")))
    vparamss.tail.foldLeft(firstApply)((fun, vparams) => Apply(fun, vparams))
  }
    
  override def defdefRefactoring(params: RefactoringParameters) = transform {
    case orig @ DefDef(mods, name, tparams, vparamss, tpt, rhs) => {
      val curried = (vparamss zip params) flatMap (l => currySingleParamList(l._1, l._2))
      DefDef(mods, name, tparams, curried, tpt, rhs) replaces orig
    }
  }
    
  override def applyRefactoring(params: RefactoringParameters) = transform {
    case apply @ Apply(fun, args) => {
      val originalTree = findOriginalTree(apply)
      val pos = paramListPos(originalTree) - 1
      val curriedParamLists = currySingleParamList(apply.args, params(pos))
      val curriedApply = makeCurriedApply(fun, curriedParamLists)
      curriedApply replaces apply
    }
  }
    
  override def traverseApply[X <% (X ⇒ X) ⇒ X](t: ⇒ Transformation[X, X]) = bottomup(t)
  
  override def prepareParamsForSingleRefactoring(originalParams: RefactoringParameters, nrParamLists: Int): RefactoringParameters = {
    val toDrop = originalParams.size - nrParamLists
    originalParams.drop(toDrop)
  }
  
}