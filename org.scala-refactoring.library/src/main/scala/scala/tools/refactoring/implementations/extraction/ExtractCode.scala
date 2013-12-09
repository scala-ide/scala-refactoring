package scala.tools.refactoring.implementations.extraction

abstract class ExtractCode extends ExtractionRefactoring with AutoExtractions

trait AutoExtractions extends MethodExtractions with ValueExtractions {
  override def prepareExtractionSource(s: Selection) =
    super[MethodExtractions].prepareExtractionSource(s)
  
  override def prepareExtractions(source: Selection, targets: List[ExtractionTarget]) = {
    val valueExtractions = (if (source.mayHaveSideEffects)
      Right(Nil)
    else
      super[ValueExtractions].prepareExtractions(source, targets)).right.getOrElse(Nil)

    val remainingTargets = targets.filterNot(t => valueExtractions.exists(e => e.extractionTarget == t))
    
    val methodExtractions = super[MethodExtractions].prepareExtractions(source, remainingTargets).right.getOrElse(Nil)
    
    (valueExtractions ::: methodExtractions) match {
      case Nil => Left(noExtractionMsg)
      case es => Right(es)
    }
  }
}