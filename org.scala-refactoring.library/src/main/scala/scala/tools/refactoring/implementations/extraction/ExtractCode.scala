package scala.tools.refactoring.implementations.extraction

abstract class ExtractCode extends ExtractionRefactoring with AutoExtractions {
  val collector = AutoExtraction
}

trait AutoExtractions extends MethodExtractions with ValueExtractions with ExtractorExtractions {
  object AutoExtraction extends ExtractionCollector[Extraction] {
    val collectors = ExtractorExtraction :: ValueOrMethodExtraction :: Nil

    override def collect(s: Selection) = {
      var applicableCollector: Option[ExtractionCollector[_ <: Extraction]] = None
      val sourceOpt = s.expand.expandTo { source: Selection =>
        applicableCollector = collectors.find(_.isValidExtractionSource(source))
        applicableCollector.isDefined
      }
      
      applicableCollector.map{ collector =>
        collector.collect(sourceOpt.get)
      }.getOrElse(Left(""))
    }
    
    def isValidExtractionSource(s: Selection) = ???
    
    def createExtractions(source: Selection, targets: List[ExtractionTarget]) = ???
  }

  object ValueOrMethodExtraction extends ExtractionCollector[Extraction] {
    def isValidExtractionSource(s: Selection) =
      MethodExtraction.isValidExtractionSource(s)

    def createExtractions(source: Selection, targets: List[ExtractionTarget]) = {
      val valueExtractions =
        if (source.mayHaveSideEffects)
          Nil
        else
          ValueExtraction.createExtractions(source, targets)

      val remainingTargets = targets.filterNot(t => valueExtractions.exists(e => e.extractionTarget == t))

      val methodExtractions = MethodExtraction.createExtractions(source, remainingTargets)

      valueExtractions ::: methodExtractions
    }
  }
}