package scala.tools.refactoring.implementations.extraction

abstract class ExtractCode extends ExtractionRefactoring with AutoExtractions {
  val collector = AutoExtraction
}

trait AutoExtractions extends MethodExtractions with ValueExtractions with ExtractorExtractions with ParameterExtractions {
  object AutoExtraction extends ExtractionCollector[Extraction] {
    val collectors = ExtractorExtraction :: ValueOrMethodExtraction :: ParameterExtraction :: Nil

    override def collect(s: Selection) = {
      var applicableCollectors: List[ExtractionCollector[_ <: Extraction]] = Nil
      val sourceOpt = s.expand.expandTo { source: Selection =>
        applicableCollectors = collectors.filter(_.isValidExtractionSource(source))
        !applicableCollectors.isEmpty
      }

      val extractions = applicableCollectors.flatMap { collector =>
        collector.collect(sourceOpt.get).right.getOrElse(Nil)
      }

      if (extractions.isEmpty)
        Left("No applicable extraction found.")
      else
        Right(extractions.sortBy(-_.extractionTarget.enclosing.pos.startOrPoint))
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