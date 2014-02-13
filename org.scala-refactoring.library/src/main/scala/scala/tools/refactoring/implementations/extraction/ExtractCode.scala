package scala.tools.refactoring.implementations.extraction

/**
 * General extraction refactoring that proposes different concrete extractions based on the
 * current selection.
 */
abstract class ExtractCode extends ExtractionRefactoring with AutoExtractions {
  val collector = AutoExtraction
}

trait AutoExtractions extends MethodExtractions with ValueExtractions with ExtractorExtractions with ParameterExtractions {
  /**
   * Proposes different kinds of extractions.
   */
  object AutoExtraction extends ExtractionCollector[Extraction] {
    /**
     * Extraction collectors used for auto extraction.
     */
    val availableCollectors =
      ExtractorExtraction ::
        ValueOrMethodExtraction ::
        ParameterExtraction ::
        Nil

    /**
     * Searches for an extraction source that is valid for at least one
     * extraction collector. If an appropriate source is found it calls
     * `collect(s)` on every collector that accepts this source.
     */
    override def collect(s: Selection) = {
      var applicableCollectors: List[ExtractionCollector[Extraction]] = Nil
      val sourceOpt = s.expand.expandTo { source: Selection =>
        applicableCollectors = availableCollectors.filter(_.isValidExtractionSource(source))
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

  /**
   * Proposes either value or method extractions for an extraction target depending
   * on whether all inbound dependencies are satisfied in the respective scope or not.
   *
   * If the extraction source contains (obvious) side effects it proposes only method
   * extractions. 
   */
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