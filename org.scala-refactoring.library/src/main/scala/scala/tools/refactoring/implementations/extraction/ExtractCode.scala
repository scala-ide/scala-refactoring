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
        ValueExtraction ::
        MethodExtraction ::
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

    def createExtractions(source: Selection, targets: List[ExtractionTarget], name: String) = ???
  }
}