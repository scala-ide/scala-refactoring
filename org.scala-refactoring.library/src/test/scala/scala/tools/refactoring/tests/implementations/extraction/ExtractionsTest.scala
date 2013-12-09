package scala.tools.refactoring.tests.implementations.extraction

import scala.tools.refactoring.tests.util.TestHelper
import scala.tools.refactoring.implementations.extraction.Extractions
import org.junit.Assert._

class ExtractionsTest extends TestHelper with Extractions {
  @Test
  def extractionTargets = {
    val s = toSelection("""
      object O{
        def fn = {
          val a = 1
          /*(*/2 * a/*)*/
        }
      }
    """)

    FakeCollector.collectExtractions(s)
    assertEquals(2, FakeCollector.extractionTargets.length)
  }
  
  @Test
  def noExtractionTargetsForSyntheticScopes = {
    val s = toSelection("""
      object O{
        def fn =
          1 :: 2 :: /*(*/Nil/*)*/
      }
    """)

    FakeCollector.collectExtractions(s)
    assertEquals(2, FakeCollector.extractionTargets.length)
  }

  object FakeCollector extends ExtractionCollector[Extraction] {
    var extractionTargets: List[ExtractionTarget] = Nil

    def prepareExtractionSource(s: Selection): Either[PreparationError, Selection] = Right(s)

    def prepareExtractions(source: Selection, targets: List[ExtractionTarget]): Either[PreparationError, List[Extraction]] = {
      extractionTargets = targets
      Left("")
    }
  }
}