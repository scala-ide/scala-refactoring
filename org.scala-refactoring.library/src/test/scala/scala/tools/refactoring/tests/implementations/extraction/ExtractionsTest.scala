package scala.tools.refactoring.tests.implementations.extraction

import scala.tools.refactoring.tests.util.TestHelper
import scala.tools.refactoring.implementations.extraction.Extractions
import org.junit.Assert._

class ExtractionsTest extends TestHelper with Extractions {
  @Test
  def findExtractionTargets = {
    val s = toSelection("""
      object O{
        def fn = {
          val a = 1
          /*(*/2 * a/*)*/
        }
      }
    """)

    collectExtractions(s)
    assertEquals(2, extractionTargets.length)
  }

  @Test
  def noExtractionTargetsForSyntheticScopes = {
    val s = toSelection("""
      object O{
        def fn =
          1 :: 2 :: /*(*/Nil/*)*/
      }
    """)

    collectExtractions(s)
    assertEquals(2, extractionTargets.length)
  }

  var extractionTargets: List[ExtractionTarget] = Nil

  def prepareExtractionSource(s: Selection): Either[PreparationError, Selection] = Right(s)

  def prepareExtractions(source: Selection, targets: List[ExtractionTarget]): Either[PreparationError, List[Extraction]] = {
    extractionTargets = targets
    Left("")
  }
}