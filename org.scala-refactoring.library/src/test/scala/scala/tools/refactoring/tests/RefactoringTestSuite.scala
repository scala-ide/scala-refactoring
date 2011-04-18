package scala.tools.refactoring
package tests

import org.junit.runner.RunWith
import org.junit.runners.Suite
import analysis._
import common._
import implementations._
import sourcegen._
import transformation._
import util._

@RunWith(value = classOf[Suite])
@Suite.SuiteClasses(value = Array(
    classOf[DeclarationIndexTest], 
    classOf[MultipleFilesIndexTest], 
    classOf[NameValidationTest], 
    classOf[TreeAnalysisTest], 
    classOf[SelectionsTest],
    classOf[PimpedTreesTest],
    classOf[AddImportStatementTest], 
    classOf[EliminateMatchTest], 
    classOf[ExplicitGettersSettersTest], 
    classOf[ExtractLocalTest], 
    classOf[ExtractMethodTest], 
    classOf[InlineLocalTest], 
    classOf[MarkOccurrencesTest], 
    classOf[OrganizeImportsTest], 
    classOf[UnusedImportsFinderTest], 
    classOf[OrganizeMissingImportsTest], 
    classOf[RenameTest], 
    classOf[IndividualSourceGenTest], 
    classOf[LayoutTest], 
    classOf[PrettyPrinterTest], 
    classOf[SourceGenTest], 
    classOf[SourceHelperTest], 
    classOf[TreeChangesDiscovererTest], 
    classOf[NotTest], 
    classOf[FindShadowedTest], 
    classOf[TreeTransformationsTest]))
class RefactoringTestSuite {}