package scala.tools.refactoring
package tests

import org.junit.runner.RunWith
import org.junit.runners.Suite
import analysis._
import common._
import implementations._
import implementations.imports._
import sourcegen._
import transformation._
import util._
import scala.tools.refactoring.implementations.OrganizeImportsAlgosTest

@RunWith(value = classOf[Suite])
@Suite.SuiteClasses(value = Array(
    classOf[AddFieldTest],
    classOf[AddImportStatementTest],
    classOf[AddMethodTest],
    classOf[ChangeParamOrderTest],
    classOf[CompilationUnitDependenciesTest],
    classOf[CustomFormattingTest],
    classOf[DeclarationIndexTest],
    classOf[EliminateMatchTest],
    classOf[ExpandCaseClassBindingTest],
    classOf[ExplicitGettersSettersTest],
    classOf[extraction.ExtractCodeTest],
    classOf[extraction.ExtractExtractorTest],
    classOf[extraction.ExtractionsTest],
    classOf[extraction.ExtractMethodTest],
    classOf[extraction.ExtractParameterTest],
    classOf[extraction.ExtractValueTest],
    classOf[ExtractLocalTest],
    classOf[ExtractMethodTest],
    classOf[ExtractTraitTest],
    classOf[FindShadowedTest],
    classOf[GenerateHashcodeAndEqualsTest],
    classOf[ImportAnalysisTest],
    classOf[IndividualSourceGenTest],
    classOf[InlineLocalTest],
    classOf[InsertionPositionsTest],
    classOf[IntroduceProductNTraitTest],
    classOf[LayoutTest],
    classOf[MarkOccurrencesTest],
    classOf[MergeParameterListsTest],
    classOf[MoveClassTest],
    classOf[MoveConstructorToCompanionObjectTest],
    classOf[MultipleFilesIndexTest],
    classOf[NameValidationTest],
    classOf[OrganizeImportsCollapseSelectorsToWildcardTest],
    classOf[OrganizeImportsFullyRecomputeTest],
    classOf[OrganizeImportsGroupsTest],
    classOf[OrganizeImportsRecomputeAndModifyTest],
    classOf[OrganizeImportsTest],
    classOf[OrganizeImportsWildcardsTest],
    classOf[OrganizeMissingImportsTest],
    classOf[EnrichedTreesTest],
    classOf[PrependOrDropScalaPackageFromRecomputedTest],
    classOf[PrependOrDropScalaPackageKeepTest],
    classOf[PrettyPrinterTest],
    classOf[RenameTest],
    classOf[ReusingPrinterTest],
    classOf[ScopeAnalysisTest],
    classOf[SelectionDependenciesTest],
    classOf[SelectionExpansionsTest],
    classOf[SelectionPropertiesTest],
    classOf[SelectionsTest],
    classOf[SourceGenTest],
    classOf[SourceHelperTest],
    classOf[SplitParameterListsTest],
    classOf[TransformableSelectionTest],
    classOf[TreeAnalysisTest],
    classOf[TreeChangesDiscovererTest],
    classOf[TreeTransformationsTest],
    classOf[UnionFindInitTest],
    classOf[UnionFindTest],
    classOf[UnusedImportsFinderTest],
    classOf[OrganizeImportsAlgosTest]))
class RefactoringTestSuite {}
