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
@RunWith(value = classOf[Suite])
@Suite.SuiteClasses(value = Array(
    classOf[AddImportStatementTest],
    classOf[ChangeParamOrderTest],
    classOf[CompilationUnitDependenciesTest],
    classOf[CustomFormattingTest],
    classOf[DeclarationIndexTest],
    classOf[EliminateMatchTest],
    classOf[ExpandCaseClassBindingTest],
    classOf[ExplicitGettersSettersTest],
    classOf[ExtractLocalTest],
    classOf[ExtractMethodTest],
    classOf[ExtractTraitTest],
    classOf[FindShadowedTest],
    classOf[GenerateHashcodeAndEqualsTest],
    classOf[IndividualSourceGenTest],
    classOf[InlineLocalTest],
    classOf[IntroduceProductNTraitTest],
    classOf[LayoutTest],
    classOf[MarkOccurrencesTest],
    classOf[MergeParameterListsTest],
    classOf[MoveClassTest],
    classOf[MoveConstructorToCompanionObjectTest],
    classOf[MultipleFilesIndexTest],
    classOf[NameValidationTest],
    classOf[OrganizeImportsFullyRecomputeTest],
    classOf[OrganizeImportsGroupsTest],
    classOf[OrganizeImportsRecomputeAndModifyTest],
    classOf[OrganizeImportsTest],
    classOf[OrganizeImportsWildcardsTest],
    classOf[OrganizeMissingImportsTest],
    classOf[PimpedTreesTest],
    classOf[PrependOrDropScalaPackageFromRecomputedTest],
    classOf[PrependOrDropScalaPackageKeepTest],
    classOf[PrettyPrinterTest],
    classOf[RenameTest],
    classOf[SelectionsTest],
    classOf[SourceGenTest],
    classOf[SourceHelperTest],
    classOf[SplitParameterListsTest],
    classOf[TreeAnalysisTest],
    classOf[TreeChangesDiscovererTest],
    classOf[TreeTransformationsTest],
    classOf[UnusedImportsFinderTest]))
class RefactoringTestSuite {}
