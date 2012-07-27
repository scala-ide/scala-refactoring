package scala.tools.refactoring.tests.implementations

import org.junit.runner.RunWith
import org.junit.runners.Suite

@RunWith(value = classOf[Suite])
@Suite.SuiteClasses(value = Array(
    classOf[ChangeParamOrderTest],
    classOf[MergeParameterListsTest],
    classOf[SplitParameterListsTest]))
class MethodSignatureRefactoringsSuite {

}