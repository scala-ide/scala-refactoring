package scala.tools.refactoring.tests.regeneration

import scala.tools.refactoring.tests.util.TestHelper
import junit.framework.TestCase
import org.junit.Test

@Test
class PartitionerImportsTest extends TestHelper {

  @Test
  def singleImport =
    """
    import scala.collection.mutable.ArrayBuffer
    """	partitionsInto
    """→0(0)❨|
    import |scala|.|collection|.|mutable|.|ArrayBuffer|
    |❩"""
    
  @Test
  def multipleImports =
    """
    import scala.collection.mutable.{ArrayBuffer, MutableList}
    """ partitionsInto
    """→0(0)❨|
    import |scala|.|collection|.|mutable|.{|ArrayBuffer, MutableList|}
    |❩"""
    
  @Test
  def importPackage =
    """
    import scala.collection.mutable._
    """ partitionsInto
    """→0(0)❨|
    import |scala|.|collection|.|mutable|.|_|
    |❩"""
        
  @Test
  def renameImport =
    """
    import scala.collection.mutable.{ArrayBuffer => Array, _}
    """ partitionsInto
    """→0(0)❨|
    import |scala|.|collection|.|mutable|.{|ArrayBuffer => Array, _|}
    |❩"""
            
  @Test
  def hideImport =
    """
    import scala.collection.mutable.{ArrayBuffer => _, _}
    """ partitionsInto
    """→0(0)❨|
    import |scala|.|collection|.|mutable|.{|ArrayBuffer => _, _|}
    |❩"""
    
}