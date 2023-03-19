package u02

import HomeExercises.*
import junit.framework.TestCase.assertTrue
import org.junit.Assert.{assertEquals, assertFalse}
import org.junit.Test

class HomeExercisesTest:
  //Exercise 3
  //Point a

  //Method
  @Test
  def testPositiveMethod() =
    assertEquals("positive", positiveMethod(5))
    assertEquals("negative", positiveMethod(-5))
    assertEquals("positive", positiveMethod(0))

  //Lambda
  @Test
  def testPositiveLambda() =
    assertEquals("positive", positiveLambda(5))
    assertEquals("negative", positiveLambda(-5))
    assertEquals("positive", positiveLambda(0))


  //Point b
  //Lambda
  @Test
  def testNegativeLambda() =
    assertTrue("positive", notEmptyLambda("fool"))
    assertFalse("negative", notEmptyLambda(""))
    assertTrue("positive", notEmptyLambda("foo") && !notEmptyLambda(""))

  //Method
  @Test
  def testNegativeMethod() =
    assertTrue("positive", notEmptyFunction("fool"))
    assertFalse("negative", notEmptyFunction(""))
    assertTrue("positive", notEmptyFunction("foo") && !notEmptyFunction(""))