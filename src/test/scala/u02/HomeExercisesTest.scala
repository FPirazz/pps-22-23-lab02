package u02

import HomeExercises.*
import junit.framework.TestCase.assertTrue
import org.junit.Assert.{assertEquals, assertFalse}
import org.junit.Test

class HomeExercisesTest:

  //Class to test all functions

  //Exercise 3
  //Point a
  @Test
  def testPositiveFunction() =
    assertEquals("positive", positiveFunction(5))
    assertEquals("negative", positiveFunction(-5))
    assertEquals("positive", positiveFunction(0))

