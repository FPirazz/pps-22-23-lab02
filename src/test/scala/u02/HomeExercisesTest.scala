package u02

import HomeExercises.*
import HomeExercises.shapeProperties.*
import HomeExercises.Option.*
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
    assertTrue(notEmptyLambda("fool"))
    assertFalse(notEmptyLambda(""))
    assertTrue(notEmptyLambda("foo") && !notEmptyLambda(""))

  //Method
  @Test
  def testNegativeMethod() =
    assertTrue(notEmptyFunction("fool"))
    assertFalse(notEmptyFunction(""))
    assertTrue(notEmptyFunction("foo") && !notEmptyFunction(""))


  //Point c
  @Test
  def testNegativeGeneric() =
    assertTrue(notEmptyGeneric("fool"))
    assertFalse(notEmptyGeneric(""))
    assertTrue(notEmptyGeneric("foo") && !notEmptyGeneric(""))


  //Exercise 4
  @Test
  def testP1() =
    assertTrue(p1(1)(2)(2))
    assertFalse(p1(1)(2)(3))
  @Test
  def testP2() =
    assertTrue(p2(1, 2, 2))
    assertFalse(p2(1, 2, 3))
  @Test
  def testP3() =
    val p3test = p3(1)(2)
    assertTrue(p3test(2))
    assertFalse(p3test(3))
  @Test
  def testP4() =
    assertTrue(p4(1, 2, 2))
    assertFalse(p4(1, 2, 3))


  //Exercise 5
  @Test
  def testCompose() =
    assertEquals(9, compose(_ - 1, _ * 2)(5))
  @Test
  def testGenericCompose() =
    assertEquals(9, genericCompose[Int](_ - 1, _ * 2)(5))


  //Exercise 6
  @Test
  def testGCD() =
    assertEquals(4, gcd(12, 8))
    assertEquals(7, gcd(14, 7))
  @Test
  def testTailGCD() =
    assertEquals(4, gcdTail(12, 8))
    assertEquals(7, gcdTail(14, 7))


  //Exercise 7
  @Test
  def testRectangleProperties() =
    assertEquals(38, perimeter(Shape.Rectangle(12, 7, (0, 0))).round)
    assertTrue(contains(Shape.Rectangle(12, 7, (0, 0)), (5, 4)))
    assertFalse(contains(Shape.Rectangle(2, 3, (0, 0)), (5, 4)))
  @Test
  def testCircleProperties() =
    assertEquals(110, perimeter(Shape.Circle(17.5, (0, 0))).round)
    assertTrue(contains(Shape.Circle(17.5, (0, 0)), (10, 8)))
    assertFalse(contains(Shape.Circle(17.5, (0, 0)), (20, 27)))
  @Test
  def testSquareProperties() =
    assertEquals(16, perimeter(Shape.Square(4, (0, 0))).round)
    assertTrue(contains(Shape.Square(4, (0, 0)), (3, 2)))
    assertFalse(contains(Shape.Square(4, (0, 0)), (3, 5)))


  //Exercise 8
  @Test
  def testFilter() =
    assertEquals(Some(5), filter(Some(5))(_ > 2))
    assertEquals(None(), filter(Some(5))(_ > 8))
    assertEquals(None(), filter(None[Int]())(_ > 2))
  @Test
  def testMap() =
    assertEquals(Some(true), map(Some(5))(_ > 2))
    assertEquals(Some(false), map(Some(5))(_ > 8))
    assertEquals(None(), map(None[Int]())(_ > 2))
  @Test
  def testFold() =
    assertEquals(6, fold(Some(5))(1)(_ + 1))
    assertEquals(1, fold(None[Int]())(1)(_ + 1))
