package u02

object Esercizio2a extends App:
  //Exercise 3
  //Point a

  //Lambda
  def positiveFun(x: Int): String = x match
    case x if x >= 0 => "positive"
    case x if x < 0 => "negative"

    println("PositiveFun " + positiveFun(5))
    println("PositiveFun " + positiveFun(-5))
    println("PositiveFun " + positiveFun(0))
    println()

  //Method
  val positiveLamb: Int => String = _ match
    case x if x >= 0 => "positive"
    case x if x < 0 => "negative"

  println("PositiveLamb " + positiveLamb(5))
  println("PositiveLamb " + positiveLamb(-5))
  println("PositiveLamb " + positiveLamb(0))
  println()


  //Point b
  val empty: (String) => (Boolean) = _ == ""
  val negLambda: (String => Boolean) => String => Boolean =
    f => s => !f(s)

  val notEmptyLambda = negLambda(empty)

  println("NegLamb " + notEmptyLambda("fool"))
  println("NegLamb " + notEmptyLambda(""))
  print("NegLamb ")
  println(notEmptyLambda("foo") && !notEmptyLambda(""))
  println()


  def negFun(f: (String => Boolean)): (String => Boolean) =
    s => !f(s)

  val notEmptyFun = negFun(empty)

  println("NegFun " + notEmptyFun("fool"))
  println("NegFun " + notEmptyFun(""))
  print("NegFun ")
  println(notEmptyFun("foo") && !notEmptyFun(""))
  println()


  //Point c
  def negGen[X](f: (X => Boolean)): (X => Boolean) =
    s => !f(s)

  val notEmptyGen = negGen(empty)

  println("NegGen " + notEmptyGen("fool"))
  println("NegGen " + notEmptyGen(""))
  print("NegGen ")
  println(notEmptyGen("foo") && !notEmptyGen(""))
  println()

  //Exercise4
  val p1: Int => Int => Int => Boolean = x => y => z => x <= y && x <= z && y == z
  println(p1(1)(2)(2))
  println(p1(1)(2)(3))
  println()

  val p2: (Int, Int, Int) => Boolean = (x, y, z) => x <= y && x <= z && y == z
  println(p2(1, 2, 2)) //true
  println(p2(1, 2, 3)) //false
  println()

  def p3(x: Int)(y: Int)(z: Int): Boolean = x <= y && x <= z && y == z
  val p3test = p3(1)(2)
  println(p3test(2)) //true
  println(p3test(3)) //false
  println()

  def p4(x: Int, y: Int, z: Int): Boolean = x <= y && x <= z && y == z
  println(p4(1, 2, 2)) //true
  println(p4(1, 2, 3)) //false
  println()

  //Exercise 5
  def compose(f: Int => Int, g: Int => Int): Int => Int = x => f(g(x))
  println(compose(_ - 1, _ * 2)(5)) // 9
  println()

  def genCompose[X](f: X => X, g: X => X): X => X = x => f(g(x))
  println(genCompose[Int](_ - 1, _ * 2)(5)) // 9
  println()

  //Exercise 6
  def gcd(a: Int, b: Int): Int = b != 0 && a > b match
    case true => gcd(b, a % b)
    case false => a

  println(gcd(12, 8)) //4
  println(gcd(14, 7)) //7
  println()


  @annotation.tailrec
  def gcdTail(a: Int, b: Int): Int = b != 0 && a > b match
    case false => a
    case true => gcdTail(b, a % b)

  println(gcdTail(12, 8)) //4
  println(gcdTail(14, 7)) //7
  println()

  //Exercise 7
  import scala.math._
  enum Shape:
    case Rectangle(sideA: Double, sideB: Double, topLeftCoord: (Double, Double))
    case Circle(radius: Double, center: (Double, Double))
    case Square(side: Double, topLeftCoord: (Double, Double))

  object shapeProperties:
    def perimeter(shape: Shape): Double = shape match
      case Shape.Rectangle(a, b, (x, y)) => (a * 2) + (b * 2)
      case Shape.Circle(r, (x, y)) => (r * 2) * 3.14
      case Shape.Square(a, (x, y)) => a * 4

    def contains(shape: Shape, point: (Double, Double)): Boolean = shape match
      case Shape.Rectangle(a, b, (x, y)) => (point._1 >= x) && (point._1 <= x + a) && (point._2 >= y) && (point._2 <= y + b)
      case Shape.Circle(r, (x, y)) => pow(point._1 - x, 2) + pow(point._2 - y, 2) <= pow(r, 2)
      case Shape.Square(a, (x, y)) => (point._1 >= x) && (point._1 <= x + a) && (point._2 >= y) && (point._2 <= y + a)

  import shapeProperties.*

  println(perimeter(Shape.Rectangle(12, 7, (0, 0)))) //38
  println(contains(Shape.Rectangle(12, 7, (0, 0)), (5, 4))) //true
  println(contains(Shape.Rectangle(2, 3, (0, 0)), (5, 4))) //false
  println()

  println(perimeter(Shape.Circle(17.5, (0, 0)))) //approx. 110
  println(contains(Shape.Circle(17.5, (0, 0)), (10, 8))) //true
  println(contains(Shape.Circle(17.5, (0, 0)), (20, 27))) //false
  println()

  println(perimeter(Shape.Square(4, (0, 0)))) //16
  println(contains(Shape.Square(4, (0, 0)), (3, 2))) //true
  println(contains(Shape.Square(4, (0, 0)), (3, 5))) //false
  println()


  //Exercise 8
  enum Option[A]:
    case Some(a: A)
    case None() // here parens are needed because of genericity

  object Option :
    def isEmpty[A](opt: Option[A]): Boolean = opt match
      case None() => true
      case _ => false

    def orElse[A, B >: A](opt: Option[A], orElse: B): B = opt match
      case Some(a) => a
      case _ => orElse

    def flatMap[A, B](opt: Option[A]) (f: A => Option [B]): Option [B] = opt match
      case Some(a) => f(a)
      case _ => None()

    def filter[A](opt: Option[A]) (f: A => Boolean): Option[A] = opt match
      case Some(a) => if f(a) then Some(a) else None()
      case None() => None()

    def map[A](opt: Option[A])(f: A => Boolean): Option[Boolean] = opt match
      case Some(a) => if f(a) then Some(true) else Some(false)
      case None() => None()

    def fold[A](opt: Option[A])(value: A)(f: A => A): A = opt match
      case Some(a) => f(a)
      case None() => value


  import Option .*

  println(filter(Some(5))(_ > 2)) //Some(5)
  println(filter(Some(5))(_ > 8)) //None
  println(filter(None[Int]())(_ > 2)) //None
  println()

  println(map(Some(5))(_ > 2)) //Some(true)
  println(map(Some(5))(_ > 8)) //Some(false
  println(map(None[Int]())(_ > 2)) //None
  println()

  println(fold(Some(5))(1)(_ + 1)) // 6
  println(fold(None[Int]())(1)(_ + 1)) // 1
  println()