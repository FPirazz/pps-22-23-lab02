package u02

object HomeExercises extends App:
  //Exercise 3
  //Point a

  //Method
  def positiveMethod(x: Int): String = x match
    case x if x >= 0 => "positive"
    case x if x < 0 => "negative"

  //Method
  val positiveLambda: Int => String = _ match
    case x if x >= 0 => "positive"
    case x if x < 0 => "negative"


  //Point b
  val empty: (String) => (Boolean) = _ == ""

  //Lambda
  val negativeLambda: (String => Boolean) => String => Boolean =
    f => s => !f(s)
  val notEmptyLambda = negativeLambda(empty)

  //Method
  def negativeMethod(f: (String => Boolean)): (String => Boolean) =
    s => !f(s)
  val notEmptyFunction = negativeMethod(empty)


  //Point c
  def negativeGeneric[X](f: (X => Boolean)): (X => Boolean) =
    s => !f(s)
  val notEmptyGeneric = negativeGeneric(empty)


  //Exercise4
  val p1: Int => Int => Int => Boolean = x => y => z => x <= y && x <= z && y == z
  val p2: (Int, Int, Int) => Boolean = (x, y, z) => x <= y && x <= z && y == z
  def p3(x: Int)(y: Int)(z: Int): Boolean = x <= y && x <= z && y == z
  def p4(x: Int, y: Int, z: Int): Boolean = x <= y && x <= z && y == z


  //Exercise 5
  def compose(f: Int => Int, g: Int => Int): Int => Int = x => f(g(x))
  def genericCompose[X](f: X => X, g: X => X): X => X = x => f(g(x))


  //Exercise 6
  def gcd(a: Int, b: Int): Int = b != 0 && a > b match
    case true => gcd(b, a % b)
    case false => a
  @annotation.tailrec
  def gcdTail(a: Int, b: Int): Int = b != 0 && a > b match
    case false => a
    case true => gcdTail(b, a % b)


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


  //Exercise 8
  enum Option[A]:
    case Some(a: A)
    case None() // here parens are needed because of genericity
  object Option:
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