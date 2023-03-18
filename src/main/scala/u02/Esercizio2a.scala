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
  enum Shape:
    case Rectangle(sideA: Double, sideB: Double)
    case Circle(diameter: Double)
    case Square(side: Double)