package u02

object Esercizio2a extends App:
  //Exercise a

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


  //Exercise b
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


  //Exercise c
  def negGen[X](f: (X => Boolean)): (X => Boolean) =
    s => !f(s)

  val notEmptyGen = negGen(empty)

  println("NegGen " + notEmptyGen("fool"))
  println("NegGen " + notEmptyGen(""))
  print("NegGen ")
  println(notEmptyGen("foo") && !notEmptyGen(""))
  println()

  //Exercise2b
  val p1: Int => Int => Int => Boolean = x => y => z => x <= y && x <= z && y == z

  println(p1(1)(2)(2))

  //val p2:
  //def p3()
  //def p4()



