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
