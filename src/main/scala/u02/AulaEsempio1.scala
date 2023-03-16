package u02


object AulaEsempio1 extends App:
  enum List:
    case Cons(head: Int, tail: List)
    case Nil

  object List:
    def size(list: List): Int = list match
      case Nil => 0
      case Cons(h, t) => 1 + size(t)

  import List.*

  println(size(Cons(10, Nil)))