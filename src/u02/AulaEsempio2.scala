package u02

object AulaEsempio2 extends App:

  case class Pair[A, B](fst: A, snd: B)

  def reverse[A, B](p: Pair[A, B]): Pair[B, A] = p match
    case Pair(f, s) => Pair(s, f)

  println(reverse(Pair(10, 20)))
