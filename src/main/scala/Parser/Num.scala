package Parser

import scala.annotation.tailrec

sealed trait Num
case object Zero extends Num
final case class Next(before: Num) extends Num
final case class Prev(after: Num) extends Num

object Num {
  val one: Num = Next(Zero)
  val negOne: Num = Prev(Zero)

  def inv(n: Num): Num = {
    @tailrec
    def looper(toInv: Num, res: Num): Num = toInv match {
      case Zero => res
      case Next(b) => looper(b, Prev(res))
      case Prev(a) => looper(a, Next(res))
    }
    looper(n, Zero)
  }

  @tailrec
  def add(l: Num, r: Num): Num = l match {
    case Zero => r
    case Next(b1) => r match {
      case Zero => l
      case Next(_) => add(b1, Next(r))
      case Prev(a1) => add(b1, a1)
    }
    case Prev(a1) => r match {
      case Zero => l
      case Next(b1) => add(a1, b1)
      case Prev(_) => add(a1, Prev(r))
    }
  }

  def multiply(l: Num, r: Num): Num = {
    @tailrec
    def times(t: Num, u: Num, acc: Num): Num = t match {
      case Zero => acc
      case Next(b) => times(b, u, add(acc, u))
      case Prev(a) => times(a, u, add(acc, inv(u)))
    }
    times(l, r, Zero)
  }

  @tailrec
  def gt(l: Num, r: Num): Boolean = l match {
    case Zero => r match {
      case Prev(_) => true
      case _ => false
    }
    case Next(b1) => r match {
      case Next(b2) => gt(b1, b2)
      case _ => true
    }
    case Prev(a1) => r match {
      case Prev(a2) => gt(a1, a2)
      case _ => false
    }
  }
}