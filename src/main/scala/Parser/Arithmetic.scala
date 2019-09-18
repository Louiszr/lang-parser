package Parser

import Num._

object Arithmetic {
  type Prog[A] = () => A

  sealed trait Expr {
    import Expr._
    def repr: String = this match {
      case Number(i) => i.toString
      case Add(l, r) => s"(${l.repr} + ${r.repr})"
      case Multiply(l, r) => s"(${l.repr} * ${r.repr})"
    }
  }

  object Expr {
    def eval(e: Expr): Num = e match {
      case Number(i) => i
      case Add(l, r) => add(eval(l), eval(r))
      case Multiply(l, r) => multiply(eval(l), eval(r))
      case If(cond, thenDo, elseDo) =>
        if (gt(eval(cond), Zero)) eval(thenDo) else eval(elseDo)
    }

    def compile(e: Expr): Prog[Num] = e match {
      case Number(i) => () => i
      case Add(l, r) =>
        val lComp = compile(l)
        val rComp = compile(r)
        () => add(lComp(), rComp())
      case Multiply(l, r) =>
        val lComp = compile(l)
        val rComp = compile(r)
        () => multiply(lComp(), rComp())
      case If(cond, thenDo, elseDo) =>
        val condComp = compile(cond)
        val thenDoComp = compile(thenDo)
        val elseDoComp = compile(elseDo)
        () => if (gt(condComp(), Zero)) thenDoComp() else elseDoComp()
    }
    final case class Number(i: Num) extends Expr
    final case class Add(l: Expr, r: Expr) extends Expr
    final case class Multiply(l: Expr, r: Expr) extends Expr
    final case class If(cond: Expr, thenDo: Expr, elseDo: Expr) extends Expr
  }


  sealed trait Result[+A]
  object Result {
    final case class Success[A](a: A, remaining: String) extends Result[A]
    case object Failure extends Result[Nothing]
  }


  sealed trait Parser[A] {
    import Parser._

    def parse(s: String): Result[A]
    def or(that: => Parser[A]): Parser[A] = new Or(this, that)
    def and[B](that: => Parser[B]): Parser[(A, B)] = new And(this, that)
    def map[B](f: A => B): Parser[B] = Map(this, f)
  }

  object Parser {
    import Expr._
    import Result._

    final case class Text(t: Char) extends Parser[Char] {
      override def parse(s: String): Result[Char] = {
        for {
          h <- s.headOption
          if h == t
        } yield Success(h, s.tail)
        }.getOrElse(Failure)
    }
    class Or[A](l: Parser[A], r: => Parser[A]) extends Parser[A] {
      override def parse(s: String): Result[A] = l.parse(s) match {
        case s @ Success(_, _) => s
        case Failure => r.parse(s)
      }
    }
    class And[A, B](l: Parser[A], r: => Parser[B]) extends Parser[(A, B)] {
      override def parse(s: String): Result[(A, B)] = l.parse(s) match {
        case Success(a, s1) => r.parse(s1) match {
          case Success(b, s2) => Success((a, b), s2)
          case _ => Failure
        }
        case _ => Failure
      }
    }
    final case class Map[A, B](p: Parser[A], f: A => B) extends Parser[B] {
      override def parse(s: String): Result[B] = p.parse(s) match {
        case Success(a, remaining) => Success(f(a), remaining)
        case _ => Failure
      }
    }

    val digitParser: Parser[Char] = Text('0') or Text('1') or Text('2') or Text('3') or Text('4') or Text('5') or Text('6') or Text('7') or Text('8') or Text('9')
    val smallNumberParser: Parser[Expr] = digitParser.map(i => Number(Num(i.toString.toInt)))
    def plusExprParser: Parser[Expr] = smallNumberParser and Text('+') and exprParser map { case ((l, _), r) => Add(l, r)}
    def multiplyExprParser: Parser[Expr] = smallNumberParser and Text('*') and exprParser map { case ((l, _), r) => Multiply(l, r)}
//    def parened: Parser[Expr] = Text('(') and exprParser and Text(')') map{ case ((_, e), _) => e}
    def exprParser: Parser[Expr] = multiplyExprParser or plusExprParser or smallNumberParser
    //
    val tensParser: Parser[Int] = digitParser.and(digitParser).map{ case (first, second) => (first.toString + second.toString).toInt }
  }
}
