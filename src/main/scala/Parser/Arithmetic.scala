package Parser

import Num._

import scala.annotation.tailrec

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
    def |(that: => Parser[A]): Parser[A] = new Or(this, that)
    def ~[B](that: => Parser[B]): Parser[(A, B)] = new And(this, that)
    def map[B](f: A => B): Parser[B] = Map(this, f)
    def rep: Parser[Seq[A]] = Rep(this)
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
    final case class Rep[A](p: Parser[A]) extends Parser[Seq[A]] {
      override def parse(s: String): Result[Seq[A]] = {
        @tailrec
        def looper(acc: Seq[A], currStr: String): (Seq[A], String) = p.parse(currStr) match {
          case Success(a, remaining) => looper(a +: acc, remaining)
          case Failure => (acc, currStr)
        }
        looper(Seq(), s) match {
          case (Seq(), _) => Failure
          case (as, str) => Success(as.reverse, str)
        }
      }
    }

    val digitParser: Parser[Char] = Text('0') | Text('1') | Text('2') | Text('3') | Text('4') | Text('5') | Text('6') | Text('7') | Text('8') | Text('9')
    val smallNumberParser: Parser[Expr] = digitParser.map(i => Number(Num(i.toString.toInt)))
    val numberParser: Parser[Expr] = digitParser.rep.map(chars => Number(Num(chars.map(_.toString).reduce(_ + _).toInt)))
    def plusExprParser: Parser[Expr] = (parened | numberParser) ~ Text('+') ~ (parened | numberParser) map { case ((l, _), r) => Add(l, r)}
    def multiplyExprParser: Parser[Expr] = (parened | numberParser) ~ Text('*') ~ (parened | numberParser) map { case ((l, _), r) => Multiply(l, r)}
    def parened: Parser[Expr] = Text('(') ~ exprParser ~ Text(')') map{ case ((_, e), _) => e}
    def exprParser: Parser[Expr] = multiplyExprParser | plusExprParser | numberParser
  }
}
