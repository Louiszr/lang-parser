package Parser

import Num._

import scala.annotation.tailrec

object Arithmetic {
  type Prog[A] = () => A

  sealed trait Expr {

    import Expr._

    def block(that: Expr) = Block(this, that)
  }

  object Expr {
    def eval(e: Expr, scope: Scope): (Option[Num], Scope) = e match {
      // lazy local
      case Number(i) => (Some(i), scope)
      case Add(l, r) =>
        val res = for {
          lVal <- eval(l, scope.passed)._1
          rVal <- eval(r, scope.passed)._1
        } yield add(lVal, rVal)
        (res, scope)
      case Multiply(l, r) =>
        val res = for {
          lVal <- eval(l, scope.passed)._1
          rVal <- eval(r, scope.passed)._1
        } yield multiply(lVal, rVal)
        (res, scope)
      case If(cond, thenDo, elseDo) =>
        val res = for {
          condVal <- eval(cond, scope.passed)._1
        } yield gt(condVal, Zero)
        res.fold[(Option[Num], Scope)]((None, scope))(b => if (b) eval(thenDo, scope.passed) else eval(elseDo, scope.passed))
      case Assign(n, v) => (None, scope.define(n, v))
      case Var(n) => scope.getOption(n)
        .map(expr => eval(expr, scope.passed))
        .getOrElse((None, scope))
      case Block(first, second) =>
        val (_, scope1) = eval(first, scope.passed)
        eval(second, scope1)
    }

    def evalEager(e: Expr, scope: Scope): (Num, Scope) = e match {
      // eager global
      case Number(i) => (i, scope)
      case Add(l, r) =>
        val (lVal, scope1) = evalEager(l, scope.passed)
        val (rVal, scope2) = evalEager(r, scope1)
        (add(lVal, rVal), scope2)
      case Multiply(l, r) =>
        val (lVal, scope1) = evalEager(l, scope.passed)
        val (rVal, scope2) = evalEager(r, scope1)
        (multiply(lVal, rVal), scope2)
      case If(cond, thenDo, elseDo) =>
        val (condVal, scope1) = evalEager(cond, scope.passed)
        if (gt(condVal, Zero)) evalEager(thenDo, scope1) else evalEager(elseDo, scope1)
      case Assign(n, v) =>
        val (num, scope1) = evalEager(v, scope.passed)
        (Zero, scope1.define(n, Number(num)))
      case Var(n) => scope.getOption(n)
        .map(expr => evalEager(expr, scope.passed))
        .getOrElse((Zero, scope))
      case Block(first, second) =>
        val (_, scope1) = evalEager(first, scope.passed)
        evalEager(second, scope1)
    }

    //    def compile(e: Expr): Prog[Num] = e match {
    //      // Match not exhaustive
    //      case Number(i) => () => i
    //      case Add(l, r) =>
    //        val lComp = compile(l)
    //        val rComp = compile(r)
    //        () => add(lComp(), rComp())
    //      case Multiply(l, r) =>
    //        val lComp = compile(l)
    //        val rComp = compile(r)
    //        () => multiply(lComp(), rComp())
    //      case If(cond, thenDo, elseDo) =>
    //        val condComp = compile(cond)
    //        val thenDoComp = compile(thenDo)
    //        val elseDoComp = compile(elseDo)
    //        () => if (gt(condComp(), Zero)) thenDoComp() else elseDoComp()
    //    }
    final case class Number(i: Num) extends Expr

    final case class Add(l: Expr, r: Expr) extends Expr

    final case class Multiply(l: Expr, r: Expr) extends Expr

    final case class If(cond: Expr, thenDo: Expr, elseDo: Expr) extends Expr

    final case class Assign(n: String, v: Expr) extends Expr

    final case class Var(n: String) extends Expr

    final case class Block(first: Expr, second: Expr) extends Expr

  }

  sealed trait Scope {
    def register: Map[String, Expr]

    def getOption(k: String): Option[Expr]

    def define(k: String, v: Expr): Scope

    def passed: Scope
  }

  object Scope {
    val emptyLocalScope: LocalScope = LocalScope(Map.empty[String, Expr], None)

    val emptyGlobalScope: GlobalScope = GlobalScope(Map.empty[String, Expr])

    final case class LocalScope(register: Map[String, Expr], calledFrom: Option[LocalScope]) extends Scope {
      override def getOption(k: String): Option[Expr] = register.get(k).orElse(calledFrom.flatMap(_.getOption(k)))

      override def define(k: String, v: Expr): Scope = this.copy(register = this.register + (k -> v))

      override def passed: Scope = LocalScope(Map.empty[String, Expr], Some(this))
    }

    final case class GlobalScope(register: Map[String, Expr]) extends Scope {
      override def getOption(k: String): Option[Expr] = register.get(k)

      override def define(k: String, v: Expr): Scope = this.copy(register = this.register + (k -> v))

      override def passed: Scope = this
    }

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

    def map[B](f: A => B): Parser[B] = flatMap(a => Pure(f(a)))

    def rep: Parser[Seq[A]] = Rep(this)

    def flatMap[B](f: A => Parser[B]): Parser[B] = FlatMap(this, f)
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
        case s@Success(_, _) => s
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

    final case class FlatMap[A, B](p: Parser[A], f: A => Parser[B]) extends Parser[B] {
      override def parse(s: String): Result[B] = p.parse(s) match {
        case Success(a, remaining) => f(a).parse(remaining)
        case Failure => Failure
      }
    }

    final case class Set(v: String) extends Parser[Unit] {
      override def parse(s: String): Result[Unit] = Success((), v)
    }

    final case class Pure[A](res: A) extends Parser[A] {
      override def parse(s: String): Result[A] = Success(res, s)
    }

    class Split[A, B](at: Parser[(String, String)], parseBefore: => Parser[A], parseAfter: => Parser[B]) extends Parser[(A, B)] {
      override def parse(s: String): Result[(A, B)] = {
        for {
          halves <- at // res: (b, a) remaining: ""
          _ <- Set(halves._1)
          resBefore <- parseBefore // res: resBefore remaining: ""
          _ <- Set(halves._2)
          resAfter <- parseAfter // res: resAfter remaining ""
        } yield (resBefore, resAfter)
        }.parse(s)
    }

    final case class Sep(t: Char) extends Parser[(String, String)] {
      def isParenClosed(str: String): Boolean = {
        @tailrec
        def looper(remaining: String, accParenCount: Int): Int =
          if (remaining.isEmpty) accParenCount
          else if (remaining.head == '(') looper(remaining.tail, accParenCount + 1)
          else if (remaining.head == ')') looper(remaining.tail, accParenCount - 1)
          else looper(remaining.tail, accParenCount)

        looper(str, 0) == 0
      }

      override def parse(s: String): Result[(String, String)] = {
        @tailrec
        def cut(i: Int): Result[(String, String)] = if (i >= s.length) Failure else {
          val indOfT = s.indexOf(t.toString, i)
          val (head, tail) = s.splitAt(indOfT)
          if (!isParenClosed(head)) cut(head.length + 1) else Success((head, tail.tail), "")
        }

        cut(0)
      }
    }

    val digitParser: Parser[Char] = Text('0') | Text('1') | Text('2') | Text('3') | Text('4') | Text('5') | Text('6') | Text('7') | Text('8') | Text('9')
    val numberParser: Parser[Expr] = digitParser.rep.map(chars => Number(Num(chars.map(_.toString).reduce(_ + _).toInt)))

    def plusExprParser: Parser[Expr] = new Split(Sep('+'), exprParser, exprParser) map { case (l, r) => Add(l, r) }

    def multiplyExprParser: Parser[Expr] = new Split(Sep('*'), exprParser, exprParser) map { case (l, r) => Multiply(l, r) }

    def parened: Parser[Expr] = Text('(') ~ exprParser ~ Text(')') map { case ((_, e), _) => e }

    def exprParser: Parser[Expr] = plusExprParser | multiplyExprParser | parened | numberParser
  }

}
