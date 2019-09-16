package Parser

import Num._

object Arithmetic {
  type Prog[A] = () => A

  sealed trait Expr {
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

    def parse(s: String): Expr = ???
  }

  final case class Number(i: Num) extends Expr
  final case class Add(l: Expr, r: Expr) extends Expr
  final case class Multiply(l: Expr, r: Expr) extends Expr
  final case class If(cond: Expr, thenDo: Expr, elseDo: Expr) extends Expr
}
