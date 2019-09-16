package Parser

object Arithmetic {
  type Prog[A] = () => A

  sealed trait Expr {
    def eval: Int = this match {
      case Number(i) => i
      case Add(l, r) => l.eval + r.eval
      case Multiply(l, r) => l.eval * r.eval
      case If(cond, thenDo, elseDo) =>
        if (cond.eval > 0) thenDo.eval else elseDo.eval
    }

    def compile: Prog[Int] = this match {
      case Number(i) => () => i
      case Add(l, r) =>
        val lComp = l.compile
        val rComp = r.compile
        () => lComp() + rComp()
      case Multiply(l, r) =>
        val lComp = l.compile
        val rComp = r.compile
        () => lComp() * rComp()
      case If(cond, thenDo, elseDo) =>
        val condComp = cond.compile
        val thenDoComp = thenDo.compile
        val elseDoComp = elseDo.compile
        () => if (condComp() > 0) thenDoComp() else elseDoComp()
    }

    def repr: String = this match {
      case Number(i) => i.toString
      case Add(l, r) => s"(${l.repr} + ${r.repr})"
      case Multiply(l, r) => s"(${l.repr} * ${r.repr})"
    }
  }

  object Expr {
    def parse(s: String): Expr = ???
  }

  final case class Number(i: Int) extends Expr
  final case class Add(l: Expr, r: Expr) extends Expr
  final case class Multiply(l: Expr, r: Expr) extends Expr
  //if expr thn els
  final case class If(cond: Expr, thenDo: Expr, elseDo: Expr) extends Expr
}
