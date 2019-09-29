package Parser

import org.scalatest.{FlatSpec, Matchers}
import Arithmetic.Parser._
import Arithmetic.Result._
import Arithmetic.Expr._
import Arithmetic.Scope._

class ArithmeticSuite extends FlatSpec with Matchers{
  "digitParser" should "parse 0" in {
    digitParser.parse("0") shouldBe Success('0', "")
  }

  it should "parse 7 and return the remaining" in {
    digitParser.parse("7 is seven") shouldBe Success('7', " is seven")
  }

  it should "fail at hello5" in {
    digitParser.parse("hello5") shouldBe Failure
  }

  "plusExprParser" should "parse 1+2" in {
    plusExprParser.parse("1+2") shouldBe Success(Add(Number(Num(1)), Number(Num(2))), "")
  }

  it should "parse 1+(2+3)" in {
    plusExprParser.parse("1+(2+3)") shouldBe Success(Add(Number(Num(1)), Add(Number(Num(2)), Number(Num(3)))), "")
  }

  "multiplyExprParser" should "parse 5*6" in {
    multiplyExprParser.parse("5*6") shouldBe Success(Multiply(Number(Num(5)), Number(Num(6))), "")
  }

  it should "parse (1*2)*3" in {
    multiplyExprParser.parse("(1*2)*3") shouldBe Success(Multiply(Multiply(Number(Num(1)), Number(Num(2))), Number(Num(3))), "")
  }

  it should "parse (5+27)*3" in {
    multiplyExprParser.parse("(5+27)*3") shouldBe Success(Multiply(Add(Number(Num(5)), Number(Num(27))), Number(Num(3))), "")
  }

  "numberParser" should "parse 12" in {
    numberParser.parse("12") shouldBe Success(Number(Num(12)), "")
  }

  it should "parse 999" in {
    numberParser.parse("999") shouldBe Success(Number(Num(999)), "")
  }

  "Split" should "parse 123 and + sign" in {
    new Split(Pure(("123", "+")), numberParser, Text('+')).parse("") shouldBe Success((Number(Num(123)), '+'), "")
  }

  "At" should "parse 123+456 as (123, 456)" in {
    Sep('+').parse("123+456") shouldBe Success(("123", "456"), "")
  }

  it should "parse (123+456)+789 as ((123+456), 789)" in {
    Sep('+').parse("(123+456)+789") shouldBe Success(("(123+456)", "789"), "")
  }

  "exprParser" should "parse 1*2+3*4" in {
    exprParser.parse("1*2+3*4") shouldBe Success(
      Add(
        Multiply(
          Number(Num(1)),
          Number(Num(2))
        ),
        Multiply(
          Number(Num(3)),
          Number(Num(4))
        )
      ),
      ""
    )
  }

  it should "parse 1+(2+1)*3" in {
    exprParser.parse("1+(2+1)*3") shouldBe Success(
      Add(
        Number(Num(1)),
        Multiply(
          Add(
            Number(Num(2)),
            Number(Num(1))
          ),
          Number(Num(3))
        )
      ),
      ""
    )
  }

  "eval" should "limit scope when using ShadowScope" in {
    val expr =
      Assign("x", Number(Num(1))) block
        Assign("y",
          Assign("x", Number(Num(3))) block
            Add(Var("x"), Var("x"))
        ) block
        Add(Var("x"), Var("y"))

    eval(expr, emptyLocalScope)._1 shouldBe Num(7)
  }

  "evalEager" should "use a global scope when using GlobalScope" in {
    val expr =
      Assign("x", Number(Num(1))) block
        Assign("y",
          Assign("x", Number(Num(3))) block
            Add(Var("x"), Var("x"))
        ) block
        Add(Var("x"), Var("y"))

    evalEager(expr, emptyGlobalScope)._1 shouldBe Num(9)
  }
}
