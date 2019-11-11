package Parser

import Parser.Arithmetic.Expr._
import Parser.Arithmetic.Parser._
import Parser.Arithmetic.Result._
import Parser.Arithmetic.Scope._
import org.scalatest.{FlatSpec, Matchers}

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

  "eval" should "limit scope when using LocalScope" in {
    val expr =
      Assign("x", Number(Num(1))) block
        Assign("y",
          Assign("x", Number(Num(3))) block
            Add(Var("x"), Var("x"))
        ) block
        Add(Var("x"), Var("y"))

    eval(expr, emptyLocalScope)._1 shouldBe Some(Num(7))
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

  "Apply" should "apply lambda x: x + 1 with x = 2" in {
    val expr =
      Apply(
        Function1(
          "x",
          Add(Var("x"), Number(Num(1)))
        ),
        Number(Num(2))
      )
    eval(expr, emptyLocalScope)._1 shouldBe Some(Num(3))
  }

  it should "apply f with x = 2 if f = lambda x: x + 1" in {
    val expr =
      Assign("f", Function1("x", Add(Var("x"), Number(Num(1))))) block
        Apply(Var("f"), Number(Num(2)))
    eval(expr, emptyLocalScope)._1 shouldBe Some(Num(3))
  }

  it should "not apply y with x = 2 if y = 3" in {
    val expr =
      Assign("f", Number(Num(3))) block
        Apply(Var("f"), Number(Num(2)))
    eval(expr, emptyLocalScope)._1 shouldBe None
  }

  it should "apply lambda x, y: x + y with x = 1, y = 2" in {
    val func =
      Function2(
        "x",
        "y",
        Add(Var("x"), Var("y"))
      )
    val expr =
      Apply(
        Apply(
          func,
          Number(Num(1))
        ),
        Number(Num(2))
      )
    eval(expr, emptyLocalScope)._1 shouldBe Some(Num(3))
  }

  it should "apply lambda x, y, z: x + y * z with x = 1, y = 2, z = 3" in {
    val func =
      Function3(
        "x",
        "y",
        "z",
        Add(Var("x"), Multiply(Var("y"), Var("z")))
      )
    val expr =
      Apply(
        Apply(
          Apply(
            func,
            Number(Num(1))
          ),
          Number(Num(2))
        ),
        Number(Num(3))
      )
    eval(expr, emptyLocalScope)._1 shouldBe Some(Num(7))
  }

  it should "do recursion" in {
    val func =
      Function1(
        "x",
        // if x > 0, return x else return f(x + 1)
        If(Var("x"), Var("x"), Apply(Var("f"), Add(Var("x"), Number(Num(1)))))
      )
    val expr =
      Assign("f", func) block
        Apply(Var("f"), Number(Num(-2)))
    eval(expr, emptyLocalScope)._1 shouldBe Some(Num(1))
  }

  it should "do higher-order function" in {
    val addOne =
      Function1(
        "x",
        Add(Var("x"), Number(Num(1)))
      )
    val modifyNumber =
      Function2(
        "x",
        "f",
        Apply(Var("f"), Var("x"))
      )
    val expr =
      Apply(
        Apply(
          modifyNumber,
          Number(Num(5))
        ),
        addOne
      )
    eval(expr, emptyLocalScope)._1 shouldBe Some(Num(6))
  }

  it should "do fibonacci" in {
    val fib =
      Function1(
        "x",
        If(
          Var("x"), // if x > 0
          If(
            Add(Var("x"), Number(Num(-1))), // if x > 1
            Add(
              Apply(Var("fib"), Add(Var("x"), Number(Num(-1)))),
              Apply(Var("fib"), Add(Var("x"), Number(Num(-2))))
            ), // fib(x - 1) + fib(x - 2)
            Number(Num(1)) // else when x == 1, return 1
          ),
          Number(Zero) // else when x <= 0, return 0
        )
      )
    val expr =
      Assign("fib", fib) block
        Apply(Var("fib"), Number(Num(20)))
    eval(expr, emptyLocalScope)._1 shouldBe Some(Num(6765))
  }

  it should "apply lambda f, x, y: f(x) + y with f = x * x, x = 2, y = 3" in {
    val func =
      Function3(
        "f",
        "x",
        "y",
        Add(Apply(Var("f"), Var("x")), Var("y"))
      )
    val expr =
      Apply(
        Apply(
          Apply(
            func,
            Function1("x", Multiply(Var("x"), Var("x")))
          ),
          Number(Num(2))
        ),
        Number(Num(3))
      )
    eval(expr, emptyLocalScope)._1 shouldBe Some(Num(7))
  }
}
