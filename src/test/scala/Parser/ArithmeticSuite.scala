package Parser

import org.scalatest.{FlatSpec, Matchers}
import Arithmetic.Parser._
import Arithmetic.Result._
import Arithmetic.Expr._

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

  "tensParser" should "parse 20" in {
    tensParser.parse("20") shouldBe Success(20, "")
  }

  "plusExprParser" should "parse 1+2" in {
    plusExprParser.parse("1+2") shouldBe Success(Add(Number(Num(1)), Number(Num(2))), "")
  }

  it should "parse 1+2+3" in {
    plusExprParser.parse("1+2+3") shouldBe Success(Add(Number(Num(1)), Add(Number(Num(2)), Number(Num(3)))), "")
  }

  "multiplyExprParser" should "parse 5*6" in {
    multiplyExprParser.parse("5*6") shouldBe Success(Multiply(Number(Num(5)), Number(Num(6))), "")
  }

  it should "parse 1*2*3" in {
    multiplyExprParser.parse("1*2*3") shouldBe Success(Multiply(Number(Num(1)), Multiply(Number(Num(2)), Number(Num(3)))), "")
  }
}
