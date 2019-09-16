package Parser

import org.scalatest.{FlatSpec, Matchers}
import Num._

class NumSuite extends FlatSpec with Matchers{
  "gt" should "say one is greater than Zero" in {
    gt(one, Zero) shouldBe true
  }

  it should "say zero is greater than negOne" in {
    gt(Zero, negOne) shouldBe true
  }

  it should "say zero is not greater than zero" in {
    gt(Zero, Zero) shouldBe false
  }

  it should "say two is greater than one" in {
    gt(Next(one), one) shouldBe true
  }

  it should "say negOne is greater than negTwo" in {
    gt(negOne, Prev(negOne)) shouldBe true
  }

  it should "say one is greater than negOne" in {
    gt(one, negOne) shouldBe true
  }
}
