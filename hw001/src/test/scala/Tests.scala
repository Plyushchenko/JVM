import org.scalatest._
import Calculator.InvalidExpressionException

class Tests extends FlatSpec with MustMatchers {
  "Calculator" should "calculate simple expressions" in {
    Calculator.Calculator.calculate("100") must equal (100)
    Calculator.Calculator.calculate("-100") must equal (-100)
    Calculator.Calculator.calculate("2 + 2 * 2") must equal (6)
    Calculator.Calculator.calculate("4*2^2") must equal (16)
    Calculator.Calculator.calculate("2^3^2") must equal (512)
    Calculator.Calculator.calculate("(2^3)^2") must equal (64)
    Calculator.Calculator.calculate("1000/20/10") must equal (5)
    Calculator.Calculator.calculate("1000/(20/10)") must equal (500)
  }
  "Calculator" should "calculate" in {
    Calculator.Calculator.calculate("-(pow(2, sin(pi))) + 5") must equal (4.0 +- 0.00001)
    Calculator.Calculator.calculate("" +
      "pow( pow(2, pow(2, 3)) , pow(2, pow(1 , pow(1, sin(0)))) )") must equal (65536)
    Calculator.Calculator.calculate("pi*sin(pi / 2)") must equal (math.Pi +- 0.00001)
  }

  "Calculator" should "throw exceptions" in {
    an[Calculator.InvalidExpressionException.type] should be thrownBy {
      Calculator.Calculator.calculate(")()(()")
    }
    an[Calculator.InvalidExpressionException.type] should be thrownBy {
      Calculator.Calculator.calculate("(1 + + 2)")
    }
    an[Calculator.InvalidExpressionException.type] should be thrownBy {
      Calculator.Calculator.calculate("(6 + 3 * 7 - 921-------2)")
    }
    an[Calculator.InvalidExpressionException.type] should be thrownBy {
      Calculator.Calculator.calculate("2 + (2 + (2 + ())))")
    }
    an[Calculator.InvalidExpressionException.type] should be thrownBy {
      Calculator.Calculator.calculate("pow(5, 6, 7)")
    }
    an[Calculator.UnknownTokenException.type] should be thrownBy {
      Calculator.Calculator.calculate("ln(18)")
    }
    an[Calculator.UnknownTokenException.type] should be thrownBy {
      Calculator.Calculator.calculate("hello, world[15]")
    }
  }
}