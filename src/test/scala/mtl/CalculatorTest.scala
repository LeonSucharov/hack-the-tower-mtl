package mtl

import org.scalatest._

/**
  * Created by lsucharov on 15/10/2016.
  */
class CalculatorTest extends FunSpec with Matchers {

  it("displays an empty screen at start"){
    val c: Calculator = new Calculator
    c.screen shouldBe ""
  }

  it("displays an a number when it is added"){
    val c: Calculator = new Calculator
    c.press(1).screen shouldBe "1"
  }

  it("displays an two numbers when it is added"){
    val c: Calculator = new Calculator
    c.press(1).press(2).screen shouldBe "12"
  }

  it("adds two numbers"){
    val c: Calculator = new Calculator
    c.press(1).plus().press(2).screen shouldBe "1+2"
  }

  it("adds two numbers and equals"){
    val c: Calculator = new Calculator
    c.press(1).plus().press(2).equals().screen shouldBe "3"
  }

}
