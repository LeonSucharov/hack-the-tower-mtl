package mtl

import org.scalatest._

class FunCalculatorTests extends FunSpec with Matchers {

  it("displays an empty screen at start"){
    val c: Calculator = new CalculatorDelegate
    c.screen shouldBe "0"
  }

  it("displays an a number when it is added"){
    val c: Calculator = new CalculatorDelegate
    c.press(1).screen shouldBe "1"
  }

  it("displays an two numbers when it is added"){
    val c: Calculator = new CalculatorDelegate
    c.press(1).press(2).screen shouldBe "12"
  }

  it("adds two numbers"){
    val c: Calculator = new CalculatorDelegate
    c.press(1).plus().press(2).screen shouldBe "2"
  }
  it("adds two numbers and equals"){
    val c: Calculator = new CalculatorDelegate
    c.press(1).plus().press(2).equals().screen shouldBe "3"
  }
  it("displays the result of a calculation on typing =") {
    val c: Calculator = new CalculatorDelegate
    val r = c.press(3).plus().press(2).equals().screen
    r shouldBe "5"
  }
  it("displays the result of a calculation on adding an operator") {
    val c: Calculator = new CalculatorDelegate
    c.press(2).plus().screen shouldBe "0"
  }
  it("displays an empty string on clear") {
    val c: Calculator = new CalculatorDelegate
    val r = c.press(3).plus().press(2).clear().screen
    r shouldBe "0"
  }
  it("123+21=144") {
    val c: Calculator = new CalculatorDelegate
    val r = c.press(1).press(2).press(3).plus().press(2).press(1).equals().screen
    r shouldBe "144"
  }
  it("123+21-11=133") {
    val c: Calculator = new CalculatorDelegate
    val r = c.press(1).press(2).press(3).plus().press(2).press(1).minus().press(1).press(1).equals().screen
    r shouldBe "133"
  }

  it("displays ERROR on typing a non-symbolic character")(pending)
  it("displays ERROR on typing two operators twice")(pending)
}
