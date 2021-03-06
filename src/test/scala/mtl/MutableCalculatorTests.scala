package mtl

import org.scalatest._

class MutableCalculatorTests extends FunSpec with Matchers {
  it("displays 5 after 3+2=") {
    val c: Calculator = new MutableCalculator()
    val r = c.press(3).plus().press(2).equals().screen
    r shouldBe "5"
  }
  it("displays an empty string on start") {
    val c: Calculator = new MutableCalculator()
    c.screen shouldBe ""
  }
  it("displays the result of a calculation on typing =") {
    val c: Calculator = new MutableCalculator()
    c.press(2).plus().screen shouldBe "2+"
  }
  it("displays an empty string on clear") {
    val c: Calculator = new MutableCalculator()
    val r = c.press(3).plus().press(2).clear().screen
    r shouldBe ""
  }

  it("displays ERROR on typing a non-symbolic character")(pending)
  it("displays ERROR on typing two operators twice")(pending)
}
