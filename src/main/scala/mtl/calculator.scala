package mtl

sealed trait Symbol
case class Number(i: Int) extends Symbol
sealed trait Op extends Symbol
case object Plus extends Op
case object Minus extends Op
case object Equals extends Symbol

object FunCalculator{

  type Expr = List[Symbol]
  val Expr = List

  case class State(accumulator : Int, op: Op, screen: Int)

  val initial: State = State(0, Plus, 0)

  def push(s: Symbol)(st: Expr): Expr = s :: st

  val x: Int= 5
  x * 10 + x

  def calc(expr: Expr): State = expr.foldLeft(initial)(
    (state: State, symbol: Symbol) => (state, symbol) match {
      case (state, n : Number) => State(state.accumulator, state.op, state.screen * 10 + n.i)
      case (state, op : Op) => State(calc(state.accumulator, state.op, state.screen), op, 0)
      case (state, Equals) => State(0, Plus, calc(state.accumulator, state.op, state.screen))
    }
  )

  def show(s: State): String = s.screen.toString

  def calc(a: Int, op: Op, b:Int) : Int = op match {
    case Plus => a + b
    case Minus => a - b
  }
}

class CalculatorDelegate extends Calculator {
  private var state:  FunCalculator.Expr = Nil

  override def press(n: Int): Calculator = {
    state = FunCalculator.push(Number(n))(state)
    this
  }

  override def plus(): Calculator =  {
    state = FunCalculator.push(Plus)(state)
    this
  }

  override def minus(): Calculator =  {
    state = FunCalculator.push(Minus)(state)
    this
  }

  override def screen: String =  {
    FunCalculator.show(FunCalculator.calc(state.reverse))
  }

  override def equals(): Calculator =  {
    state = FunCalculator.push(Equals)(state)
    this
  }

  override def clear(): Calculator =  {
    state = Nil
    this
  }
}

trait Calculator {
  def press(n: Int): Calculator
  def plus(): Calculator
  def minus(): Calculator
  def screen: String
  def equals(): Calculator
  def clear(): Calculator
}

object Operator extends Enumeration {
  val Plus, Minus = Value
}

import scala.collection.mutable.StringBuilder

final class MutableCalculator extends Calculator { self =>

  private var _current: Option[Int] = None
  private var _screen: StringBuilder = new StringBuilder
  private var _currentOperator: Operator.Value = null

  def screen: String = {
    _screen.toString()
  }

  def noOperator() = {
    _currentOperator = null
  }

  def setCurrent(n: Int): Unit = {
    _current = Some(n)
  }

  def appendScreen(n: Int): Unit = {
    _screen.append(n)
  }

  def appendScreen(s: String): Unit = {
    _screen.append(s)
  }

  def press(n: Int): Calculator = {
    if(_current.isEmpty && _currentOperator == null) {
      setCurrent(n)
    } else if(_current.isDefined && _currentOperator == Operator.Plus) {
      setCurrent(_current.get + n)
      noOperator()
    } else if(_current.isDefined && _currentOperator == Operator.Minus) {
      setCurrent(_current.get - n)
      noOperator()
    } else if(_current.isEmpty && _currentOperator == Operator.Plus) {
      setCurrent(n)
      noOperator()
    } else if(_current.isEmpty && _currentOperator == Operator.Minus) {
      setCurrent(-n)
      noOperator()
    } else {
      setCurrent(_current.get * 10 + n)
    }
    appendScreen(n)
    self
  }

  def plus(): Calculator = {
    appendScreen("+")
    _currentOperator = Operator.Plus
    self
  }

  def minus(): Calculator = {
    appendScreen("-")
    _currentOperator = Operator.Minus
    self
  }

  def equals(): Calculator = {
    _screen = new StringBuilder(_current.get.toString)
    noOperator()
    self
  }

  def clear(): Calculator = {
    _screen = new StringBuilder()
    noOperator()
    self
  }
}
