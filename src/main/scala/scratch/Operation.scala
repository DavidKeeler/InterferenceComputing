package scratch

abstract class Operation {
  def name: String
  def apply(t: Double): Double

  def f: (Double=>Double) = {
    t: Double => this(t)
  }
}

case class Input(freq: Double, phase: Double=0.0) extends Operation {
  def name = "Input"
  def apply(t: Double): Double = {
    Math.cos(2 * Math.PI * freq * t + phase)
  }
}

case class Control(c1: Input, c2: Input) extends Operation {

  def name = "Control"
  def apply(t: Double): Double = {
    c1(t) + c2(t)
  }
}

case class CSwap(s1: Input, s2: Input, c: Control) extends Operation {
  def name = "Controled Swap"
  def apply(t: Double): Double  = {
    s1(t) + s2(t) + c(t)
  }
}