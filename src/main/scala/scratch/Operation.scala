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

case class ControlledSwap(in1: Input, in2: Input, c: Control) extends Operation {
  def name = "Controlled Swap"
  def apply(t: Double): Double  = {
    in1(t) + in2(t) + c(t)
  }
}