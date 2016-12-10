package scratch

abstract class Operation {
  def apply(t: Double): Double

  def f: (Double=>Double) = {
    t: Double => this(t)
  }
}

case class Input(freq: Double, phase: Double=0.0) extends Operation {
  def apply(t: Double): Double = {
    Math.sin(2 * Math.PI * freq * t + phase)
  }
}

object Control {
  def apply(con1: Double, des1: Double, con2: Double, des2: Double, period: Double): Control = {
    Control(Input(con1, Math.PI), Input(des1, Math.PI), Input(con2, Math.PI), Input(des2, Math.PI), period)
  }
}

case class Control(con1: Input, des1: Input, con2: Input, des2: Input, period: Double) extends Operation {
  def apply(t: Double): Double = {
    if (isFirst(t))
      con1(t) + des1(t)
    else
      con2(t) + des2(t)
  }

  def first(t: Double): Double = {
    if (isFirst(t))
      con1(t)
    else
      des2(t)
  }

  def second(t: Double): Double = {
    if (isFirst(t))
      des1(t)
    else
      con2(t)
  }

  def isFirst(t: Double): Boolean = (t/period).toInt % 2 == 0
}

case class HalfASwap(in: Input, c: Control, isFirst: Boolean) extends Operation {
  def apply(t: Double): Double  = {
    if (isFirst)
      in(t) + c.first(t)
    else
      in(t) + c.second(t)
  }
}

case class ControlledSwap(in1: Input, in2: Input, c: Control) extends Operation {

  def this(in1: Double, in2: Double, c: Control): ControlledSwap = ControlledSwap(Input(in1), Input(in2), c)

  def apply(t: Double): Double  = in1(t) + in2(t) + c(t)
}