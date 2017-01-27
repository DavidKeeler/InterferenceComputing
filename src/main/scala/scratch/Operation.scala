package scratch

abstract class Operation {
  def apply(t: Double): Double

  def f: (Double=>Double) = {
    t: Double => this(t)
  }

  def +(that: Operation): Operation = {
    val otherThis = this
    new Operation {
      def apply(t: Double): Double = otherThis(t) + that(t)
    }
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
    if (isFirstPeriod(t))
      con1(t) + des1(t)
    else
      con2(t) + des2(t)
  }

  def firstOutput(t: Double): Double = {
    if (isFirstPeriod(t))
      con1(t)
    else
      des2(t)
  }

  def secondOutput(t: Double): Double = {
    if (isFirstPeriod(t))
      des1(t)
    else
      con2(t)
  }

  def isFirstPeriod(t: Double): Boolean = (t/period).toInt % 2 == 0
}

case class HalfASwap(in: Input, c: Control, isFirstOutput: Boolean) extends Operation {
  def apply(t: Double): Double = {
    if (isFirstOutput)
      in(t) + c.firstOutput(t)
     else
      in(t) + c.secondOutput(t)
  }
}

object ControlledSwap {
  def apply(in1: Double, in2: Double, c: Control) = new ControlledSwap(Input(in1), Input(in2), c)
}

case class ControlledSwap(in1: Input, in2: Input, c: Control) extends Operation {
  def apply(t: Double): Double  = c(t) + in1(t) + in2(t)
}