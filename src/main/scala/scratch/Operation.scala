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
    Math.cos(2 * Math.PI * freq * t + phase)
  }
}

object Control {
  def apply(con: Double, des: Double): Control = Control(Input(con), Input(des))
//  def apply(con: Double, des: Double): Control = Control(Input(con, Math.PI), Input(des, Math.PI))
}

case class Control(con: Input, des: Input) extends Operation {
  def apply(t: Double): Double = con(t) + des(t)
}

object ControlledStore {
  def apply(in1: Double, in2: Double, control: Control): ControlledStore = ControlledStore(Input(in1), Input(in2), control)
}

case class ControlledStore(in1: Input, in2: Input, c: Control) extends Operation {
  def apply(t: Double): Double = c(t) + in1(t) + in2(t)
}