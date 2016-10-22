package scratch

object ControlledSwap {
  def firstPeriod(trueFreq1: Double, trueFreq2: Double,
                  falseFreq1: Double, falseFreq2: Double,
                  i_t: Int, i_f: Int,
                  j_t: Int, j_f: Int,
                  k_t: Int, k_f: Int,
                  m_t: Int, m_f: Int): Double = {
    val (_, _,  period, _) = controlledSwap(trueFreq1, trueFreq2, falseFreq1, falseFreq2, i_t, i_f, j_t, j_f, k_t, k_f, m_t, m_f)
    period
  }

  def secondPeriod(trueFreq1: Double, trueFreq2: Double,
                  falseFreq1: Double, falseFreq2: Double,
                  i_t: Int, i_f: Int,
                  j_t: Int, j_f: Int,
                  k_t: Int, k_f: Int,
                  m_t: Int, m_f: Int): Double = {
    val (_, _,  _, period) = controlledSwap(trueFreq1, trueFreq2, falseFreq1, falseFreq2, i_t, i_f, j_t, j_f, k_t, k_f, m_t, m_f)
    period
  }

  def controlTrue(trueFreq1: Double, trueFreq2: Double,
                  falseFreq1: Double, falseFreq2: Double,
                  i_t: Int, i_f: Int,
                  j_t: Int, j_f: Int,
                  k_t: Int, k_f: Int,
                  m_t: Int, m_f: Int): Control = {
    val ((control, _), _,  _, _) = controlledSwap(trueFreq1, trueFreq2, falseFreq1, falseFreq2, i_t, i_f, j_t, j_f, k_t, k_f, m_t, m_f)
    control
  }

  def controlFalse(trueFreq1: Double, trueFreq2: Double,
                  falseFreq1: Double, falseFreq2: Double,
                  i_t: Int, i_f: Int,
                  j_t: Int, j_f: Int,
                  k_t: Int, k_f: Int,
                  m_t: Int, m_f: Int): Control = {
    val ((_, control), _,  _, _) = controlledSwap(trueFreq1, trueFreq2, falseFreq1, falseFreq2, i_t, i_f, j_t, j_f, k_t, k_f, m_t, m_f)
    control
  }

  def outputTrue(trueFreq1: Double, trueFreq2: Double,
                falseFreq1: Double, falseFreq2: Double,
                i_t: Int, i_f: Int,
                j_t: Int, j_f: Int,
                k_t: Int, k_f: Int,
                m_t: Int, m_f: Int): Control = {
    val (_, (output, _),  _, _) = controlledSwap(trueFreq1, trueFreq2, falseFreq1, falseFreq2, i_t, i_f, j_t, j_f, k_t, k_f, m_t, m_f)
    output
  }

  def outputFalse(trueFreq1: Double, trueFreq2: Double,
                falseFreq1: Double, falseFreq2: Double,
                i_t: Int, i_f: Int,
                j_t: Int, j_f: Int,
                k_t: Int, k_f: Int,
                m_t: Int, m_f: Int): Control = {
    val (_, (_, output),  _, _) = controlledSwap(trueFreq1, trueFreq2, falseFreq1, falseFreq2, i_t, i_f, j_t, j_f, k_t, k_f, m_t, m_f)
    output
  }

  private def controlledSwap(trueFreq1: Double, trueFreq2: Double,
                            falseFreq1: Double, falseFreq2: Double,
                            i_t: Int, i_f: Int,
                            j_t: Int, j_f: Int,
                            k_t: Int, k_f: Int,
                            m_t: Int, m_f: Int): ((Control, Control), (Control, Control), Double, Double) = {
    val c11 = control1(trueFreq1, falseFreq1, i_f, i_t)
    val c21 = control1(trueFreq2, falseFreq2, m_f, m_t)

    val c22 = control5(trueFreq2, falseFreq2, j_f, j_t)
    val c12 = control5(trueFreq1, falseFreq1, k_f, k_t)

    val conTime1_1 = periodConstructive(trueFreq1, c11, i_t)
    val conTime1_2 = periodConstructive(falseFreq1, c11, i_f)

    val conTime2_1 = periodConstructive(trueFreq2, c21, m_t)
    val conTime2_2 = periodConstructive(falseFreq2, c21, m_f)

    val destructTime1_1 = periodDestructive(trueFreq2, c22, j_t)
    val destructTime1_2 = periodDestructive(falseFreq2, c22, j_f)

    val destructTime2_1 = periodDestructive(trueFreq1, c12, k_t)
    val destructTime2_2 = periodDestructive(falseFreq1, c12, k_f)

    val outputTrue1_1 = outputFreq(trueFreq1, c11)
    val outputTrue1_2 = outputFreq(trueFreq2, c21)

    val outputFalse1_1 = outputFreq(falseFreq1, c11)
    val outputFalse1_2 = outputFreq(falseFreq2, c21)

    val outputTrue2_1 = outputFreq(trueFreq1, c12)
    val outputTrue2_2 = outputFreq(trueFreq2, c22)

    val outputFalse2_1 = outputFreq(falseFreq1, c12)
    val outputFalse2_2 = outputFreq(falseFreq2, c22)

    compare(0.1, conTime1_1, conTime1_2, destructTime1_1, destructTime1_2)
    compare(0.1, conTime2_1, conTime2_2, destructTime2_1, destructTime2_2)
    compare(0.1, outputTrue1_1, outputTrue1_2)
    compare(0.1, outputFalse1_1, outputFalse1_2)
    compare(0.1, outputTrue2_1, outputTrue2_2)
    compare(0.1, outputFalse2_1, outputFalse2_2)

    ((Control(c11, c22), Control(c21, c12)), (Control(outputTrue1_1, outputTrue2_1), Control(outputFalse1_1, outputFalse1_1)), conTime1_1, conTime2_1)
  }

  private def control1(trueFreq: Double, falseFreq: Double, i1: Int, i2: Int): Double = (i1 * trueFreq + i2 * falseFreq)/(i1 + i2)
  private def control2(trueFreq: Double, falseFreq: Double, i1: Int, i2: Int): Double = (i1 * trueFreq - i2 * falseFreq)/(i1 + i2)
  private def control3(trueFreq: Double, falseFreq: Double, i1: Int, i2: Int): Double = (i1 * trueFreq + i2 * falseFreq)/(i1 - i2)
  private def control4(trueFreq: Double, falseFreq: Double, i1: Int, i2: Int): Double = (i1 * trueFreq - i2 * falseFreq)/(i1 - i2)

  private def control5(trueFreq: Double, falseFreq: Double, i1: Int, i2: Int): Double = ((i1 + 0.5) * trueFreq + (i2 + 0.5) * falseFreq)/((i1 + 0.5) + (i2 + 0.5))
  private def control6(trueFreq: Double, falseFreq: Double, i1: Int, i2: Int): Double = ((i1 + 0.5) * trueFreq - (i2 + 0.5) * falseFreq)/((i1 + 0.5) + (i2 + 0.5))
  private def control7(trueFreq: Double, falseFreq: Double, i1: Int, i2: Int): Double = ((i1 + 0.5) * trueFreq + (i2 + 0.5) * falseFreq)/((i1 + 0.5) - (i2 + 0.5))
  private def control8(trueFreq: Double, falseFreq: Double, i1: Int, i2: Int): Double = ((i1 + 0.5) * trueFreq - (i2 + 0.5) * falseFreq)/((i1 + 0.5) - (i2 + 0.5))

  private def periodConstructive(signal: Double, control: Double, i: Int): Double = 2 * i /Math.abs(signal - control)
  private def periodDestructive(signal: Double, control: Double, i: Int): Double = 2 * i /Math.abs(signal - control)

  private def outputFreq(signal: Double, control: Double): Double = (signal + control)/2

  private def compare(tolerance: Double, values: Double *) {
    for(a <- values; b <- values) {
      if (Math.abs(a - b) > tolerance)
        throw new SomeoneFuckedUpException
    }
  }
}

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

object Control {
  def apply(c1: Double, c2: Double): Control = Control(Input(c1), Input(c2))
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