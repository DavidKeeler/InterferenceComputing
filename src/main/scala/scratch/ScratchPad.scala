package scratch

import java.util.Random

import quisp.Plot
import quisp.SeriesData
import quisp.Point

object ScratchPad {
  val minRange = 0.0
  val maxRange = 40.0
  val numGraphPoints = 10000

	def main(args: Array[String]) {
    val trueFreq1 = 7.525
    val trueFreq2 = 7.525
    val falseFreq1 = 7.5
    val falseFreq2 = 7.5

    val i_t = 2
    val i_f = 3
    val j_t = 5
    val j_f = 7
    val k_t = 11
    val k_f = 13
    val m_t = 17
    val m_f = 21

    val controlTrue = ControlledSwap.controlTrue(trueFreq1, trueFreq2, falseFreq1, falseFreq2, i_t, i_f, j_t, j_f, k_t, k_f, m_t, m_f)
    val controlFalse = ControlledSwap.controlFalse(trueFreq1, trueFreq2, falseFreq1, falseFreq2, i_t, i_f, j_t, j_f, k_t, k_f, m_t, m_f)
    val time1 = ControlledSwap.firstPeriod(trueFreq1, trueFreq2, falseFreq1, falseFreq2, i_t, i_f, j_t, j_f, k_t, k_f, m_t, m_f)
    val time2 = ControlledSwap.secondPeriod(trueFreq1, trueFreq2, falseFreq1, falseFreq2, i_t, i_f, j_t, j_f, k_t, k_f, m_t, m_f)

    val Control(outTrue1, outTrue2) = ControlledSwap.outputTrue(trueFreq1, trueFreq2, falseFreq1, falseFreq2, i_t, i_f, j_t, j_f, k_t, k_f, m_t, m_f)
    val Control(outFalse1, outFalse2) = ControlledSwap.outputFalse(trueFreq1, trueFreq2, falseFreq1, falseFreq2, i_t, i_f, j_t, j_f, k_t, k_f, m_t, m_f)

    val in1 = Input(trueFreq1)
    val in2 = Input(falseFreq2)

    val swap = ControlledSwap(in1, in2, controlTrue)

    doStuff(operation = swap, time = time1, trueFrequencies = Seq(outTrue1.freq, outTrue2.freq), falseFrequencies = Seq(outFalse1.freq, outFalse2.freq))
	}

	private def doStuff(operation: Operation, time: Double, trueFrequencies: Seq[Double], falseFrequencies: Seq[Double]) {
    val listener = new ShittyEstimateListener(trueFrequencies, falseFrequencies, 10.0)

    val outputSymbol = listener.listen(operation, time)
    println("OUTPUT: " + outputSymbol)

    display1(operation, listener, time, trueFrequencies.head)
  }

	private def display1(operation: Operation, listener: OutputListener, time: Double, referenceFrequency: Double) {
    Plot.line(data(operation, randXValues))
      .addSeries(extermePoints(listener, operation, time, referenceFrequency))
      .yAxis.range(-4, 4)
  }

  private def display2(op1: Operation, op2: Operation, listener: OutputListener, time: Double, referenceFrequency: Double) {
    Plot.line(data(op1, randXValues))
      .addSeries(data(op2, randXValues))
      .addSeries(extermePoints(listener, op1, time, referenceFrequency))
      .yAxis.range(-4, 4)
  }

  private def randXValues: Seq[Double] = {
    val rand = new Random
    for (i <- 0 to numGraphPoints) yield {
      (maxRange - minRange) * rand.nextDouble + minRange
    }
  }
	
	private def data(operation: Operation, randomPoints: Seq[Double]): SeriesData = {
	  val functPoints = 
		  for (x <- randomPoints.sorted) yield {
		    new Point {
		      def X = Some(x)
		      def Y = Some(operation(x))
		      def Name = None
		    }
		  }
	  
	  new SeriesData {
	    def points = functPoints
	  }
	}

	private def extermePoints(listener: OutputListener, operation: Operation, time: Double, frequecy: Double): SeriesData = {
	  val points = listener.extermePoints(operation, time, frequecy)
    val pts = points.map {
      case (x, y) => new Point {
        def X = Some(x)
        def Y = Some(operation(x))
        def Name = None
      }
    }

    new SeriesData {
      def points = pts
    }
  }
}