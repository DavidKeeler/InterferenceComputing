package scratch

import java.util.Random

import quisp.Plot
import quisp.SeriesData
import quisp.Point

object ScratchPad {
  val numGraphPoints = 10000

	def main(args: Array[String]) {
	  val scale = 50.0
    val trueFreq1 = scale
    val falseFreq1 = scale + 4

    val falseFreq2 = scale + 30
    val trueFreq2 = scale + 30

    val controlFalse = Control(con1 = scale + 2, des1 = scale + 26, con2 = scale + 28, des2 = scale + 8, 1.0)
//    val controlTrue = Control(c12, c21, scale + 28, scale - 1, 1.0)

    val swapFalse = ControlledSwap(falseFreq1, falseFreq2, controlFalse)
//    val swapTrue = ControlledSwap(in1, in2, controlTrue)


    // Are the output values right?
//    val outFalse1 = Math.abs(falseFreq1 + c11)/2
//    val outTrue1 = Math.abs(trueFreq1 + c11)/2
//
//    val outFalse2 = Math.abs(falseFreq2 + c22)/2
//    val outTrue2 = Math.abs(trueFreq2 + c22)/2

//    listen(operation = swapFalse, times = Seq(1.0), trueFrequencies = Seq(outTrue1, outTrue2), falseFrequencies = Seq(outFalse1, outFalse2))
    display(swapFalse)
	}

	private def listen(operation: Operation, times: Seq[Double], trueFrequencies: Seq[Double], falseFrequencies: Seq[Double]) {
    val listener = new ShittyEstimateListener(trueFrequencies, falseFrequencies, 10.0)
    val outputSymbols = times.map(time => listener.listen(operation, time))
    println("OUTPUT: " + outputSymbols.mkString(", ") + " at times: " + times.mkString(","))
  }

	private def display(swap: ControlledSwap) {

    val beat1 = HalfASwap(swap.in1, swap.c, true)
    val beat2 = HalfASwap(swap.in2, swap.c, false)
    Plot.line(data(beat1.f, randXValues))
      .addSeries(data(beat2.f, randXValues))
      .yAxis.range(-3, 3)
  }

  private def randXValues: Seq[Double] = {
    val minRange = 0.0
    val maxRange = 2.0
    val rand = new Random
    for (i <- 0 to numGraphPoints) yield {
      (maxRange - minRange) * rand.nextDouble + minRange
    }
  }
	
	private def data(f: Double=>Double, randomPoints: Seq[Double]): SeriesData = {
	  val functPoints = 
		  for (x <- randomPoints.sorted) yield {
		    new Point {
		      def X = Some(x)
		      def Y = Some(f(x))
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