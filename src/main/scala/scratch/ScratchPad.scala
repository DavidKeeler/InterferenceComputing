package scratch

import java.util.Random

import quisp.Plot
import quisp.SeriesData
import quisp.Point

import scala.collection.immutable.NumericRange

object ScratchPad {
  val minRange = 0.0
  val maxRange = 80.0
  val numGraphPoints = 10000

	def main(args: Array[String]) {
    val in1 = Input(20.025, 0.0)
    val in2 = Input(20.0, 0.0)
    val otherControl = Control(in1, in2)

    val c1 = Input(10.0, 0.0)
    val c2 = Input(10.05, 0.0)
    val control = Control(c1, c2)
    val swap = ControlledSwap(in1, in2, control)

    doStuff(operation = swap, time = 20.0, trueFrequencies = Seq(10.025), falseFrequencies = Seq(20.05))
	}

	private def doStuff(operation: Operation, time: Double, trueFrequencies: Seq[Double], falseFrequencies: Seq[Double]) {
    val listener = new ShittyEstimateListener(trueFrequencies, falseFrequencies, 10.0)

    val outputSymbol = listener.listen(operation, time)
    println("OUTPUT: " + outputSymbol)

    display(operation, listener, time, trueFrequencies.head)
  }

	private def display(operation: Operation, listener: OutputListener, time: Double, referenceFrequency: Double) {
    val rand = new Random
	  val randomPoints = for (i <- 0 to numGraphPoints) yield {
      (maxRange - minRange) * rand.nextDouble + minRange
    }
    Plot.line(data(operation, randomPoints))
      .addSeries(extermePoints(listener, operation, time, referenceFrequency))
      .yAxis.range(-4, 4)
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