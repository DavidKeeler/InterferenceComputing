package scratch

import quisp.Plot
import quisp.SeriesData
import quisp.Point

import scala.collection.immutable.NumericRange

object ScratchPad {
   
	def main(args: Array[String]) {
	  val s1 = Input(1, 0.0)
	  val s2 = Input(11, 0.0)

	  val c1 = Input(15, 0.0)
	  val c2 = Input(16, 0.0)
	  val c = Control(c1, c2)

	  val swap = CSwap(s1, s2, c)
	  val operation = s1

    val time = 0.0
    val trueFrequencies = Seq(100.0)
    val falseFrequencies = Seq(120.0)
    val listener = new ShittyEstimateListener(trueFrequencies, falseFrequencies)

    val outputSymbol = listener.listen(operation, time)
    println("OUTPUT: " + outputSymbol)

    display(operation, listener, time, trueFrequencies.head)
	}

	private def display(operation: Operation, listener: OutputListener, time: Double, referenceFrequency: Double) {
    val range = 1.0
    val chartRange = Range.Double(-range, range, range/1000)
    Plot.line(data(operation, chartRange))
      .addSeries(extermePoints(listener, operation, time, referenceFrequency))
      .yAxis.range(-4, 4)
  }
	
	private def data(operation: Operation, chartRange: NumericRange[Double]): SeriesData = {
	  val functPoints = 
		  for (x <- chartRange) yield {
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