package scratch

import quisp.Plot
import quisp.SeriesData
import quisp.Point

import scala.collection.immutable.NumericRange

object Scratch {
   
	def main(args: Array[String]) {
	  val s1 = Input(15.93, 0.0)
	  val s2 = Input(115, 0.0)

	  val c1 = Input(98, 0.0)
	  val c2 = Input(102, 0.0)
	  val c = Control(c1, c2)

	  val swap = CSwap(s1, s2, c)
	  val func = c

    val time = 0.0
    val listener = new LeastSquaresListener(trueFrequency = 100, falseFrequency = 120, periods=10)
    val sym = listener.listen(func.toFunc, time)
    println("OUTPUT: " + sym)


    println("End Time: " + listener.endTime(time))
    println("Start Time: " + listener.startTime(time))
    val range = 0.1
    val chartRange = Range.Double(-range, range, range/1000)
	  val chart = Plot.line(data(func, chartRange)).addSeries(extermePoints(listener, func.toFunc, time))
	    .yAxis.range(-4, 4)
	}
	
	private def data(funct: Operation, chartRange: NumericRange[Double]): SeriesData = {
	  val functPoints = 
		  for (x <- chartRange) yield {
		    new Point {
		      def X = Some(x)
		      def Y = Some(funct(x))
		      def Name = None
		    }
		  }
	  
	  new SeriesData {
	    def points = functPoints
	  }
	}

	private def extermePoints(listener: OutputListener, f: Double=>Double, time: Double): SeriesData = {
	  val points = listener.extermePoints(f, time)
    val pts = points.map {
      case (x, y) => new Point {
        def X = Some(x)
        def Y = Some(f(x))
        def Name = None
      }
    }

    new SeriesData {
      def points = pts
    }
  }
}