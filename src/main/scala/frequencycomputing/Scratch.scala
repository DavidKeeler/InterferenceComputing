package frequencycomputing

import quisp.Plot
import quisp.SeriesData
import quisp.Point

object Scratch {
   
   private val range: Double = Math.PI
   private val chartRange = Range.Double(0.0, range, range/1000)
   
	def main(args: Array[String]) {
	  val s1 = Input(100, 0.0)
	  val s2 = Input(115, 0.0)

	  val c1 = Input(78, 0.0)
	  val c2 = Input(112, 0.0)
	  val c = Control(c1, c2)

	  val swap = CSwap(s1, s2, c)
	  val chart = Plot.line(data(s1))
//	    .addSeries(otherFunct)
	    .yAxis.range(-8, 8)

    val time = 0
    val output = new LeastSquaresListener(100, 115)
    val sym = output.listen(s2.toFunc, time)
    println(sym)
	}
	
	private def data(funct: Operation): SeriesData = {
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
}