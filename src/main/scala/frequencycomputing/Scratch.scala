package frequencycomputing

import quisp.Plot
import quisp.SeriesData
import quisp.Point

object Scratch {
   private val trueFreq = 1.0
   private val falseFreq = 1.1
   
   private val range = 120.0
   private val chartRange = Range.Double(0.0, range, 2 * range/1000)
   
   private val TRUE = sine(trueFreq, 0)
   private val FALSE = sine(falseFreq, 0)
   
   private val OR = sine(falseFreq, Math.PI)
   private val AND = sine(trueFreq, Math.PI)
   private val NOT = sine(trueFreq, Math.PI) + sine(falseFreq, Math.PI)
   
   private val NAND = AND + NOT
   private val NOR = OR + NOT
   
	def main(args: Array[String]) {
	  val funct = data(sine(1.0, 0) + sine(1.01, 0))
//	  val otherFunct = data(FALSE)
	  
	  val chart = Plot.line(funct)
//	    .addSeries(otherFunct)
//	    .series(0).name("TRUE + FALSE")
//	    .series(1).name("TRUE")
	    .yAxis.range(-2, 2)
	}
   
   private implicit def functionToAdditiveFunction(f: Double=>Double): AdditiveFunction = new AdditiveFunction(f)
   
   private class AdditiveFunction(private val f: Double=>Double) extends (Double=>Double) {
     override def apply(x: Double): Double = f(x)
     
     def +(that: Double=>Double): AdditiveFunction = {
       x: Double => this(x) + that(x)
     }
   }
   
   private def sine(freq: Double, phase: Double): Double=>Double = {
     val normedFreq = 2 * Math.PI * freq
     x: Double => Math.sin(normedFreq * x + phase)
   }
	
	private def data(funct: Double=>Double): SeriesData = {
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