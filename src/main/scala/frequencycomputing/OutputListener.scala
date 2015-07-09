package frequencycomputing

import org.apache.commons.math3.fitting.leastsquares.LevenbergMarquardtOptimizer
import org.apache.commons.math3.fitting.leastsquares.LeastSquaresProblem
import org.apache.commons.math3.analysis.ParametricUnivariateFunction
import org.apache.commons.math3.fitting.WeightedObservedPoints
import org.apache.commons.math3.fitting.SimpleCurveFitter
import scala.collection.JavaConverters._

sealed trait OutputSymbol
case object TRUE extends OutputSymbol
case object FALSE extends OutputSymbol

/**
 * Listens to the output wave and writes to the output strip, if 
 * a frequency is recognized.
 */
trait OutputListener {
  def listen(outputWave: Double=>Double): Option[OutputSymbol]
}

class LeastSquaresListener(trueFrequency: Double, falseFrequency: Double) extends OutputListener {
  
  private val threshold = 0.03       // threshold below which the classifier will fire
  private val minX = 0.0
  private val maxX = 2 * Math.PI
  
  private val trueFunct = new SinFunct(trueFrequency)
  private val falseFunct = new SinFunct(falseFrequency)
  
  def listen(outputWave: Double=>Double): Option[OutputSymbol] = {
    val (_, _, trueFit) = meanSqError(outputWave, trueFunct)
    val (_, _, falseFit) = meanSqError(outputWave, falseFunct)
println("LeastSquaresListener: trueFit: " + trueFit + " falseFit: " + falseFit + " ratio: " + trueFit/falseFit)
    if (trueFit <= threshold && trueFit < falseFit) Some(TRUE)
    else if (falseFit <= threshold && falseFit < trueFit) Some(FALSE)
    else None
  }
  
  /**
   * @returns (amplitude, phase, mean squared error)
   */
  private def meanSqError(outputWave: Double=>Double, target: ParametricUnivariateFunction): (Double, Double, Double) = {    
    val points = new WeightedObservedPoints
    for (x <- minX to maxX by (maxX - minX)/10000) 
      points.add(x, outputWave(x))
    
    // Find the best fit for the amplitude and phase
    val initialParams = Array(1.0, 0.0)
    val fitter = SimpleCurveFitter.create(target, initialParams)
    val Array(amplitude, phase) = fitter.fit(points.toList)
    
    // Find the mean sq error
    val avError = points.toList.asScala.map {
      point => 
        val diff = point.getY - target.value(point.getX, amplitude, phase)
        diff * diff
    }.sum/points.toList.size
    
	(amplitude, phase, avError)
  }

  private class SinFunct(val frequency: Double) extends ParametricUnivariateFunction {
    override def value(x: Double, param: Double*): Double = {
      if (param.size != 2) throw new IllegalArgumentException("Incorrect number of paramters.")
      
      val amplitude = param(0)
      val phase = param(1)
      amplitude * Math.sin(frequency * x + phase)
    }
    
    override def gradient(x: Double, param: Double*): Array[Double] = {
      if (param.size != 2) throw new IllegalArgumentException("Incorrect number of paramters.")
      
      val amplitude = param(0)
      val phase = param(1)
      
      // find the derivative for each param
      val derMag = Math.sin(frequency * x + phase)
      val derPhase = amplitude * Math.cos(frequency * x + phase)
      
      Array(derMag, derPhase)
    }
  }
}
