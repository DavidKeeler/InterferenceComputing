package scratch

import java.util.Random

import org.apache.commons.math3.fitting.leastsquares.LevenbergMarquardtOptimizer
import org.apache.commons.math3.fitting.leastsquares.LeastSquaresProblem
import org.apache.commons.math3.analysis.ParametricUnivariateFunction
import org.apache.commons.math3.fitting.WeightedObservedPoints
import org.apache.commons.math3.fitting.SimpleCurveFitter

import scala.collection.JavaConverters._
import scala.collection.mutable

sealed trait OutputSymbol
case object TRUE extends OutputSymbol
case object FALSE extends OutputSymbol

/**
 * Listens to the output wave and writes to the output strip, if 
 * a frequency is recognized.
 */
trait OutputListener {
  def listen(outputWave: Double=>Double, time: Double): Option[OutputSymbol]
  def extermePoints(outputWave: Double=>Double, time: Double): Seq[(Double, Double)]
}

class LeastSquaresListener(
    val trueFrequency: Double, 
    val falseFrequency: Double,
    val periods: Double = 10.0) extends OutputListener {

  val error = 0.01 * (trueFrequency + falseFrequency)/2
  def startTime(time: Double) = time - 2 * periods/(trueFrequency + falseFrequency)
  def endTime(time: Double) = time + 2 * periods/(trueFrequency + falseFrequency)

  def listen(outputWave: Double=>Double, time: Double): Option[OutputSymbol] = {
    val estFreq = estimateFrequency(outputWave, time)
    println("estFreq: " + estFreq)

	  if (Math.abs(estFreq - trueFrequency) < error) Some(TRUE)
    else if (Math.abs(estFreq - falseFrequency) < error) Some(FALSE)
    else None
  }

  private def estimateFrequency(outputWave: Double=>Double, time: Double): Double = {

    val rand = new Random
    val xValues = for (i <- 0 until 10000) yield {
      (endTime(time) - startTime(time)) * rand.nextDouble + startTime((time))
    }
    val allPoints = for (x <- xValues.sorted) yield {
      val y = outputWave(x)
      (x, y)
    }

    val threshold = 0.5
    val cutpoints = maxCutpoints(allPoints, threshold) ++ minCutpoints(allPoints, -threshold)
    val estimatedExtremePts = cutpoints.sortBy(_._1).map {
      case (begin, end) => allPoints.slice(begin, end)
    }.map {
      points =>
        if (points.head._2 >= threshold)
          (points.map(_._1).sum/points.size, points.map(_._2).max)
        else
          (points.map(_._1).sum/points.size, points.map(_._2).min)
    }

    val estWaveLenths = estimatedExtremePts.map(_._1).sliding(2).toSeq.map {
      case Seq(x1, x2) => 2 * (x2 - x1)
    }
    val estWaveLength = estWaveLenths.sum/estWaveLenths.size

    1.0/estWaveLength
  }

  def extermePoints(outputWave: Double=>Double, time: Double): Seq[(Double, Double)] = {
    val rand = new Random
    val xValues = for (i <- 0 until 10000) yield {
      (endTime(time) - startTime(time)) * rand.nextDouble + startTime((time))
    }
    val allPoints = for (x <- xValues.sorted) yield {
      val y = outputWave(x)
      (x, y)
    }

    val threshold = 0.5
    val cutpoints = maxCutpoints(allPoints, threshold) ++ minCutpoints(allPoints, -threshold)
    cutpoints.sortBy(_._1).map {
      case (begin, end) => allPoints.slice(begin, end)
    }.map {
      points =>
        if (points.head._2 >= threshold)
          (points.map(_._1).sum/points.size, points.map(_._2).max)
        else
          (points.map(_._1).sum/points.size, points.map(_._2).min)
    }
  }

  private def maxCutpoints(allPoints: Seq[(Double, Double)], threshold: Double): Seq[(Int, Int)] = {

    val leadingPtOpts = for (i <- 1 until allPoints.size) yield {
      if (allPoints(i)._2 >= threshold && allPoints(i - 1)._2 < threshold) Some(i)
      else None
    }
    val trailingPtOpts = for (i <- 1 until allPoints.size) yield {
      if (allPoints(i)._2 < threshold && allPoints(i - 1)._2 >= threshold) Some(i)
      else None
    }

    val trailingPoints =
      if (leadingPtOpts.flatten.head > trailingPtOpts.flatten.head) trailingPtOpts.flatten.tail
      else trailingPtOpts.flatten
    val leadingPoints =
      if (trailingPtOpts.flatten.last < leadingPtOpts.flatten.last) leadingPtOpts.flatten.dropRight(1)
      else leadingPtOpts.flatten

    leadingPoints zip trailingPoints
  }

  private def minCutpoints(allPoints: Seq[(Double, Double)], threshold: Double): Seq[(Int, Int)] = {
    val leadingPtOpts = for (i <- 1 until allPoints.size) yield {
      if (allPoints(i)._2 <= threshold && allPoints(i - 1)._2 > threshold) Some(i)
      else None
    }
    val trailingPtOpts = for (i <- 1 until allPoints.size) yield {
      if (allPoints(i)._2 > threshold && allPoints(i - 1)._2 <= threshold) Some(i)
      else None
    }

    val trailingPoints =
      if (leadingPtOpts.flatten.head > trailingPtOpts.flatten.head) trailingPtOpts.flatten.tail
      else trailingPtOpts.flatten
    val leadingPoints =
      if (trailingPtOpts.flatten.last < leadingPtOpts.flatten.last) leadingPtOpts.flatten.dropRight(1)
      else leadingPtOpts.flatten

    leadingPoints zip trailingPoints
  }
}
