package scratch

import java.util.Random

import org.apache.commons.math3.fitting.leastsquares.LevenbergMarquardtOptimizer
import org.apache.commons.math3.fitting.leastsquares.LeastSquaresProblem
import org.apache.commons.math3.analysis.ParametricUnivariateFunction
import org.apache.commons.math3.fitting.WeightedObservedPoints
import org.apache.commons.math3.fitting.SimpleCurveFitter

import scala.collection.JavaConverters._
import scala.collection.mutable

abstract class OutputSymbol(value: Boolean, estimatedFreq: Double, targetFreq: Double, error: Double)
case class True(estimatedFreq: Double, targetFreq: Double, error: Double) extends OutputSymbol(true, estimatedFreq, targetFreq, error)
case class False(estimatedFreq: Double, targetFreq: Double, error: Double) extends OutputSymbol(false, estimatedFreq, targetFreq, error)


/**
 * Listens to the output wave and writes to the output strip, if 
 * a frequency is recognized.
 */
trait OutputListener {
  def listen(outputWave: Operation, time: Double): Option[OutputSymbol]
  def extermePoints(outputWave: Operation, time: Double, targetFreq: Double): Seq[(Double, Double)]
}

class ShittyEstimateListener (
    val trueFrequencies: Seq[Double],
    val falseFrequencies: Seq[Double],
    val sampleWavelengths: Double = 10.0) extends OutputListener {

  private val numSamples = 10000
  def listen(operation: Operation, time: Double): Option[OutputSymbol] = {
    val trueOutput = trueFrequencies.map(listen(operation.f, time, _)).filter {
      case (hasOutput, _, _, _) => hasOutput
    }.map {
      case (hasOutput, estFreq, target, error) => True(estFreq, target, error)
    }
    val falseOutput = falseFrequencies.map(listen(operation.f, time, _)).filter {
      case (hasOutput, _, _, _) => hasOutput
    }.map {
      case (hasOutput, estFreq, target, error) => False(estFreq, target, error)
    }

    if (!trueOutput.isEmpty && !falseOutput.isEmpty)
      throw new SomeoneFuckedUpException

    if (!trueOutput.isEmpty) Some(trueOutput.minBy(_.error))
    else if (!falseOutput.isEmpty) Some(falseOutput.minBy(_.error))
    else None
  }

  def extermePoints(operation: Operation, time: Double, frequency: Double): Seq[(Double, Double)] = {
    val timeDelta = sampleWavelengths/frequency
    getExtremes(operation.f, time, timeDelta)
  }

  private def listen(outputWave: Double=>Double, time: Double, frequency: Double): (Boolean, Double, Double, Double) = {
    val allowedError = 2 //0.01 * frequency   // TODO: is there some principaled way to do this?

    val timeDelta = sampleWavelengths/frequency
    val estFreq = estimateFrequency(outputWave, time, timeDelta)
    val error = Math.abs(estFreq - frequency)
//    println(s"estFreq $estFreq frequency $frequency allowedError $allowedError")
    (error < allowedError, estFreq, frequency, error)
  }

  private def estimateFrequency(outputWave: Double=>Double, time: Double, timeDelta: Double): Double = {
    val estWaveLenths = getExtremes(outputWave, time, timeDelta).map(_._1).sliding(2).toSeq.map {
      case Seq(x1, x2) => 2 * (x2 - x1)
    }
    val estWaveLength = estWaveLenths.sum/estWaveLenths.size

    1.0/estWaveLength
  }

  private def getExtremes(outputWave: Double=>Double, time: Double, timeDelta: Double): Seq[(Double, Double)] = {
    val startTime = time - timeDelta
    val endTime = time + timeDelta

    val rand = new Random
    val xValues = for (i <- 0 until numSamples) yield {
      (endTime - startTime) * rand.nextDouble + startTime
    }
    val allPoints = for (x <- xValues.sorted) yield {
      val y = outputWave(x)
      (x, y)
    }

    val threshold = 0.5   // TODO: make configuration param
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

    if (leadingPtOpts.flatten.isEmpty || trailingPtOpts.flatten.isEmpty)
      return Seq()

    val trailingPoints =
      if (leadingPtOpts.flatten.head > trailingPtOpts.flatten.toList.head) trailingPtOpts.flatten.tail
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

    if (leadingPtOpts.flatten.isEmpty || trailingPtOpts.flatten.isEmpty)
      return Seq()

    val trailingPoints =
      if (leadingPtOpts.flatten.head > trailingPtOpts.flatten.head) trailingPtOpts.flatten.tail
      else trailingPtOpts.flatten
    val leadingPoints =
      if (trailingPtOpts.flatten.last < leadingPtOpts.flatten.last) leadingPtOpts.flatten.dropRight(1)
      else leadingPtOpts.flatten

    leadingPoints zip trailingPoints
  }
}

class SomeoneFuckedUpException extends Exception("Was it you?")