package scratch

import java.util.Random

import org.apache.commons.math3.fitting.leastsquares.LevenbergMarquardtOptimizer
import org.apache.commons.math3.fitting.leastsquares.LeastSquaresProblem
import org.apache.commons.math3.analysis.ParametricUnivariateFunction
import org.apache.commons.math3.fitting.WeightedObservedPoints
import org.apache.commons.math3.fitting.SimpleCurveFitter

import scala.collection.JavaConverters._
import scala.collection.mutable

abstract class OutputSymbol(val value: Boolean, estimatedFreq: Double, targetFreq: Double, error: Double)
case class True(estimatedFreq: Double, targetFreq: Double, error: Double) extends OutputSymbol(true, estimatedFreq, targetFreq, error)
case class False(estimatedFreq: Double, targetFreq: Double, error: Double) extends OutputSymbol(false, estimatedFreq, targetFreq, error)


/**
 * Listens to the output wave and writes to the output strip, if 
 * a frequency is recognized.
 */
trait OutputListener {
  def listen(outputWave: Operation, time: Double): Option[OutputSymbol]
  def extremePoints(outputWave: Operation, time: Double, targetFreq: Double): Seq[(Double, Double)]
}

class ShittyEstimateListener(
    val trueFrequencies: Seq[Double],
    val falseFrequencies: Seq[Double],
    val sampleWavelengths: Double = 5.0,
    val threshold: Double = 0.0,
    val numSamples: Int = 10000) extends OutputListener {

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

    if (!trueOutput.isEmpty && !falseOutput.isEmpty) {
      throw new SomeoneFuckedUpException
    }

    if (!trueOutput.isEmpty) Some(trueOutput.minBy(_.error))
    else if (!falseOutput.isEmpty) Some(falseOutput.minBy(_.error))
    else None
  }

  def extremePoints(operation: Operation, time: Double, frequency: Double): Seq[(Double, Double)] = {
    val timeDelta = (sampleWavelengths + 1)/frequency
    getExtremes(operation.f, time, timeDelta)
  }

  private def listen(outputWave: Double=>Double, time: Double, frequency: Double): (Boolean, Double, Double, Double) = {
    val allowedError = 1 // TODO: is there some principaled way to do this?

    val timeDelta = sampleWavelengths/frequency
    //    val timeDelta = (sampleWavelengths + 1)/frequency

    val estFreq = estimateFrequency(outputWave, time, timeDelta)
    val error = Math.abs(estFreq - frequency)

//    println(s"estFreq $estFreq frequency $frequency allowedError $allowedError")
    (error < allowedError, estFreq, frequency, error)
  }

  private def estimateFrequency(outputWave: Double=>Double, time: Double, timeDelta: Double): Double = {
    val extremePoints = getExtremes(outputWave, time, timeDelta)
    val estWaveLengths = extremePoints.sortBy(_._1).sliding(2).toSeq.map {
      case Seq(pt1, pt2) => 2 * (pt2._1 - pt1._1)
    }
    val estWaveLength = estWaveLengths.sum/estWaveLengths.size

    1.0/estWaveLength
  }

  private def getExtremes(outputWave: Double=>Double, time: Double, timeDelta: Double): Seq[(Double, Double)] = {
    val startTime = time - timeDelta
    val endTime = time + timeDelta

//    val startTime = time - timeDelta/2
//    val endTime = time + timeDelta/2

    val rand = new Random
    val xValues = for (i <- 0 until numSamples) yield {
      (endTime - startTime) * rand.nextDouble + startTime
    }
    val allPoints = for (x <- xValues.sorted) yield {
      (x, outputWave(x))
    }

    val sectionsOfPoints = sections(allPoints, threshold)

    sectionsOfPoints.map { points =>
      val weightedX = points.map { case (x, y) => x * y }
      val x = weightedX.sum/points.map(_._2).sum
      (x, points.map(_._2).max)
    }
  }

  private def sections(allPoints: Seq[(Double, Double)], threshold: Double): Seq[Seq[(Double, Double)]] = {
    val sortedPoints = allPoints.sortBy(_._1)
    val indexedPoints = sortedPoints.zipWithIndex
    val pointPairs = indexedPoints.sliding(2).toSeq

    val boundaryIndices = pointPairs.filter { case Seq(((x, y), index), ((nextX, nextY), nextIndex)) =>
      y > 0 && nextY <= 0 || y < 0 && nextY >= 0
    } map { case Seq((point, index), (nextPoint, nextIndex)) =>
      index
    }

    boundaryIndices.sliding(2).toSeq.map {
      case Seq(prev, next) => sortedPoints.slice(prev, next)
    }.map { points =>
      points.filter { case (x, y) => Math.abs(y) >= threshold }
    }.filter { points =>
      !points.isEmpty
    }
  }
}

class SomeoneFuckedUpException(how: String = "Was it you?") extends Exception(how)