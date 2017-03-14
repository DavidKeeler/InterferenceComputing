package scratch

import java.util.Random

import org.apache.commons.math3.fitting.leastsquares.LevenbergMarquardtOptimizer
import org.apache.commons.math3.fitting.leastsquares.LeastSquaresProblem
import org.apache.commons.math3.analysis.ParametricUnivariateFunction
import org.apache.commons.math3.fitting.WeightedObservedPoints
import org.apache.commons.math3.fitting.SimpleCurveFitter

import scala.collection.JavaConverters._
import scala.collection.mutable

abstract class OutputSymbol(val value: Option[Boolean], estimatedFreq: Double, targetFreq: Double) {
  def error = Math.abs(estimatedFreq - targetFreq)
  override def toString = f"${this.getClass.getSimpleName}[est freq: $estimatedFreq%.2f error: $error%.2f]" //target freq: $targetFreq%.2f]"
}
case class True(estimatedFreq: Double, targetFreq: Double) extends OutputSymbol(Some(true), estimatedFreq, targetFreq)
case class False(estimatedFreq: Double, targetFreq: Double) extends OutputSymbol(Some(false), estimatedFreq, targetFreq)
case class Empty(estimatedFreq: Double, targetFreq: Double) extends OutputSymbol(None, estimatedFreq, targetFreq)

/**
 * Listens to the output wave and writes to the output strip, if 
 * a frequency is recognized.
 */
trait OutputListener {
  def listen(outputWave: Operation, time: Double): OutputSymbol
  def extremePoints(outputWave: Operation, time: Double, targetFreq: Double): Seq[(Double, Double)]
}

class ShittyEstimateListener(
    val trueFrequencies: Seq[Double],
    val falseFrequencies: Seq[Double],
    val sampleWavelengths: Double = 5.0,
    val threshold: Double = 0.0,
    val numSamples: Int = 100000,
    val allowedError: Double = 40.0) extends OutputListener {

  def listen(operation: Operation, time: Double): OutputSymbol = {
    val trueOutput = trueFrequencies.map(listen(operation.f, time, _)).map {
      case (_, estFreq, target) => True(estFreq, target)
//      case (true, estFreq, target) => True(estFreq, target)
//      case (false, estFreq, target) => Empty(estFreq, target)
    }
    val falseOutput = falseFrequencies.map(listen(operation.f, time, _)).map {
      case (_, estFreq, target) => False(estFreq, target)
//      case (true, estFreq, target) => False(estFreq, target)
//      case (false, estFreq, target) => Empty(estFreq, target)
    }

      // TODO: error, if a true and false is matched
//    if (!trueOutput.isEmpty && !falseOutput.isEmpty) {
//      throw new SomeoneFuckedUpException
//    }

    (trueOutput ++ falseOutput).minBy(_.error)
  }

  def extremePoints(operation: Operation, time: Double, frequency: Double): Seq[(Double, Double)] = {
    val timeDelta = (sampleWavelengths + 1)/frequency
    getExtremes(operation.f, time, timeDelta)
  }

  private def listen(outputWave: Double=>Double, time: Double, frequency: Double): (Boolean, Double, Double) = {
    val timeDelta = (sampleWavelengths + 1)/frequency
    val estFreq = estimateFrequency(outputWave, time, timeDelta)
    val error = Math.abs(estFreq - frequency)

    (error < allowedError, estFreq, frequency)
  }

  private def estimateFrequency(outputWave: Double=>Double, time: Double, timeDelta: Double): Double = {
    val extremePoints = getExtremes(outputWave, time, timeDelta)
    if (extremePoints.size == 1)
      return 1.0/timeDelta

    val estWaveLengths = extremePoints.sortBy(_._1).sliding(2).toSeq.map {
      case Seq(pt1, pt2) => 2 * (pt2._1 - pt1._1)
    }
    val estWaveLength = estWaveLengths.sum/estWaveLengths.size
    1.0/estWaveLength
  }

  private def getExtremes(outputWave: Double=>Double, time: Double, timeDelta: Double): Seq[(Double, Double)] = {
    val startTime = time - timeDelta/2
    val endTime = time + timeDelta/2

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
      (x, points.map(_._2).maxBy(Math.abs))
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