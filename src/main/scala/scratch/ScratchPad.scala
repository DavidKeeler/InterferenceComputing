package scratch

import java.io.{File, FileOutputStream}
import java.util.Random

import org.jfree.chart.{ChartFactory, ChartUtilities}

import scalax.chart.api._

object ScratchPad {
  val numGraphPoints = 10000
  val outputDir = "/home/seusswithmoose/tmp/"

  def main(args: Array[String]) {
    val fs = 10000.0
    val sampleLength = 0.01

    val swap = new ControlledSwap(outT = 100, outF = 101, in1T = 100, in2T = 101, outStore = 100, mag = 1)
    swap.input1 = false
    swap.input2 = false
    swap.control = true
    val samples = sampleFrom(swap.inputFreq, 1.0, fs, sampleLength)

    val trueSamples = sampleFrom(Seq(100), 1.0, fs, sampleLength)
    val falseSamples = sampleFrom(Seq(101), 1.0, fs, sampleLength)
    val listener = new Listener(trueSamples.map(_._2), falseSamples.map(_._2), 2.0, fs)
    val output = listener(samples.map(_._2))

    println("Output: " + output)
    display("tmp/", "help", swap, listener, 10 * sampleLength, fs)

    println(swap)
  }

  def sampleFrom(f: Seq[Double], mag: Double, fs: Double, sampleLength: Double): Seq[(Double, Double)] = {
    val times = sampleTimes(fs, sampleLength)
    times.map {
      t => (t, f.map(frequency => mag * Math.cos(frequency * t)).sum)
    }
  }

  def sampleTimes(fs: Double, sampleLength: Double) = (2*Math.PI - sampleLength) until (2*Math.PI + sampleLength) by 1.0/fs

  def display(dir: String,
              title: String,
              swap: ControlledSwap,
              listener: Listener,
              displayLength: Double,
              fs: Double) {

    val trueSamples = sampleFrom(Seq(100), 1.0, fs, displayLength)
    val falseSamples = sampleFrom(Seq(101), 1.0, fs, displayLength)
    val swapSamples = sampleFrom(swap.inputFreqDes, swap.mag, fs, displayLength)

    val dataset = new XYSeriesCollection
    val samplesSeries = new XYSeries("Swap")
    swapSamples.map { case (t, a) => samplesSeries.add(t, a) }
    dataset.addSeries(samplesSeries)

    val trueSeries = new XYSeries("True")
    trueSamples.map { case (t, a) => trueSeries.add(t, a) }
//    dataset.addSeries(trueSeries)

    val falseSeries = new XYSeries("False")
    falseSamples.map { case (t, a) => falseSeries.add(t, a) }
//    dataset.addSeries(falseSeries)

    val testSeries = new XYSeries("WTF?")
    sampleFrom(Seq(1.0), swap.mag, fs, 2*Math.PI).map { case (t, a) => testSeries.add(t, a) }
//    dataset.addSeries(testSeries)

    val chart = ChartFactory.createXYLineChart("Title", "Time", "Amp", dataset)
    val out = new FileOutputStream(new File(dir + title + ".png"))
    ChartUtilities.writeChartAsPNG(out, chart, 900, 470)
  }
}

class Listener(val trueSamples: Seq[Double], val falseSamples: Seq[Double], val mag: Double, val fs: Double) {
  def apply(samples: Seq[Double]): Boolean = {
    val trueError = samples.zip(trueSamples).map {
      case (sample, trueSample) => Math.abs(sample - trueSample)
    }.sum

    val falseError = samples.zip(falseSamples).map {
      case (sample, falseSample) => Math.abs(sample - falseSample)
    }.sum

    println("true: " + trueError + " falseError: " + falseError)
    trueError < falseError
  }
}

class ControlledSwap(val outT: Int, val outF: Int, val in1T: Int, val in2T: Int, val outStore: Int, val mag: Int) {
  val in1F = in1T - 2 * (outT - outF)
  val in2F = in2T - 2 * (outT - outF)
  val outStoreT = outStore + 0.25
  val outStoreF = outStore - (outT - outF) + 0.25

  val conT = 2 * outT - in1T
  val conStoreT = 2 * outStoreT - in2T

  val conF = 2 * outT - in2T
  val conStoreF = 2 * outStoreT - in1T

  var input1 = true
  var input2 = true
  var control = true

  def inputFreq: Seq[Double] = inputFreq(input1, input2, control)
  private def inputFreq(first: Boolean, second: Boolean, control: Boolean): Seq[Double] = {
    (first, second, control) match {
      case (true, true, true) => Seq(in1T, in2T, conT, conStoreT)
      case (true, true, false) => Seq(in1T, in2T, conF, conStoreF)
      case (true, false, true) => Seq(in1T, in2F, conT, conStoreT)
      case (true, false, false) => Seq(in1T, in2F, conF, conStoreF)
      case (false, true, true) => Seq(in1F, in2T, conT, conStoreT)
      case (false, true, false) => Seq(in1F, in2T, conF, conStoreF)
      case (false, false, true) => Seq(in1F, in2F, conT, conStoreT)
      case (false, false, false) => Seq(in1F, in2F, conF, conStoreF)
    }
  }

  def inputFreqCon: Seq[Double] = inputFreqCon(input1, input2, control)
  private def inputFreqCon(first: Boolean, second: Boolean, control: Boolean): Seq[Double] = {
    (first, second, control) match {
      case (true, true, true) => Seq(in1T, conT)
      case (true, true, false) => Seq(in2T, conF)
      case (true, false, true) => Seq(in1T, conT)
      case (true, false, false) => Seq(in2F, conF)
      case (false, true, true) => Seq(in1F, conT)
      case (false, true, false) => Seq(in2T, conF)
      case (false, false, true) => Seq(in1F, conT)
      case (false, false, false) => Seq(in2F, conF)
    }
  }

  def inputFreqDes: Seq[Double] = inputFreqDes(input1, input2, control)
  private def inputFreqDes(first: Boolean, second: Boolean, control: Boolean): Seq[Double] = {
    (first, second, control) match {
        case (true, true, true) => Seq(in2T, conStoreT)
        case (true, true, false) => Seq(in1T, conStoreF)
        case (true, false, true) => Seq(in2F, conStoreT)
        case (true, false, false) => Seq(in1T, conStoreF)
        case (false, true, true) => Seq(in2T, conStoreT)
        case (false, true, false) => Seq(in1F, conStoreF)
        case (false, false, true) => Seq(in2F, conStoreT)
        case (false, false, false) => Seq(in1F, conStoreF)
    }
  }

  override def toString =
    "input1: " + input1 + " input2: " + input2 + " control: " + control +
      "\noutT: " + outT + " outF: " + outF + "\noutStoreT: " + outStoreT + " outStoreF: " + outStoreF +
      "\nin1T: " + in1T + " in1F: " + in1F + "\nin2T: " + in2T + " in2F: " + in2F +
      "\nconT: " + conT + " conF: " + conF + "\nconStoreT: " + conStoreT + " conStoreF: " + conStoreF
}


