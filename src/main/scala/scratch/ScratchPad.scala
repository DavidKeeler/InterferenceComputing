package scratch

import java.io.File
import java.util.Random

import scalax.chart.api._

object ScratchPad {
  val numGraphPoints = 10000
  val scale = 1000.0
  val outputDir = "/home/seusswithmoose/tmp/"

	def main(args: Array[String]) {
    //    val controlFalse = Control(con1 = scale + 2, des1 = scale + n - 4, con2 = scale + n - 6, des2 = scale + 8, 1.0)
    //    val controlTrue = Control(con1 = scale + n - 6, des1 = scale + 4, con2 = scale + 2, des2 = scale + n - 4, 1.0)

    val i = 1
    val j = 1
    val k = 1
    val m = 1
    val l = 1
    val h = 1

    val b = 2
    val n = 3

    val trueFreq1 = scale
    val trueFreq2 = scale + (2 * n)

    val controlFalse = Control(
      con1 = scale + (2 * i - 1),
      des1 = scale + (2 * n) - (2 * j),
      con2 = scale + (2 * n) - (2 * k - 1),
      des2 = scale + (2 * m),
      period = 1.0)
    val controlTrue = Control(
      con1 = trueFreq1 + controlFalse.con1.freq - trueFreq2,
      des1 = scale + (2 * l),
      con2 = trueFreq2 + controlFalse.con2.freq - trueFreq1,
      des2 = trueFreq2 - (2 * h),
      period = 1.0)

    val falseFreq1 = scale + 2 * b
    val falseFreq2 = falseFreq1 + controlFalse.con1.freq - controlTrue.con1.freq

    println(s"Input 1: true - $trueFreq1, false - $falseFreq1")
    println(s"Input 2: true - $trueFreq2, false - $falseFreq2")

    println(s"Con True - con1: ${controlTrue.con1.freq} des1: ${controlTrue.des1.freq} con2: ${controlTrue.con2.freq} des2: ${controlTrue.des2.freq}")
    println(s"Con False - con1: ${controlFalse.con1.freq} des1: ${controlFalse.des1.freq} con2: ${controlFalse.con2.freq} des2: ${controlFalse.des2.freq}")

    val swap000 = ControlledSwap(falseFreq1, falseFreq2, controlFalse)
    val swap001 = ControlledSwap(falseFreq1, falseFreq2, controlTrue)
    val swap010 = ControlledSwap(falseFreq1, trueFreq2, controlFalse)
    val swap011 = ControlledSwap(falseFreq1, trueFreq2, controlTrue)
    val swap100 = ControlledSwap(trueFreq1, falseFreq2, controlFalse)
    val swap101 = ControlledSwap(trueFreq1, falseFreq2, controlTrue)
    val swap110 = ControlledSwap(trueFreq1, trueFreq2, controlFalse)
    val swap111 = ControlledSwap(trueFreq1, trueFreq2, controlTrue)

    val outTrue11 = Math.abs(trueFreq1 + controlFalse.con1.freq)/2
    val outTrue12 = Math.abs(trueFreq2 + controlTrue.con1.freq)/2
    val outTrue21 = Math.abs(trueFreq2 + controlFalse.con2.freq)/2
    val outTrue22 = Math.abs(trueFreq1 + controlTrue.con2.freq)/2
    println(s"Out true: $outTrue11, $outTrue21")
    val outFalse11 = Math.abs(falseFreq1 + controlFalse.con1.freq)/2
    val outFalse12 = Math.abs(falseFreq2 + controlTrue.con1.freq)/2
    val outFalse21 = Math.abs(falseFreq2 + controlFalse.con2.freq)/2
    val outFalse22 = Math.abs(falseFreq1 + controlTrue.con2.freq)/2
    println(s"Out false: $outFalse11, $outFalse21")

    val times = Seq(0.5, 1.5)
    val trueFrequencies = Seq(outTrue11, outTrue21)
    val falseFrequencies = Seq(outFalse11, outFalse21)

    val error = 2.5
    val listener = new ShittyEstimateListener(trueFrequencies, falseFrequencies, sampleWavelengths = 5.0, allowedError = error)
    println(s"error: $error")

    display(outputDir, "in1: 0, in2: 0, c: 0", swap000, false, times, listener)
    display(outputDir, "in1: 0, in2: 0, c: 1", swap001, true, times, listener)
    display(outputDir, "in1: 0, in2: 1, c: 0", swap010, false, times, listener)
    display(outputDir, "in1: 0, in2: 1, c: 1", swap011, true, times, listener)
    display(outputDir, "in1: 1, in2: 0, c: 0", swap100, false, times, listener)
    display(outputDir, "in1: 1, in2: 0, c: 1", swap101, true, times, listener)
    display(outputDir, "in1: 1, in2: 1, c: 0", swap110, false, times, listener)
    display(outputDir, "in1: 1, in2: 1, c: 1", swap111, true, times, listener)

    listen("swap000", operation = swap000, times, listener)
    listen("swap001", operation = swap001, times, listener)
    listen("swap010", operation = swap010, times, listener)
    listen("swap011", operation = swap011, times, listener)
    listen("swap100", operation = swap100, times, listener)
    listen("swap101", operation = swap101, times, listener)
    listen("swap110", operation = swap110, times, listener)
    listen("swap111", operation = swap111, times, listener)
  }

	private def listen(descr: String, operation: ControlledSwap, times: Seq[Double], listener: OutputListener) {
    val beat1 = HalfASwap(operation.in1, operation.c, isFirstOutput = true)
    val beat2 = HalfASwap(operation.in2, operation.c, isFirstOutput = false)
    val outputSymbols = times.map(time => listener.listen(beat1, time))


//    val outputSymbols = times.map(time => listener.listen(operation, time))
    println(descr + ": " + outputSymbols.mkString(", ") + " at times: " + times.mkString(","))
  }

	private def display(dir: String,
                      title: String,
                      swap: ControlledSwap,
                      isSwap: Boolean,
                      times: Seq[Double],
                      listener: OutputListener) {
    val beat1 = HalfASwap(swap.in1, swap.c, isFirstOutput = !isSwap)
    val beat2 = HalfASwap(swap.in2, swap.c, isFirstOutput = isSwap)

    val waves = List("input 1" -> points(beat1.f), "input 2" -> points(beat2.f))

//    val listenerPts = List(s"time: 0.5" -> extermePoints(listener, swap, 0.5, scale))
    val listenerPts = times.map { time =>
      s"time: $time" -> extermePoints(listener, swap, time, scale)
    }

    val chart = XYLineChart(waves ++ listenerPts) //, title)
    chart.saveAsPDF(dir + title + ".pdf")
  }
	
	private def points(f: Double=>Double, minRange: Double = 0.0, maxRange: Double = 2.0): Seq[(Double, Double)] = {
    val xValues = randXValues(minRange, maxRange)
    for (x <- xValues.sorted) yield (x, f(x))
	}

  private def randXValues(minRange: Double, maxRange: Double): Seq[Double] = {
    val rand = new Random
    for (i <- 0 to numGraphPoints) yield {
      (maxRange - minRange) * rand.nextDouble + minRange
    }
  }

	private def extermePoints(listener: OutputListener, operation: Operation, time: Double, frequecy: Double): Seq[(Double, Double)] = {
	  val pts = listener.extremePoints(operation, time, frequecy)
//    pts.map(println)
    pts
  }
}