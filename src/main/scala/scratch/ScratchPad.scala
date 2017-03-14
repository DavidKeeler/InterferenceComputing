package scratch

import java.io.File
import java.util.Random

import scalax.chart.api._

object ScratchPad {
  val numGraphPoints = 10000
  val outputDir = "/home/seusswithmoose/tmp/"

	def main(args: Array[String]) {
    // Free params
    val period = 1.0
    val a = 1
    val b = 4
    val c = 4
    val trueFreq1 = 1000.0

    // Constructive
    val controlTrueCon = 2 * a/period + trueFreq1
    val falseFreq1 = (1 - b/a) * controlTrueCon + b * trueFreq1/a
    val outTrue = (trueFreq1 + controlTrueCon)/2
    val controlFalseCon = outTrue + c/period
    val trueFreq2 = outTrue + (outTrue - controlFalseCon)
    val falseFreq2 = falseFreq1 - trueFreq1 + trueFreq2
    val outFalse = (falseFreq1 + controlTrueCon)/2

    // Verify
    val outTrue1 = (trueFreq1 + controlTrueCon)/2
    val outTrue2 = (trueFreq2 + controlFalseCon)/2
    val outFalse1 = (falseFreq1 + controlTrueCon)/2
    val outFalse2 = (falseFreq2 + controlFalseCon)/2

    val beatTrue1 = Math.abs(trueFreq1 - controlTrueCon)/2
    val beatTrue2 = Math.abs(trueFreq2 - controlFalseCon)/2
    val beatfalse1 = Math.abs(falseFreq1 - controlTrueCon)/2
    val beatFalse2 = Math.abs(falseFreq2 - controlFalseCon)/2

    // Destructive
    val d = 2
    val e = 1
    val f = 2
    val g = 1
    val controlTrueDes = ((d + 0.25) * trueFreq2 - (e + 0.25) * falseFreq2)/(d - e)
    val controlFalseDes = ((f + 0.25) * trueFreq1 - (g + 0.25) * falseFreq1)/(f - g)

    println(s"IN 1: true - $trueFreq1, false - $falseFreq1")
    println(s"IN 2: true - $trueFreq2, false - $falseFreq2")
    println(s"CONTROL TRUE: con - $controlTrueCon, des - $controlTrueDes")
    println(s"CONTROL FALSE: con - $controlFalseCon, des - $controlFalseDes")
    println(s"OUT: true - $outTrue, false - $outFalse")

    println(s"Verify output - True: $outTrue1 $outTrue2")
    println(s"Verify output - False: $outFalse1 $outFalse2")
    println(s"beatTrue1: $beatTrue1 beatTrue2: $beatTrue2 beatfalse1: $beatfalse1 beatFalse2: $beatFalse2")

    val controlTrue = Control(con = controlTrueCon, des = controlTrueDes)
    val controlFalse = Control(con = controlFalseCon, des = controlFalseDes)

    val store000 = ControlledStore(falseFreq1, falseFreq2, controlFalse)
    val store001 = ControlledStore(falseFreq1, falseFreq2, controlTrue)
    val store010 = ControlledStore(falseFreq1, trueFreq2, controlFalse)
    val store011 = ControlledStore(falseFreq1, trueFreq2, controlTrue)
    val store100 = ControlledStore(trueFreq1, falseFreq2, controlFalse)
    val store101 = ControlledStore(trueFreq1, falseFreq2, controlTrue)
    val store110 = ControlledStore(trueFreq1, trueFreq2, controlFalse)
    val store111 = ControlledStore(trueFreq1, trueFreq2, controlTrue)

    val listener = new ShittyEstimateListener(
      trueFrequencies = Seq(trueFreq1, trueFreq2),
      falseFrequencies = Seq(falseFreq1, falseFreq2),
      sampleWavelengths = 1.0,
      threshold = 0.0,
      numSamples = 1000,
      allowedError = 2.0)

    display(outputDir, "in1: 0, in2: 0, c: 0", store000, false, Seq(period), listener, trueFreq1)
    display(outputDir, "in1: 0, in2: 0, c: 1", store001, true, Seq(period), listener, trueFreq1)
    display(outputDir, "in1: 0, in2: 1, c: 0", store010, false, Seq(period), listener, trueFreq1)
    display(outputDir, "in1: 0, in2: 1, c: 1", store011, true, Seq(period), listener, trueFreq1)
    display(outputDir, "in1: 1, in2: 0, c: 0", store100, false, Seq(period), listener, trueFreq1)
    display(outputDir, "in1: 1, in2: 0, c: 1", store101, true, Seq(period), listener, trueFreq1)
    display(outputDir, "in1: 1, in2: 1, c: 0", store110, false, Seq(period), listener, trueFreq1)
    display(outputDir, "in1: 1, in2: 1, c: 1", store111, true, Seq(period), listener, trueFreq1)

    listen("swap000", store000, Seq(period), listener)
    listen("swap001", store001, Seq(period), listener)
    listen("swap010", store010, Seq(period), listener)
    listen("swap011", store011, Seq(period), listener)
    listen("swap100", store100, Seq(period), listener)
    listen("swap101", store101, Seq(period), listener)
    listen("swap110", store110, Seq(period), listener)
    listen("swap111", store111, Seq(period), listener)

    sys.exit
  }

  private def constructive(numPeriods: Int, time: Double): Double = {
    2 * (numPeriods + 0.25)/time
  }

  private def destructive(numPeriods: Int, time: Double): Double = {
    numPeriods/time
  }

	private def listen(descr: String,
                     operation: ControlledStore,
                     times: Seq[Double],
                     listener: OutputListener) {
    val outputSymbols = times.map(time => listener.listen(operation, time))
    println(descr + ": " + outputSymbols.mkString(", ") + " at times: " + times.mkString(","))
  }

  def display(dir: String,
              title: String,
              store: ControlledStore,
              isFirst: Boolean,
              times: Seq[Double],
              listener: OutputListener,
              scale: Double) {
    val (beat1, beat2) =
      if (isFirst) {
        (store.in1 + store.c.con, store.in2 + store.c.des)
      } else {
        (store.in1 + store.c.des, store.in2 + store.c.con)
      }

    val waves = List("input 1" -> points(beat1.f), "input 2" -> points(beat2.f))
    val listenerPts = times.map { time =>
      s"time: $time" -> extermePoints(listener, store, time, scale)
    }

    val chart = XYLineChart(waves ++ listenerPts) //, title)
    chart.saveAsPDF(dir + title + ".pdf")
  }
	
	private def points(f: Double=>Double, minRange: Double = 0.9, maxRange: Double = 1.1): Seq[(Double, Double)] = {
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
	  listener.extremePoints(operation, time, frequecy)
  }
}