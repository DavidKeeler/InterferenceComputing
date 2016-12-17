package scratch

import java.util.Random

import quisp.Plot
import quisp.SeriesData
import quisp.Point

object ScratchPad {
  val numGraphPoints = 10000
  val scale = 100.0

	def main(args: Array[String]) {
    doStuff
//    doOtherStuff
	}

	private def doOtherStuff {
    object Beat {
      def apply(freq1: Double, freq2: Double) = new Beat(Input(freq1), Input(freq2))
    }

    case class Beat(freq1: Input, freq2: Input) extends Operation {
      def apply(t: Double): Double = {
        freq1(t) + freq2(t)
      }
      def output: Double = (freq1.freq + freq2.freq)/2
    }

    val freqScale = 100
    val beat1 = Beat(freqScale, freqScale + 2)
    val beat2 = Beat(Input(freqScale - 9), Input(freqScale - 7, Math.PI))

    val otherListener = new ShittyEstimateListener(trueFrequencies = Seq(beat1.output), falseFrequencies = Seq(beat2.output),
      sampleWavelengths = 5.0,
      threshold = 1.0,
      numSamples = 10000)

    listen("Beat", beat1 + beat2, times = Seq(0.0), otherListener)
    displaySomething("Beat", beat1 + beat2, otherListener, 0.0)
  }

	private def doStuff {
    //    val controlFalse = Control(con1 = scale + 2, des1 = scale + n - 4, con2 = scale + n - 6, des2 = scale + 8, 1.0)
    //    val controlTrue = Control(con1 = scale + n - 6, des1 = scale + 4, con2 = scale + 2, des2 = scale + n - 4, 1.0)

    val i = 2
    val j = 2
    val k = 2
    val m = 4
    val l = 2
    val h = 2

    val n = 2 * i
    val trueFreq1 = scale
    val trueFreq2 = scale + n

    val controlFalse = Control(
      con1 = scale + 2,
      des1 = scale + n - (2 * j),
      con2 = scale + n - 2 * (2 * k - 1),
      des2 = scale + 2 * m,
      period = 1.0)
    val controlTrue = Control(
      con1 = trueFreq1 + controlFalse.con1.freq - trueFreq2,
      des1 = scale + 2 * l,
      con2 = trueFreq2 + controlFalse.con2.freq - trueFreq1,
      des2 = trueFreq2 - 2 * l,
      period = 1.0)

    val falseFreq1 = scale + 4
    val falseFreq2 = falseFreq1 + controlFalse.con1.freq - controlTrue.con1.freq

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
    println(s"Out true 1: $outTrue11, $outTrue12")

    val outFalse11 = Math.abs(falseFreq1 + controlFalse.con1.freq)/2
    val outFalse12 = Math.abs(falseFreq2 + controlTrue.con1.freq)/2
    println(s"Out false 1: $outFalse11, $outFalse12")

    val outTrue21 = Math.abs(trueFreq2 + controlFalse.con2.freq)/2
    val outTrue22 = Math.abs(trueFreq1 + controlTrue.con2.freq)/2
    println(s"Out true 2: $outTrue21, $outTrue22")

    val outFalse21 = Math.abs(falseFreq2 + controlFalse.con2.freq)/2
    val outFalse22 = Math.abs(falseFreq1 + controlTrue.con2.freq)/2
    println(s"Out false 2: $outFalse21, $outFalse22")

    val times = Seq(0.25, 1.25)
    val trueFrequencies = Seq(outTrue11, outTrue21)
    val falseFrequencies = Seq(outFalse11, outFalse21)
    listen("swap000", operation = swap000, times, trueFrequencies, falseFrequencies)
    listen("swap001", operation = swap001, times, trueFrequencies, falseFrequencies)
    listen("swap010", operation = swap010, times, trueFrequencies, falseFrequencies)
    listen("swap011", operation = swap011, times, trueFrequencies, falseFrequencies)
    listen("swap100", operation = swap100, times, trueFrequencies, falseFrequencies)
    listen("swap101", operation = swap101, times, trueFrequencies, falseFrequencies)
    listen("swap110", operation = swap110, times, trueFrequencies, falseFrequencies)
    listen("swap111", operation = swap111, times, trueFrequencies, falseFrequencies)

    display("in1: 0, in2: 0, c: 0", swap000, false)
    display("in1: 0, in2: 0, c: 1", swap001, true)
    display("in1: 0, in2: 1, c: 0", swap010, false)
    display("in1: 0, in2: 1, c: 1", swap011, true)
    display("in1: 1, in2: 0, c: 0", swap100, false)
    display("in1: 1, in2: 0, c: 1", swap101, true)
    display("in1: 1, in2: 1, c: 0", swap110, false)
    display("in1: 1, in2: 1, c: 1", swap111, true)
  }

	private def listen(descr: String, operation: Operation, times: Seq[Double], trueFrequencies: Seq[Double], falseFrequencies: Seq[Double]) {
    val listener = new ShittyEstimateListener(trueFrequencies, falseFrequencies, 5.0)
    listen(descr, operation, times, listener)
  }


  private def listen(descr: String, operation: Operation, times: Seq[Double], listener: OutputListener) {
    val outputSymbols = times.map(time => listener.listen(operation, time))
    println(descr + ": " + outputSymbols.mkString(", ") + " at times: " + times.mkString(","))
  }

  private def displaySomething(title: String, operation: Operation, listener: OutputListener, time: Double, minRange: Double = -0.05, maxRange: Double = 0.05) {
    val chart = Plot.line(data(operation.f, randXValues(minRange, maxRange)))
      .addSeries(extermePoints(listener, operation, time, scale))

    chart.title(title)
      .yAxis.range(-3, 3)
  }

	private def display(title: String, swap: ControlledSwap, isSwap: Boolean, listener: Option[OutputListener] = None) {
    val beat1 = HalfASwap(swap.in1, swap.c, isFirstOutput = !isSwap)
    val beat2 = HalfASwap(swap.in2, swap.c, isFirstOutput = isSwap)

    val chart = Plot.line(data(beat1.f, randXValues()))
      .addSeries(data(beat2.f, randXValues()))

    if (listener.isDefined) {
      chart.addSeries(extermePoints(listener.get, swap, time = 0.25, scale))
      chart.addSeries(extermePoints(listener.get, swap, time = 1.25, scale))
    }

    chart.title(title)
      .yAxis.range(-3, 3)
    chart.series(0).name("input 1")
      .series(1).name("input 2")
  }

  private def randXValues(minRange: Double = 0.0, maxRange: Double = 2.0): Seq[Double] = {
    val rand = new Random
    for (i <- 0 to numGraphPoints) yield {
      (maxRange - minRange) * rand.nextDouble + minRange
    }
  }
	
	private def data(f: Double=>Double, randomPoints: Seq[Double]): SeriesData = {
	  val functPoints = 
		  for (x <- randomPoints.sorted) yield {
		    new Point {
		      def X = Some(x)
		      def Y = Some(f(x))
		      def Name = None
		    }
		  }
	  
	  new SeriesData {
	    def points = functPoints
	  }
	}

	private def extermePoints(listener: OutputListener, operation: Operation, time: Double, frequecy: Double): SeriesData = {
	  val points = listener.extremePoints(operation, time, frequecy)
    val pts = points.map {
      case (x, y) => new Point {
        def X = Some(x)
        def Y = Some(operation(x))
        def Name = None
      }
    }

    new SeriesData {
      def points = pts
    }
  }
}