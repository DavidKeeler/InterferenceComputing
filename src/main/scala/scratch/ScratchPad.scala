package scratch

import java.io.File
import java.util.Random

import scalax.chart.api._

object ScratchPad {
  val numGraphPoints = 10000
  val outputDir = "/home/seusswithmoose/tmp/"

  def main(args: Array[String]) {
    val fs = 10000.0
    val sampleTime = 0.1

    val times = (-sampleTime until sampleTime by 1.0/fs)
    val f = Seq(999, 1001)
    val samples = times.map {
      t => f.map(frequency => Math.sin(frequency * t)).sum
    }.toArray
//    val samples = times.map(t => Math.sin(1010 * t))
    val timeAndSamples = times.zip(samples)

    val output = new TryAgain(1001, 1000, 2.0).listen(samples, fs)
    println("Output: " + output)
//    val freqs = Listener(samples, fs)
//    freqs.foreach {
//      case (c, frequency) =>
//        val mag = Math.sqrt(c.real * c.real + c.imag * c.imag)/samples.length
//        if (mag > 0.1 || frequency == 900)
//          println(f"${frequency}%1.4f: $mag%1.4f <$c>")
//    }

//    implicit val period = 1.0
//    val circuit = new Circuit
//    circuit.toGate(0, Input1)
//    circuit.toGate(0, ControlInput)
//    circuit.toGate(1, Input1)

//    circuit.print(false)

    display("tmp/", "help", timeAndSamples, f)
  }

  def display(dir: String,
              title: String,
              samples: Seq[(Double, Double)],
              f: Seq[Double]) {

    val chart = XYLineChart(samples)
    chart.saveAsPDF(dir + title + ".pdf")
  }
}