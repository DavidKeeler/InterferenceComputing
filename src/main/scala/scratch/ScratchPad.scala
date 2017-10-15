package scratch

import java.io.File
import java.util.Random

import scalax.chart.api._

object ScratchPad {
  val numGraphPoints = 10000
  val outputDir = "/home/seusswithmoose/tmp/"

  def main(args: Array[String]) {
    val fs = 1000.0
    val sampleTime = 0.1

    val f = Seq(905, 895)
    println("Input Freq: " + f.mkString(","))
    val times = (0.0 until sampleTime by 1.0/fs)
    val samples = times.map {
      t => f.map(x => Math.sin(2 * Math.PI * x * t)).sum/f.size
    }.toArray

//    println(times.zip(samples.map(s => if (Math.abs(s) < 0.001) 0.0 else s)).mkString("\n"))

    val freqs = Listener(samples).zipWithIndex.map { case (c, i) => (c, (i * fs)/samples.length) }
    freqs.foreach {
      case (c, frequency) =>
        val mag = Math.sqrt(c.real * c.real + c.imag * c.imag)/samples.length
        if (mag > 0.1)
          println(f"${frequency}%1.4f: $mag%1.4f <$c>")
    }

    println("Max: " + freqs.map(_._2).max)

//    implicit val period = 1.0
//    val circuit = new Circuit
//    circuit.toGate(0, Input1)
//    circuit.toGate(0, ControlInput)
//    circuit.toGate(1, Input1)

//    circuit.print(false)


//    val timeAndSamples = samples.toSeq.zip(times)
//    display("tmp?", "help", timeAndSamples)
  }

  def display(dir: String,
              title: String,
              samples: Seq[(Double, Double)]) {

    val data = for (i <- 1 to 5) yield (i,i)
    val chart = XYLineChart(samples)
    chart.saveAsPDF(dir + title + ".pdf")
  }
}