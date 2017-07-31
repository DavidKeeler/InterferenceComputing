package scratch

import java.io.File
import java.util.Random

import scalax.chart.api._

object ScratchPad {
  val numGraphPoints = 10000
  val outputDir = "/home/seusswithmoose/tmp/"

  def main(args: Array[String]) {
    implicit val period = 1.0

    val circuit = new Circuit
    circuit.toGate(gateId = circuit.inputGates.keys.head, which = Input1)

    val ids = circuit.inputGates.keys
    val gates = circuit.inputGates.flatMap(_._2)

    println(gates.size)
    gates.map(gate => printGate(gate))
  }

  def printGate(gate: Gate, verify: Boolean = false)(implicit period: Double) {
    println("Id: " + gate.id)
    println("inTrue1: " + gate.params.inTrue1 + " inFalse1: " + gate.params.inFalse1)
    println("inTrue2: " + gate.params.inTrue2 + " inFalse2: " + gate.params.inFalse2)
    println("controlTrue: " + gate.params.controlTrue + " controlFalse: " + gate.params.controlFalse)
    println("controlTrueDes: " + gate.params.controlTrueDes + " controlFalseDes: " + gate.params.controlFalseDes)
    println

    println("outTrue: " + gate.params.outTrue + " outFalse: " + gate.params.outFalse)
    println("outDesTrue: " + gate.params.outDesTrue + " outDesFalse: " + gate.params.outDesFalse)
    println

    if (verify) {
      println("Verification")
      val outTrue1 = (gate.params.inTrue1 + gate.params.controlTrue) / 2
      val outTrue2 = (gate.params.inTrue2 + gate.params.controlFalse) / 2
      println("outTrue1: " + outTrue1 + " outTrue2: " + outTrue2)

      val outFalse1 = (gate.params.inFalse1 + gate.params.controlTrue) / 2
      val outFalse2 = (gate.params.inFalse2 + gate.params.controlFalse) / 2
      println("outFalse1: " + outFalse1 + " outFalse2: " + outFalse2)

      val outTrueDes1 = (gate.params.inTrue2 + gate.params.controlTrueDes) / 2
      val outTrueDes2 = (gate.params.inTrue1 + gate.params.controlFalseDes) / 2
      println("outTrueDes1: " + outTrueDes1 + " outTrueDes2: " + outTrueDes2)

      val outFalseDes1 = (gate.params.inFalse1 + gate.params.controlFalseDes) / 2
      val outFalseDes2 = (gate.params.inFalse2 + gate.params.controlTrueDes) / 2
      println("outFalseDes1: " + outFalseDes1 + " outFalseDes2: " + outFalseDes2)
    }
  }

  def old() {
    val period = 1.0
    val x1 = 1    // Distance between true and false out
    val x2 = 1    // Distance between input 1 and destruct out
    val x3 = 1    // Distance between input 1 and out
    val x4 = 1    // Distance between input 2 and destruct out

    val destructTrue = 100.75
    val destructFalse = destructTrue - x1/period

    val trueFreq1 = destructTrue + (x2 + 0.25)/period
    val falseFreq1 = trueFreq1 - 2 * x1/period

    val trueFreq2 = destructTrue + (x4 + 0.25)/period
    val falseFreq2 = trueFreq2 - 2 * x1/period

    val outTrue = trueFreq1 - x3/period
    val outFalse = outTrue - x1/period

    val controlTrue = outTrue - x3/period
    val controlFalse = 2 * outTrue - trueFreq2

    val controlTrueDes = destructTrue - (x4 + 0.25)/period
    val controlFalseDes = destructTrue - (x2 + 0.25)/period

    println(s"trueFreq1: $trueFreq1 falseFreq1: $falseFreq1")
    println(s"controlTrue: $controlTrue controlTrueDes: $controlTrueDes")
    println
    println(s"trueFreq2: $trueFreq2 falseFreq2: $falseFreq2")
    println(s"controlFalse: $controlFalse controlFalseDes: $controlFalseDes")
    println
    println(s"outTrue: $outTrue outFalse: $outFalse")
    println(s"destructTrue: $destructTrue destructFalse: $destructFalse")
    println
    println(s"Verify out1 true: ${(trueFreq1 + controlTrue)/2} false ${(falseFreq1 + controlTrue)/2}")
    println(s"Verify out2 true: ${(trueFreq2 + controlFalse)/2} false ${(falseFreq2 + controlFalse)/2}")

    val trueControl = ControlOp(con = controlTrue, des = controlTrueDes, amp = 1.0)
    val falseControl = ControlOp(con = controlFalse, des = controlFalseDes, amp = 1.0)

    val store000 = ControlledStore(falseFreq1, falseFreq2, falseControl)
    val store001 = ControlledStore(falseFreq1, falseFreq2, trueControl)
    val store010 = ControlledStore(falseFreq1, trueFreq2, falseControl)
    val store011 = ControlledStore(falseFreq1, trueFreq2, trueControl)
    val store100 = ControlledStore(trueFreq1, falseFreq2, falseControl)
    val store101 = ControlledStore(trueFreq1, falseFreq2, trueControl)
    val store110 = ControlledStore(trueFreq1, trueFreq2, falseControl)
    val store111 = ControlledStore(trueFreq1, trueFreq2, trueControl)

    val listener = new ShittyEstimateListener(
      trueFrequencies = Seq(trueFreq1, trueFreq2),
      falseFrequencies = Seq(falseFreq1, falseFreq2),
      sampleWavelengths = 1.0,
      threshold = 0.0,
      numSamples = 1000,
      allowedError = 2.0)

//      display(outputDir, "in1: 0, in2: 0, c: 0", store000, false, Seq(period), listener, trueFreq1)
//      display(outputDir, "in1: 0, in2: 0, c: 1", store001, true, Seq(period), listener, trueFreq1)
//      display(outputDir, "in1: 0, in2: 1, c: 0", store010, false, Seq(period), listener, trueFreq1)
//      display(outputDir, "in1: 0, in2: 1, c: 1", store011, true, Seq(period), listener, trueFreq1)
//      display(outputDir, "in1: 1, in2: 0, c: 0", store100, false, Seq(period), listener, trueFreq1)
//      display(outputDir, "in1: 1, in2: 0, c: 1", store101, true, Seq(period), listener, trueFreq1)
//      display(outputDir, "in1: 1, in2: 1, c: 0", store110, false, Seq(period), listener, trueFreq1)
//      display(outputDir, "in1: 1, in2: 1, c: 1", store111, true, Seq(period), listener, trueFreq1)
//
//      listen("swap000", store000, Seq(period), listener)
//      listen("swap001", store001, Seq(period), listener)
//      listen("swap010", store010, Seq(period), listener)
//      listen("swap011", store011, Seq(period), listener)
//      listen("swap100", store100, Seq(period), listener)
//      listen("swap101", store101, Seq(period), listener)
//      listen("swap110", store110, Seq(period), listener)
//      listen("swap111", store111, Seq(period), listener)
  }

  private def constructive(numPeriods: Int, time: Double): Double = {
    2 * (numPeriods + 0.25)/time
  }

  private def destructive(numPeriods: Int, time: Double): Double = {
    numPeriods/time
  }

	private def listen(descr: String,
                     operation: Operation,
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