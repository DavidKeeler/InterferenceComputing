package scratch

import java.util.Random

import org.scalatest.FunSuite

import scalax.chart.api._

class OutputListenerTest extends FunSuite {
  private val trueFrequencies = Seq(100.0)
  private val falseFrequencies = Seq(105.0)
  private val listener = new ShittyEstimateListener(
    trueFrequencies = trueFrequencies,
    falseFrequencies = falseFrequencies,
    sampleWavelengths = 5.0,
    threshold = 0.0,
    numSamples = 10000,
    allowedError = 2.0)

  test("None") {
    val output = listener.listen(Input(trueFrequencies.head + 10), 0.0)

    assert(output.value.isEmpty)
  }

  test("true") {
    val output = listener.listen(Input(trueFrequencies.head), 0.0)

    assert(output.value.isDefined)
    assert(output.value.get === true)
  }

  test("false") {
    val output = listener.listen(Input(falseFrequencies.head), 0.0)
    assert(output.value.isDefined)
    assert(output.value.get === false)
  }

  test("single beat") {
    val beat = Beat(98, 102)
    val output = listener.listen(beat, 0.0)

    assert(output.value.isDefined)
    assert(output.value.get === true)
  }

  test("a beat between beats") {
    val beat1 = Beat(99, 101)
    val beat2 = Beat(104.5, 105.5)
    val output = listener.listen(beat1 + beat2, time = 0.5)

    assert(output.value.isDefined)
    assert(output.value.get === true)
  }

  object Beat {
    def apply(freq1: Double, freq2: Double) = new Beat(Input(freq1), Input(freq2))
  }

  case class Beat(freq1: Input, freq2: Input) extends Operation {
    def apply(t: Double): Double = {
      freq1(t) + freq2(t)
    }
  }
}



