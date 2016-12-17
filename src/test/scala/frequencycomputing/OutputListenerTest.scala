package scratch

import org.scalatest.FunSuite

class OutputListenerTest extends FunSuite {
  private val trueFrequencies = Seq(100.0, 150.0)
  private val falseFrequencies = Seq(120.0, 170.0)

//  test("None") {
//    val listener = new ShittyEstimateListener(trueFrequencies, falseFrequencies)
//    val output: Option[OutputSymbol] = listener.listen(Input(trueFrequencies.head + 100), 0.0)
//
//    assert(output.isEmpty)
//  }
//
//  test("true") {
//    val listener = new ShittyEstimateListener(trueFrequencies, falseFrequencies)
//    val output: Option[OutputSymbol] = listener.listen(Input(trueFrequencies.head), 0.0)
//
//    assert(output.isDefined)
//    assert(output.get.value === true)
//  }
//
//  test("false") {
//    val listener = new ShittyEstimateListener(trueFrequencies, falseFrequencies)
//    val output: Option[OutputSymbol] = listener.listen(Input(falseFrequencies.head), 0.0)
//    val expectedOutput = Some(false)
//
//    assert(output.isDefined)
//    assert(output.get.value === false)
//  }
//
//  test("single beat") {
//    val beat = Beat(98, 102)
//
//    val listener = new ShittyEstimateListener(trueFrequencies, falseFrequencies)
//    val output: Option[OutputSymbol] = listener.listen(beat, 0.0)
//
//    assert(output.isDefined)
//    assert(output.get.value === true)
//  }

  test("a beat between beats") {
//    val in1 = Input(20.025, 0.0)
//    val in2 = Input(20.0, 0.0)
//
//    val c1 = Input(10.0, 0.0)
//    val c2 = Input(10.05, 0.0)
//    val control = Control(c1, c2)
//    val swap = ControlledSwap(in1, in2, control)
//
//    val listener = new ShittyEstimateListener(trueFrequencies = Seq(10.025), falseFrequencies = Seq(20.05), sampleWavelengths = 10.0)
//    val output: Option[OutputSymbol] = listener.listen(swap, 10.0)
//
//    assert(output.get.value === false)

    val beat1 = Beat(20.025, 10.0)
    val beat2 = Beat(20.0, 10.05)
    val trueFrequencies = Seq(10.025)
    val falseFrequencies = Seq(20.05)
    val time = 10.0

    val scale = 100
    val wtf = Beat(scale, scale + 2)

    val listener = new ShittyEstimateListener(trueFrequencies = Seq(scale + 1), falseFrequencies,
      sampleWavelengths = 10.0,
      threshold = 1.0,
      numSamples = 10000)
    val output = listener.listen(wtf, time = 0.0)

    assert(output.get.value === true)
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



