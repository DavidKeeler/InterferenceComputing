package scratch

import org.scalatest.FunSuite

class OutputListenerTest extends FunSuite {
  private val trueFrequencies = Seq(100.0, 150.0)
  private val falseFrequencies = Seq(120.0, 170.0)

  test("true") {
    val listener = new ShittyEstimateListener(trueFrequencies, falseFrequencies)
    val output: Option[OutputSymbol] = listener.listen(Input(trueFrequencies.head), 0.0)
    val expectedOutput = Some(True)

    assert(output === expectedOutput)
  }
  
  test("false") {
    val listener = new ShittyEstimateListener(trueFrequencies, falseFrequencies)
    val output: Option[OutputSymbol] = listener.listen(Input(falseFrequencies.head), 0.0)
    val expectedOutput = Some(False)

    assert(output === expectedOutput)
  }

  test("single beat") {
    val c1 = Input(98, 0.0)
    val c2 = Input(102, 0.0)
    val c = Control(c1, c2)

    val listener = new ShittyEstimateListener(trueFrequencies, falseFrequencies)
    val output: Option[OutputSymbol] = listener.listen(c, 0.0)
    val expectedOutput = Some(True)

    assert(output === expectedOutput)
  }

  test("a beat between beats") {
    val in1 = Input(20.025, 0.0)
    val in2 = Input(20.0, 0.0)

    val c1 = Input(10.0, 0.0)
    val c2 = Input(10.05, 0.0)
    val control = Control(c1, c2)
    val swap = ControlledSwap(in1, in2, control)

    val listener = new ShittyEstimateListener(trueFrequencies = Seq(10.025), falseFrequencies = Seq(20.05), sampleWavelengths = 10.0)
    val output: Option[OutputSymbol] = listener.listen(swap, 10.0)
    val expectedOutput = Some(False)

    assert(output === expectedOutput)
  }
}



