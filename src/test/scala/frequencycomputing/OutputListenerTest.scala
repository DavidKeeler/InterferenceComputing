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
    val c1 = Input(1.0, 0.0)
    val c2 = Input(1.05, 0.0)
    val c = Control(c1, c2)

    val in1 = Input(1.0, 0.0)
    val in2 = Input(1.1, 0.0)
    val cswap = ControlledSwap(in1, in2, c)

    val listener = new ShittyEstimateListener(trueFrequencies = Seq(1.025), falseFrequencies = Seq(1.05), sampleWavelengths = 5.0)
    val output: Option[OutputSymbol] = listener.listen(cswap, 10.0)
    val expectedOutput = Some(False)

    assert(output === expectedOutput)
  }
}



