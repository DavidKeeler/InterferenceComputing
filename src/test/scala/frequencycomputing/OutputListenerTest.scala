package scratch

import org.scalatest.FunSuite

class OutputListenerTest extends FunSuite {
  private val trueFrequencies = Seq(100.0, 150.0)
  private val falseFrequencies = Seq(120.0, 170.0)
  private val listener = new ShittyEstimateListener(trueFrequencies, falseFrequencies)

  test("true") {
    val output: Option[OutputSymbol] = listener.listen(Input(trueFrequencies.head), 0.0)
    val expectedOutput = Some(True)

    assert(output === expectedOutput)
  }
  
  test("false") {
    val output: Option[OutputSymbol] = listener.listen(Input(falseFrequencies.head), 0.0)
    val expectedOutput = Some(False)

    assert(output === expectedOutput)
  }

  test("single beat") {
    val c1 = Input(98, 0.0)
    val c2 = Input(102, 0.0)
    val c = Control(c1, c2)

    val output: Option[OutputSymbol] = listener.listen(c, 0.0)
    val expectedOutput = Some(True)

    assert(output === expectedOutput)
  }
}



