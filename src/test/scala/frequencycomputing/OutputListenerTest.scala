package scratch

import org.scalatest.FunSuite

class OutputListenerTest extends FunSuite {
  private val trueFrequency = 100.0
  private val falseFrequency = 120.0
  private val listener = new LeastSquaresListener(trueFrequency, falseFrequency)

  private val TRUE_INPUT = OutputWave(trueFrequency)
  private val FALSE_INPUT = OutputWave(falseFrequency)
  
  private object OutputWave {
    def apply(frequency: Double, phase: Double=0.0) = new OutputWave(frequency, phase)
  }
  
  private class OutputWave(frequency: Double, phase: Double = 0.0) extends (Double=>Double) {
    def apply(x: Double): Double = Math.cos(2 * Math.PI * frequency * x + phase)
    
    def +(otherFunc: Double=>Double): Double=>Double = {
      x: Double => this(x) + otherFunc(x)
    }
  }
  
  private implicit def functionToAdditiveFunction(f: Double=>Double): AdditiveFunction = new AdditiveFunction(f)
   
  private class AdditiveFunction(private val f: Double=>Double) extends (Double=>Double) {
    override def apply(x: Double): Double = f(x)
     
    def +(that: Double=>Double): AdditiveFunction = {
      x: Double => this(x) + that(x)
    }
  }
  
  test("true") {
    val output: Option[OutputSymbol] = listener.listen(TRUE_INPUT, 0.0)
    val expectedOutput = Some(TRUE)

    assert(output === expectedOutput)
  }
  
  test("false") {
    val output: Option[OutputSymbol] = listener.listen(FALSE_INPUT, 0.0)
    val expectedOutput = Some(FALSE)
    
    assert(output === expectedOutput)
  }

  test("beat") {
    val input1 = OutputWave(trueFrequency + 5)
    val input2 = OutputWave(trueFrequency - 5)
    val output: Option[OutputSymbol] = listener.listen(input1 + input2, 0.0)
    val expectedOutput = Some(TRUE)

    assert(output === expectedOutput)
  }
}



