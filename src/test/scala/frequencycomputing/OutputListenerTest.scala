package frequencycomputing

import org.scalatest.FunSuite

class OutputListenerTest extends FunSuite {
  private val trueFrequency = 2 * Math.PI * 1
  private val falseFrequency = 2 * Math.PI * 1.1
  private val listener = new LeastSquaresListener(trueFrequency, falseFrequency)

  private val TRUE_INPUT = OutputWave(trueFrequency)
  private val FALSE_INPUT = OutputWave(falseFrequency)
  private val OR = OutputWave(falseFrequency, Math.PI)
  private val AND = OutputWave(trueFrequency, Math.PI)
  private val NOT = OutputWave(trueFrequency, Math.PI) + OutputWave(falseFrequency, Math.PI)
  private val NAND = AND + NOT
  private val NOR = OR + NOT
  
  private object OutputWave {
    def apply(frequency: Double, phase: Double=0.0) = new OutputWave(frequency, phase)
  }
  
  private class OutputWave(frequency: Double, phase: Double) extends (Double=>Double) {
    def apply(x: Double): Double = Math.sin(frequency * x + phase)
    
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
  
  ignore("true") {
    val output: Option[OutputSymbol] = listener.listen(TRUE_INPUT)
    val expectedOutput = Some(TRUE)
    
    assert(output === expectedOutput)
  }
  
  ignore("false") {
    val output: Option[OutputSymbol] = listener.listen(FALSE_INPUT)
    val expectedOutput = Some(FALSE)
    
    assert(output === expectedOutput)
  }
  
  ignore("No output") {
    val output: Option[OutputSymbol] = listener.listen(OutputWave(falseFrequency + 200))
    val expectedOutput = None
    
    assert(output === expectedOutput)
  }

  ignore("TRUE OR TRUE") {
    val output: Option[OutputSymbol] = listener.listen(TRUE_INPUT + TRUE_INPUT + OR)
    val expectedOutput = Some(TRUE)
    
    assert(output === expectedOutput)
  }
  
  ignore("FALSE OR FALSE") {
    val output: Option[OutputSymbol] = listener.listen(FALSE_INPUT + FALSE_INPUT + OR)
    val expectedOutput = Some(FALSE)
    
    assert(output === expectedOutput)
  }
  
  ignore("FALSE OR TRUE") {
    val output: Option[OutputSymbol] = listener.listen(TRUE_INPUT + FALSE_INPUT + OR)
    val expectedOutput = Some(TRUE)
    
    assert(output === expectedOutput)
  }
  
  ignore("TRUE AND TRUE") {
    val output: Option[OutputSymbol] = listener.listen(TRUE_INPUT + TRUE_INPUT + AND)
    val expectedOutput = Some(TRUE)
    
    assert(output === expectedOutput)
  }
  
  ignore("FALSE AND FALSE") {
    val output: Option[OutputSymbol] = listener.listen(FALSE_INPUT + FALSE_INPUT + AND)
    val expectedOutput = Some(FALSE)
    
    assert(output === expectedOutput)
  }
  
  ignore("FALSE AND TRUE") {
    val output: Option[OutputSymbol] = listener.listen(FALSE_INPUT + TRUE_INPUT + AND)
    val expectedOutput = Some(FALSE)
    
    assert(output === expectedOutput)
  }
  
  ignore("NOT TRUE") {
    val output: Option[OutputSymbol] = listener.listen(TRUE_INPUT + NOT)
    val expectedOutput = Some(FALSE)
    
    assert(output === expectedOutput)
  }
  
  ignore("NOT FALSE") {
    val output: Option[OutputSymbol] = listener.listen(FALSE_INPUT + NOT)
    val expectedOutput = Some(TRUE)
    
    assert(output === expectedOutput)
  }
  
  ignore("TRUE NAND TRUE") {
    val output: Option[OutputSymbol] = listener.listen(TRUE_INPUT + TRUE_INPUT + NAND)
    val expectedOutput = Some(FALSE)
    
    assert(output === expectedOutput)
  }
  
  ignore("TRUE NAND FALSE") {
    val output: Option[OutputSymbol] = listener.listen(FALSE_INPUT + TRUE_INPUT + NAND)
    val expectedOutput = Some(TRUE)
    
    assert(output === expectedOutput)
  }
  
  ignore("FALSE NAND FALSE") {
    val output: Option[OutputSymbol] = listener.listen(FALSE_INPUT + FALSE_INPUT + NAND)
    val expectedOutput = Some(TRUE)
    
    assert(output === expectedOutput)
  }
  
  ignore("TRUE NOR TRUE") {
    val output: Option[OutputSymbol] = listener.listen(TRUE_INPUT + TRUE_INPUT + NOR)
    val expectedOutput = Some(FALSE)
    
    assert(output === expectedOutput)
  }
  
  ignore("TRUE NOR FALSE") {
    val output: Option[OutputSymbol] = listener.listen(FALSE_INPUT + TRUE_INPUT + NOR)
    val expectedOutput = Some(FALSE)
    
    assert(output === expectedOutput)
  }
  
  ignore("FALSE NOR FALSE") {
    val output: Option[OutputSymbol] = listener.listen(FALSE_INPUT + FALSE_INPUT + NOR)
    val expectedOutput = Some(TRUE)
    
    assert(output === expectedOutput)
  }
  
  ignore("TRUE AND TRUE AND TRUE") {
    val output: Option[OutputSymbol] = listener.listen(TRUE_INPUT + TRUE_INPUT + TRUE_INPUT + AND + AND)
    val expectedOutput = Some(TRUE)
    
    assert(output === expectedOutput)
  }
  
  ignore("TRUE AND TRUE AND FALSE") {
    val output: Option[OutputSymbol] = listener.listen(TRUE_INPUT + TRUE_INPUT + FALSE_INPUT + AND + AND)
    val expectedOutput = Some(FALSE)
    
    assert(output === expectedOutput)
  }  
  
  ignore("TRUE AND FALSE AND FALSE") {
    val output: Option[OutputSymbol] = listener.listen(TRUE_INPUT + FALSE_INPUT + FALSE_INPUT + AND + AND)
    val expectedOutput = Some(FALSE)
    
    assert(output === expectedOutput)
  }
  
  test("FALSE AND FALSE AND FALSE") {
    val output: Option[OutputSymbol] = listener.listen(FALSE_INPUT + FALSE_INPUT + FALSE_INPUT + AND + AND)
    val expectedOutput = Some(FALSE)
    
    assert(output === expectedOutput)
  }
}



