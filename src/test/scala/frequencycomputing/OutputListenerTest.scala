package frequencycomputing

import org.scalatest.FunSuite

class OutputListenerTest extends FunSuite {
  private val trueFrequency = 2 * Math.PI * 1
  private val falseFrequency = 2 * Math.PI * 1.01
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
    def apply(x: Double): Double = Math.cos(frequency * x + phase)
    
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
    val output: Option[OutputSymbol] = listener.listen(TRUE_INPUT)
    val expectedOutput = Some(TRUE)
    
    assert(output === expectedOutput)
  }
  
  test("false") {
    val output: Option[OutputSymbol] = listener.listen(FALSE_INPUT)
    val expectedOutput = Some(FALSE)
    
    assert(output === expectedOutput)
  }
  
//  test("FALSE OR") {
//    val output: Option[OutputSymbol] = listener.listen(FALSE_INPUT + OR)
//    val expectedOutput = None
//
//    assert(output === expectedOutput)
//  }
//
//  test("TRUE OR TRUE") {
//    val output: Option[OutputSymbol] = listener.listen(TRUE_INPUT + TRUE_INPUT + OR)
//    val expectedOutput = Some(TRUE)
//
//    assert(output === expectedOutput)
//  }
//
//  test("FALSE OR FALSE") {
//    val output: Option[OutputSymbol] = listener.listen(FALSE_INPUT + FALSE_INPUT + OR)
//    val expectedOutput = Some(FALSE)
//
//    assert(output === expectedOutput)
//  }
//
//  test("FALSE OR TRUE") {
//    val output: Option[OutputSymbol] = listener.listen(TRUE_INPUT + FALSE_INPUT + OR)
//    val expectedOutput = Some(TRUE)
//
//    assert(output === expectedOutput)
//  }
//
//  test("TRUE AND TRUE") {
//    val output: Option[OutputSymbol] = listener.listen(TRUE_INPUT + TRUE_INPUT + AND)
//    val expectedOutput = Some(TRUE)
//
//    assert(output === expectedOutput)
//  }
//
//  test("FALSE AND FALSE") {
//    val output: Option[OutputSymbol] = listener.listen(FALSE_INPUT + FALSE_INPUT + AND)
//    val expectedOutput = Some(FALSE)
//
//    assert(output === expectedOutput)
//  }
//
//  test("FALSE AND TRUE") {
//    val output: Option[OutputSymbol] = listener.listen(FALSE_INPUT + TRUE_INPUT + AND)
//    val expectedOutput = Some(FALSE)
//
//    assert(output === expectedOutput)
//  }
//
//  test("NOT TRUE") {
//    val output: Option[OutputSymbol] = listener.listen(TRUE_INPUT + NOT)
//    val expectedOutput = Some(FALSE)
//
//    assert(output === expectedOutput)
//  }
//
//  test("NOT FALSE") {
//    val output: Option[OutputSymbol] = listener.listen(FALSE_INPUT + NOT)
//    val expectedOutput = Some(TRUE)
//
//    assert(output === expectedOutput)
//  }
//
//  test("TRUE NAND TRUE") {
//    val output: Option[OutputSymbol] = listener.listen(TRUE_INPUT + TRUE_INPUT + NAND)
//    val expectedOutput = Some(FALSE)
//
//    assert(output === expectedOutput)
//  }
//
//  test("TRUE NAND FALSE") {
//    val output: Option[OutputSymbol] = listener.listen(FALSE_INPUT + TRUE_INPUT + NAND)
//    val expectedOutput = Some(TRUE)
//
//    assert(output === expectedOutput)
//  }
//
//  test("FALSE NAND FALSE") {
//    val output: Option[OutputSymbol] = listener.listen(FALSE_INPUT + FALSE_INPUT + NAND)
//    val expectedOutput = Some(TRUE)
//
//    assert(output === expectedOutput)
//  }
//
//  test("TRUE NOR TRUE") {
//    val output: Option[OutputSymbol] = listener.listen(TRUE_INPUT + TRUE_INPUT + NOR)
//    val expectedOutput = Some(FALSE)
//
//    assert(output === expectedOutput)
//  }
//
//  test("TRUE NOR FALSE") {
//    val output: Option[OutputSymbol] = listener.listen(FALSE_INPUT + TRUE_INPUT + NOR)
//    val expectedOutput = Some(FALSE)
//
//    assert(output === expectedOutput)
//  }
//
//  test("FALSE NOR FALSE") {
//    val output: Option[OutputSymbol] = listener.listen(FALSE_INPUT + FALSE_INPUT + NOR)
//    val expectedOutput = Some(TRUE)
//
//    assert(output === expectedOutput)
//  }
//
//  test("TRUE AND TRUE AND TRUE") {
//    val output: Option[OutputSymbol] = listener.listen(TRUE_INPUT + TRUE_INPUT + TRUE_INPUT + AND + AND)
//    val expectedOutput = Some(TRUE)
//
//    assert(output === expectedOutput)
//  }
//
//  test("TRUE AND TRUE AND FALSE") {
//    val output: Option[OutputSymbol] = listener.listen(TRUE_INPUT + TRUE_INPUT + FALSE_INPUT + AND + AND)
//    val expectedOutput = Some(FALSE)
//
//    assert(output === expectedOutput)
//  }
//
//  test("TRUE AND FALSE AND FALSE") {
//    val output: Option[OutputSymbol] = listener.listen(TRUE_INPUT + FALSE_INPUT + FALSE_INPUT + AND + AND)
//    val expectedOutput = Some(FALSE)
//
//    assert(output === expectedOutput)
//  }
//
//  test("FALSE AND FALSE AND FALSE") {
//    val output: Option[OutputSymbol] = listener.listen(FALSE_INPUT + FALSE_INPUT + FALSE_INPUT + AND + AND)
//    val expectedOutput = Some(FALSE)
//
//    assert(output === expectedOutput)
//  }
//
//  test("FALSE AND FALSE AND") {
//    val output: Option[OutputSymbol] = listener.listen(FALSE_INPUT + FALSE_INPUT + AND + AND)
//    val expectedOutput = None
//
//    assert(output === expectedOutput)
//  }
}



