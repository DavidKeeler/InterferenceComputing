package scratch

import breeze.linalg.DenseVector

import scala.collection.mutable
import breeze.signal._

sealed trait WhichInput
case object Input1 extends WhichInput
case object Input2 extends WhichInput
case object ControlInput extends WhichInput

class Circuit(period: Double= 1.0, initialParams: Params = new Params) {
  implicit val periodImpl = period

  private var t = 0

  val root = new Gate(id=Gate.nextId, parent=None, period=period, initialParams)
  val inputGates = mutable.Map[Int, Set[Gate]](root.id -> Set(root))

  private val samples: Array[Double] = (0 until 10000).map(index => root(index/period)).toArray
  private val listener = Listener(root.params.outTrue, root.params.outFalse, period)

  def changeInput(which: WhichInput, gateId: Int): Boolean = {
    val gates = inputGates(gateId)
    val inputs = gates.map(gate => gate.getInput(which))
    val inverse = Array[Double](samples.size).map {
      index => inputs.foldLeft(0.0) { case (sum, input) => sum - input(index/period) }
    }

    gates.map(_.changeValue(which))

    val updatedInputs = gates.map(gate => gate.getInput(which))
    val updated = Array[Double](samples.size).map {
      index => updatedInputs.foldLeft(0.0) { case (sum, input) => sum + input(index/period) }
    }

    for (i <- 0 to samples.size) {
      samples(i) += inverse(i) + updated(i)
    }

    listener(samples)
  }

  def removeGate(gateId: Int): Boolean = {
    val gates = inputGates(gateId)
    if (gates.foldLeft(true){ case (isNotRoot, gate) => isNotRoot && gate.parent.isDefined })
      throw new IllegalArgumentException

    val inverse = Array[Double](samples.size).map {
      index => gates.foldLeft(0.0) { case (sum, gate) => sum - gate(index/period) }
    }

    val updatedInputs = gates.flatMap {
      gate => gate.parent.map(_.replaceWithInput(gate))
    }

    val updated = Array[Double](samples.size).map {
      index => updatedInputs.foldLeft(0.0) { case (sum, input) => sum + input(index/period) }
    }

    for (i <- 0 to samples.size) {
      samples(i) += inverse(i) + updated(i)
    }

    listener(samples)
  }

  def toGate(gateId: Int, which: WhichInput): Boolean = {
    // Find the inverse of the existing input
    val gates = inputGates(gateId)
    val inputs = gates.map(gate => gate.getInput(which))
    val inverse = (0 until samples.size).map {
      index => inputs.foldLeft(0.0) { case (sum, input) => sum - input(index/period) }
    }.toArray

    // Change to gate
    val addedGates = gates.map(gate => gate.replaceWithGate(which, gateId))
    inputGates ++ addedGates.map(gate => (gate.id, gate))

    // Find the new signal
    val updated = (0 until samples.size).map {
      index => addedGates.foldLeft(0.0) { case (sum, input) => sum + input(index/period) }
    }.toArray

    // Play the inverse and new signal and listen for output
    for (i <- 0 until samples.size) {
      samples(i) += inverse(i) + updated(i)
    }

    listener(samples)
  }
}

case class Listener(trueFreq: Double, falseFreq: Double, period: Double) {

  def apply(signal: Array[Double]): Boolean = {
    val samples = signal.slice(0, 200)          // TODO: this doesn't seem right
    val sampleFreq = period/signal.length

    val frequencyVector = fourierTr(DenseVector(samples))

    val trueOutput = frequencyVector.apply(trueFreq.asInstanceOf[Int])
    val falseOutput = frequencyVector.apply(falseFreq.asInstanceOf[Int])

    trueOutput.real > falseOutput.real
  }
}

trait Connection {
  def apply(t: Double): Double
  def outTrue: Double
  def outFalse: Double
}

object Control {
  def apply(value: Boolean, amp: Double, conTrue: Double, conFalse: Double, desTrue: Double, desFalse: Double, parent: Gate): Control =
    new Control(new Input(value, amp, conTrue, conFalse), new Input(value, amp, desTrue, desFalse), parent)
}
class Control(var construct: Connection, var destruct: Connection, parent: Gate) extends Connection {
  def apply(t: Double): Double = construct(t) + destruct(t)

  def isInput: Boolean = construct.isInstanceOf[Input] && destruct.isInstanceOf[Input]

  def replaceWithGate(id: Int) {
  if (!construct.isInstanceOf[Input] || !destruct.isInstanceOf[Input])
    throw new IllegalArgumentException

    construct = Gate(parent, parent.period)
    destruct = Gate(parent, parent.period)
  }

  def replaceWithInput {
    if (!construct.isInstanceOf[Gate] || ! destruct.isInstanceOf[Gate])
      throw new IllegalArgumentException

    construct = new Input(false, parent.amp/2.0, construct.outTrue, construct.outFalse)
    destruct = new Input(false, parent.amp/2.0, destruct.outTrue, destruct.outFalse)
  }

  def outTrue = construct.outTrue
  def outFalse = construct.outFalse
  def changeValue {
    construct match {
      case input: Input =>
        construct.asInstanceOf[Input].changeValue
        destruct.asInstanceOf[Input].changeValue
      case _ => throw new IllegalStateException
    }
  }
}

class Input(var value: Boolean, amp: Double, val outTrue: Double, val outFalse: Double) extends Connection {
   override def apply(t: Double): Double = value match {
     case true => amp * Math.cos(2 * Math.PI * outTrue * t)
     case false => amp * Math.cos(2 * Math.PI * outFalse * t)
   }

  def changeValue { !value }
}

class Params(val a: Int = 100, val b: Int = 2, val c: Int = 2, val d: Int = 0, val e: Int = 0) extends (Int, Int, Int, Int, Int)(a, b, c, d, e) {
  override def toString = s"($a, $b, $c, $d, $e)"

  def outTrue(implicit period: Double): Double = a/period
  def outFalse(implicit period: Double): Double = (a + b)/period

  def inTrue1(implicit period: Double): Double = (a - c)/period
  def inFalse1(implicit period: Double): Double = inTrue1 + 2 * b/period

  def inTrue2(implicit period: Double): Double = (a - d)/period
  def inFalse2(implicit period: Double): Double = inTrue2 + 2 * b/period

  def controlTrue(implicit period: Double): Double = (a + c)/period
  def controlFalse(implicit period: Double): Double = (a + d)/period

  def outDesTrue(implicit period: Double): Double = (a + e + 0.75) / period
  def outDesFalse(implicit period: Double): Double = outDesTrue + b/period

  def controlTrueDes(implicit period: Double): Double = 2 * outDesTrue - inTrue2
  def controlFalseDes(implicit period: Double): Double = 2 * outDesTrue - inTrue1

  def inputFreqs(implicit period: Double): Set[Double] = Set(inTrue1, inTrue2, inFalse1, inFalse2, controlTrue, controlFalse, controlTrueDes, controlFalseDes)
}

object Gate {
  private var id = 0
  def nextId: Int = {
    id += 1
    id
  }

  def apply(parent: Gate, period: Double): Gate = {
    val params = new Params(nextId, parent.params.b + 1, parent.params.c + 1, 0, 0)
    new Gate(id, Some(parent), period, params)
  }
}

class Gate(val id: Int,
           val parent: Option[Gate],
           implicit val period: Double,
           val params: Params) extends Connection {
  import params._

  def outTrue: Double = params.outTrue
  def outFalse: Double = params.outFalse

  var input1: Connection = new Input(false, amp, inTrue1, inFalse1)
  var input2: Connection = new Input(false, amp, inTrue2, inFalse2)
  val control = Control(false, amp, controlTrue, controlFalse, controlTrueDes, controlFalseDes, this)

  override def toString = s"Gate(id: $id, parent: ${parent.map(_.id)} period: $period params: $params)"

  def apply(t: Double): Double = input1(t) + input2(t) + control(t)

  def amp: Double = {
    if (parent.isEmpty)
      return 1.0

    parent.get.amp/2
  }

  def getInput(which: WhichInput): Input = getConnection(which).asInstanceOf[Input]
  def getGate(which: WhichInput): Gate = getConnection(which).asInstanceOf[Gate]
  def getConnection(which: WhichInput): Connection = {
    which match {
      case Input1 => input1
      case Input2 => input2
      case ControlInput => control
      case _ => throw new IllegalArgumentException
    }
  }

  def changeValue(which: WhichInput) {
    which match {
      case Input1 if input1.isInstanceOf[Input] => input1.asInstanceOf[Input].changeValue
      case Input2 if input2.isInstanceOf[Input] => input2.asInstanceOf[Input].changeValue
      case ControlInput if control.isInput => control.changeValue
    }
  }

  def which(con: Connection): WhichInput = {
    if (con == input1)
      return Input1
    else if (con == input2)
      return Input2
    else if (con == control)
      return ControlInput
    else
      throw new IllegalArgumentException
  }

  def replaceWithGate(con: Connection, id: Int): Gate = replaceWithGate(which(con), id)
  def replaceWithGate(which: WhichInput, id: Int): Gate = {
    which match {
      case Input1 => input1 = Gate(this, period)
      case Input2 => input2 = Gate(this, period)
      case ControlInput => control.replaceWithGate(id)
    }

    getGate(which)
  }

  def replaceWithInput(con: Connection): Input = replaceWithInput(which(con))
  def replaceWithInput(which: WhichInput): Input = {
    which match {
      case Input1 => input1 = new Input(false, amp, input1.outTrue, input1.outFalse)
      case Input2 => input2 = new Input(false, amp, input2.outTrue, input2.outFalse)
      case ControlInput => control.replaceWithInput
    }

    getInput(which)
  }
}

abstract class Operation {
  def apply(t: Double): Double

  def f: (Double=>Double) = {
    t: Double => this(t)
  }

  def +(that: Operation): Operation = {
    val otherThis = this
    new Operation {
      def apply(t: Double): Double = otherThis(t) + that(t)
    }
  }
}

case class SimpleInput(freq: Double, phase: Double=0.0, amplitude: Double=1.0) extends Operation {
  def apply(t: Double): Double = {
    amplitude * Math.cos(2 * Math.PI * freq * t + phase)
  }
}

object ControlOp {
  def apply(con: Double, des: Double): ControlOp = ControlOp(SimpleInput(con), SimpleInput(des))

  def apply(con: Double, des: Double, amp: Double): ControlOp =
    ControlOp(SimpleInput(con, amplitude=amp), SimpleInput(des, amplitude=amp))
}

case class ControlOp(con: Operation, des: Operation) extends Operation {
  def apply(t: Double): Double = con(t) + des(t)
}

object ControlledStore {
  def apply(in1: Double, in2: Double, control: ControlOp): ControlledStore = ControlledStore(SimpleInput(in1), SimpleInput(in2), control)
}

case class ControlledStore(in1: SimpleInput, in2: SimpleInput, c: ControlOp) extends Operation {
  def apply(t: Double): Double = c(t) + in1(t) + in2(t)
}

