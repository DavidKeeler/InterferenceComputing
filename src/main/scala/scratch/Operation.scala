//package scratch
//
//import breeze.linalg.DenseVector
//import breeze.math.Complex
//
//import scala.collection.mutable
//import breeze.signal._
//
//sealed trait WhichInput
//case object Input1 extends WhichInput
//case object Input2 extends WhichInput
//case object ControlInput extends WhichInput
//
//class Circuit()(implicit period: Double= 1.0) {
//  private var t = 0
//  private var id = -1
//  private def nextId: Int = {
//    id += 1
//    id
//  }
//
//  var root = new Gate(nextId, parent=None, period=period, new Params())
//  private val allGates = mutable.Map[Int, Set[Gate]](root.id -> Set(root))
//
//  private val samples: Array[Double] = (0 until 10000).map(index => root(index/period)).toArray
//  private val listener = new Listener(root.params.outTrue, root.params.outFalse, 2.0, 10000.0)
//
//  def ids: Set[Int] = allGates.keySet.toSet
//
//  def changeInput(which: WhichInput, gateId: Int): Boolean = {
//    val gates = allGates(gateId)
//    val inputs = gates.map(gate => gate.getInput(which))
//    val inverse = Array[Double](samples.size).map {
//      index => inputs.foldLeft(0.0) { case (sum, input) => sum - input(index/period) }
//    }
//
//    gates.map(_.changeValue(which))
//
//    val updatedInputs = gates.map(gate => gate.getInput(which))
//    val updated = Array[Double](samples.size).map {
//      index => updatedInputs.foldLeft(0.0) { case (sum, input) => sum + input(index/period) }
//    }
//
//    for (i <- 0 to samples.size) {
//      samples(i) += inverse(i) + updated(i)
//    }
//
//    listener(samples)
//  }
//
//  def toGate(gateId: Int, which: WhichInput): Boolean = {
//    // Find the inverse of the existing input
//    val gates = allGates(gateId)
//    val inputs = gates.map(gate => gate.getConnection(which))
//    val inverse = (0 until samples.size).map {
//      index => inputs.foldLeft(0.0) { case (sum, input) => sum - input(index/period) }
//    }.toArray
//
//    // Change to gate
//    val id = nextId
//    val addedGates = gates.flatMap(gate => gate.replaceWithGate(which, id))
//    allGates.put(addedGates.head.id, addedGates)
//
//    // Find the new signal
//    val updated = (0 until samples.size).map {
//      index => addedGates.foldLeft(0.0) { case (sum, input) => sum + input(index/period) }
//    }.toArray
//
//    // Play the inverse and new signal and listen for output
//    for (i <- 0 until samples.size) {
//      samples(i) += inverse(i) + updated(i)
//    }
//
//    listener(samples)
//  }
//
//  private def newInputs(startingTrue: Double, startingFalse: Double): (Double, Double) = {
//    val inputPairs = allGates.values.flatten.flatMap { gate =>
//      Set((gate.input1.outTrue, gate.input1.outFalse),
//        (gate.input2.outTrue, gate.input2.outFalse),
//        (gate.control.outTrue, gate.control.outFalse),
//        (gate.desControl.outTrue, gate.desControl.outFalse))
//    }.toSet
//
//    var first = (startingTrue, startingFalse)
//    while (inputPairs.contains(first)) {
//      first = (first._1 + 1, first._2 + 1)
//    }
//
//    var second = (first._1 + 1, first._2 + 1)
//    while (inputPairs.contains(second)) {
//      second = (second._1 + 1, second._2 + 1)
//    }
//
//    (first._1, second._1)
//  }
//
//  def print(verify: Boolean = false)(implicit period: Double) {
//    allGates.values.flatten.foreach { gate =>
//        println("Id: " + gate.id)
//        println("Params: " + gate.params)
//        println("inTrue1: " + gate.params.inTrue1 + " inFalse1: " + gate.params.inFalse1)
//        println("inTrue2: " + gate.params.inTrue2 + " inFalse2: " + gate.params.inFalse2)
//        println("controlTrue: " + gate.params.controlTrue + " controlFalse: " + gate.params.controlFalse)
//        println("controlTrueDes: " + gate.params.controlTrueDes + " controlFalseDes: " + gate.params.controlFalseDes)
//        println
//
//        println("outTrue: " + gate.params.outTrue + " outFalse: " + gate.params.outFalse)
//        println("outDesTrue: " + gate.params.outDesTrue + " outDesFalse: " + gate.params.outDesFalse)
//        println
//
//        if (verify) {
//          println("Verification")
//          val outTrue1 = (gate.params.inTrue1 + gate.params.controlTrue)/2
//          val outTrue2 = (gate.params.inTrue2 + gate.params.controlFalse)/2
//          println("outTrue1: " + outTrue1 + " outTrue2: " + outTrue2)
//
//          val outFalse1 = (gate.params.inFalse1 + gate.params.controlTrue)/2
//          val outFalse2 = (gate.params.inFalse2 + gate.params.controlFalse)/2
//          println("outFalse1: " + outFalse1 + " outFalse2: " + outFalse2)
//
//          val outTrueDes1 = (gate.params.inTrue2 + gate.params.controlTrueDes)/2
//          val outTrueDes2 = (gate.params.inTrue1 + gate.params.controlFalseDes)/2
//          println("outTrueDes1: " + outTrueDes1 + " outTrueDes2: " + outTrueDes2)
//
//          val outFalseDes1 = (gate.params.inFalse1 + gate.params.controlFalseDes)/2
//          val outFalseDes2 = (gate.params.inFalse2 + gate.params.controlTrueDes)/2
//          println("outFalseDes1: " + outFalseDes1 + " outFalseDes2: " + outFalseDes2)
//          println
//
//          val beatTrueCon1 = Math.abs(gate.params.inTrue1 - gate.params.controlTrue)/2
//          val beatTrueCon2 = Math.abs(gate.params.inTrue2 - gate.params.controlFalse)/2
//          println("beatTrueCon1: " + beatTrueCon1 + " beatTrueCon2: " + beatTrueCon2)
//
//          val beatFalseCon1 = Math.abs(gate.params.inFalse1 - gate.params.controlTrue)/2
//          val beatFalseCon2 = Math.abs(gate.params.inFalse2 - gate.params.controlFalse)/2
//          println("beatFalseCon1: " + beatFalseCon1 + " beatFalseCon2: " + beatFalseCon2)
//
//          val beatTrueDes1 = Math.abs(gate.params.inTrue2 - gate.params.controlTrueDes)/2
//          val beatTrueDes2 = Math.abs(gate.params.inTrue1 - gate.params.controlFalseDes)/2
//          println("beatTrueDes1: " + beatTrueDes1 + " beatTrueDes2: " + beatTrueDes2)
//
//          val beatFalseDes1 = Math.abs(gate.params.inFalse2 - gate.params.controlTrueDes)/2
//          val beatFalseDes2 = Math.abs(gate.params.inFalse1 - gate.params.controlFalseDes)/2
//          println("beatFalseDes1: " + beatFalseDes1 + " beatFalseDes2: " + beatFalseDes2)
//          println
//        }
//      }
//    }
//
//  class Gate(val id: Int,
//             val parent: Option[Gate],
//             implicit val period: Double,
//             val params: Params) extends Connection {
//    import params._
//
//    def outTrue: Double = params.outTrue
//    def outFalse: Double = params.outFalse
//
//    var input1: Connection = new Input(false, amp, inTrue1, inFalse1)
//    var input2: Connection = new Input(false, amp, inTrue2, inFalse2)
//    var control: Connection = new Input(true, amp, controlTrue, controlFalse)
//    var desControl: Connection = new Input(true, amp, controlTrueDes, controlFalseDes)
//
//    override def toString = s"Gate(id: $id, parent: ${parent.map(_.id)} period: $period params: $params)"
//
//    def apply(t: Double): Double = input1(t) + input2(t) + control(t) + desControl(t)
//
//    def amp: Double = {
//      if (parent.isEmpty)
//        return 1.0
//
//      parent.get.amp/2
//    }
//
//    def getInput(which: WhichInput): Input = getConnection(which).asInstanceOf[Input]
//    def getGate(which: WhichInput): Gate = getConnection(which).asInstanceOf[Gate]
//    def getConnection(which: WhichInput): Connection = {
//      which match {
//        case Input1 => input1
//        case Input2 => input2
//        case ControlInput => control
//        case _ => throw new IllegalArgumentException
//      }
//    }
//
//    def changeValue(which: WhichInput) {
//      which match {
//        case Input1 if input1.isInstanceOf[Input] => input1.asInstanceOf[Input].changeValue
//        case Input2 if input2.isInstanceOf[Input] => input2.asInstanceOf[Input].changeValue
//        case ControlInput if control.isInstanceOf[Input] && desControl.isInstanceOf[Input] =>
//          control.asInstanceOf[Input].changeValue
//          desControl.asInstanceOf[Input].changeValue
//      }
//    }
//
//    def replaceWithGate(which: WhichInput, newId: Int): Set[Gate] = {
//      val createdGates = which match {
//        case Input1 => {
//          val in = newInputs(root.params.outTrue, root.params.outTrue + 2 * params.delta)
//          val p = new Params(params.in1, in._1, in._2, params.destruct, 2 * params.delta)
//          input1 = new Gate(newId, Some(this), period, p)
//          Set(input1.asInstanceOf[Gate])
//        }
//        case Input2 => {
//          val in = newInputs(root.params.outTrue, root.params.outTrue + 2 * params.delta)
//          val p = new Params(params.in2, in._1, in._2, params.destruct, 2 * params.delta)
//          input2 = new Gate(newId, Some(this), period, p)
//          Set(input2.asInstanceOf[Gate])
//        }
//        case ControlInput => {
//          var in = newInputs(root.params.outTrue, root.params.outTrue + (params.controlFalse - params.controlTrue))
//          val p1 = new Params(params.controlTrue, in._1, in._2, params.destruct, (params.controlFalse - params.controlTrue).toInt)
//          control = new Gate(newId, Some(this), period, p1)
//
//          in = newInputs(root.params.outTrue + 0.5, root.params.outTrue + (params.controlFalseDes - params.controlTrueDes) + 0.5)
//          val p2 = new Params(params.controlTrueDes, in._1, in._2, params.destruct, (params.controlFalseDes - params.controlTrueDes).toInt)
//
//          desControl = new Gate(newId, Some(this), period, p2)
//          Set(control.asInstanceOf[Gate], desControl.asInstanceOf[Gate])
//        }
//      }
//
//      createdGates
//    }
//
//    def replaceWithInput(which: WhichInput): Input = {
//      which match {
//        case Input1 => input1 = new Input(false, amp, input1.outTrue, input1.outFalse)
//        case Input2 => input2 = new Input(false, amp, input2.outTrue, input2.outFalse)
//        case ControlInput => {
//          control = new Input(true, amp, control.outTrue, control.outFalse)
//          desControl = new Input(true, amp, desControl.outTrue, desControl.outFalse)
//        }
//      }
//
//      getInput(which)
//    }
//  }
//
//  trait Connection {
//    def apply(t: Double): Double
//    def outTrue: Double
//    def outFalse: Double
//  }
//
//  class Input(var value: Boolean, amp: Double, val outTrue: Double, val outFalse: Double) extends Connection {
//    override def apply(t: Double): Double = value match {
//      case true => amp * Math.cos(2 * Math.PI * outTrue * t)
//      case false => amp * Math.cos(2 * Math.PI * outFalse * t)
//    }
//
//    def changeValue { !value }
//  }
//
//  class Params(val out: Double = 100, val in1: Double = 100, val in2: Double = 101, val destruct: Double = 100, val delta: Int = 1) {
//    override def toString = s"($out, $in1, $in2, $destruct, $delta)"
//
//    def outTrue(implicit period: Double): Double = out
//    def outFalse(implicit period: Double): Double = out + delta
//
//    def inTrue1(implicit period: Double): Double = in1
//    def inFalse1(implicit period: Double): Double = in1 + 2 * delta
//
//    def inTrue2(implicit period: Double): Double = in2
//    def inFalse2(implicit period: Double): Double = in2 + 2 * delta
//
//    def controlTrue(implicit period: Double): Double = 2 * out - in1
//    def controlFalse(implicit period: Double): Double = 2 * out - in2
//
//    def outDesTrue(implicit period: Double): Double = destruct + 0.25
//    def outDesFalse(implicit period: Double): Double = destruct + delta + 0.25
//
//    def controlTrueDes(implicit period: Double): Double = 2 * destruct - in2 + 0.5
//    def controlFalseDes(implicit period: Double): Double = 2 * destruct - in1 + 0.5
//
//    def inputFreqs(implicit period: Double): Set[Double] = Set(inTrue1, inTrue2, inFalse1, inFalse2, controlTrue, controlFalse, controlTrueDes, controlFalseDes)
//  }
//}
//
//
//
//
//
//
//
//
//
//
//
//
//
//
//
//
