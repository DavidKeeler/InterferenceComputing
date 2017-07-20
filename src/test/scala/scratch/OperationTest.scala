package scratch

import com.typesafe.scalalogging.slf4j.Logger
import org.scalatest.FunSuite
import org.slf4j.LoggerFactory

class OperationTest extends FunSuite {
  System.setProperty("org.slf4j.simpleLogger.logFile", "System.out")
  val logger = Logger(LoggerFactory.getLogger("OperationTest"))

  test("Single Gate") {

    implicit val period = 1.0

    val circuit = new Circuit(period, new Params(100, 2, 2, 0, 0))
    circuit.toGate("0", Input1)

    val ids = circuit.inputGates.keys

    val gates = circuit.inputGates.flatMap(_._2)
    val root = gates.head

    logger.info("Id: " + root.id)
    logger.info("inTrue1: " + root.params.inTrue1 + " inFalse1: " + root.params.inFalse1)
    logger.info("inTrue2: " + root.params.inTrue2 + " inFalse2: " + root.params.inFalse2)
    logger.info("controlTrue: " + root.params.controlTrue + " controlFalse: " + root.params.controlFalse)
    logger.info("controlTrueDes: " + root.params.controlTrueDes + " controlFalseDes: " + root.params.controlFalseDes)

    logger.info("outTrue: " + root.params.outTrue + " outFalse: " + root.params.outFalse)
    logger.info("outDesTrue: " + root.params.outDesTrue + " outDesFalse: " + root.params.outDesFalse)

    val outTrue1 = (root.params.inTrue1 + root.params.controlTrue)/2
    val outTrue2 = (root.params.inTrue2 + root.params.controlFalse)/2
    assert(outTrue1 == outTrue2)

    val outFalse1 = (root.params.inFalse1 + root.params.controlTrue)/2
    val outFalse2 = (root.params.inFalse2 + root.params.controlFalse)/2
    assert(outFalse1 == outFalse2)

    val outTrueDes1 = (root.params.inTrue2 + root.params.controlTrueDes)/2
    val outTrueDes2 = (root.params.inTrue1 + root.params.controlFalseDes)/2
    assert(outTrueDes1 == outTrueDes2)

    val outFalseDes1 = (root.params.inFalse1 + root.params.controlFalseDes)/2
    val outFalseDes2 = (root.params.inFalse2 + root.params.controlTrueDes)/2
    assert(outFalseDes1 == outFalseDes2)
  }

}
