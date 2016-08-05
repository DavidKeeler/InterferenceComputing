package frequencycomputing

import scala.collection._

object MusicBox {
    type OutputNote = Double=>Double        // time => wave height

    val listenerPosition = 1.2
    val trueFrequency = 5.0
    val falseFrequency = 5.5
}

class MusicBox(private val input: Iterator[Operation]) {
  import MusicBox._

  /*
   * The wave function holding the machine state. Each operation will
   * update this wave and the output antenna will listen to it.
   */
  private var wave: Double=>Double = (t: Double) => 0.0
  private val outputListener: OutputListener = new LeastSquaresListener(trueFrequency, falseFrequency)

  /**
   * Perform the next operation from the input.
   * @returns the operation that was performed.
   */
  def nextOperation: Operation = {
      val operation = input.next
      wave = (t: Double) => operation(t) + wave(t)

      operation
  }

  def hasNextOperation: Boolean = input.hasNext

  /**
   * First param is distance.  Second is time.
   *
   * @return The current wave function for the machine.
   */
  def waveFunction: Double=>Double = wave
  def outputWave: Double=>Double = (t: Double) => wave(t)

  def ouputIterator: Iterator[OutputSymbol] = new Iterator[OutputSymbol] {
    private var nextSymbol: Option[OutputSymbol] = None

    def hasNext = nextSymbol.isDefined
    def next = {
      val returnMe = nextSymbol.get

      // Run the machine until there's another output symbol
      do {
        nextOperation
        nextSymbol = outputListener.listen(outputWave, 0.0)
      } while (nextSymbol.isEmpty)

      returnMe
    }
  }
}


