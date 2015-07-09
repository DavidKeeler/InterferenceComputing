package frequencycomputing

import scala.collection._
import frequencycomputing.OutputSymbol
import frequencycomputing.OutputListener
import frequencycomputing.LeastSquaresListener

object MusicBox {
    type Note = (Double, Double)=>Double      // (x, time) => wave height
    type OutputNote = Double=>Double        // time => wave height
  
    val listenerPosition = 1.2
    val trueFrequency = 50.0
    val falseFrequency = 55.0
    
  def TRUE: Note = (x: Double, t: Double) => Math.sin(trueFrequency * x + t) - Math.sin(trueFrequency * x - t)
  def FALSE: Note = (x: Double, t: Double) => Math.sin(falseFrequency * x + t) - Math.sin(falseFrequency * x - t)
  
  sealed trait Operation {
    def name: String
    def wave: Note
  }
  
  case class Write(value: Boolean) extends Operation {
    def name = "WRITE"
    def wave = 
          if (value) TRUE
          else FALSE
  }
  case class Store(frequency: Double, phase: Double) extends Operation {
    def name = "STORE"
    def wave = (x: Double, t: Double) => 
      Math.sin(frequency * x + t + phase) - Math.sin(frequency * x - t + phase)
  }
  case class Retrieve(frequency: Double, phase: Double) extends Operation {
    def name = "RETRIEVE"
    def wave = (x: Double, t: Double) => 
      // TODO: what's the relationship between the phase and the frequency
      // to be compatible with STORE
      Math.sin(frequency * x + t + phase) - Math.sin(frequency * x - t + phase)
  }
}

class MusicBox(private val input: Iterator[MusicBox.Operation]) {
  import MusicBox._
  
  /*
   * The wave function holding the machine state. Each operation will
   * update this wave and the output antenna will listen to it.
   */
  private var wave: Note = (x: Double, t: Double) => 0.0
  private val outputListener: OutputListener = new LeastSquaresListener(trueFrequency, falseFrequency)

  /**
   * Perform the next operation from the input.
   * @returns the operation that was performed.
   */
  def nextOperation: Operation = {
      val operation = input.next
      wave = (x: Double, t: Double) => operation.wave(x, t) + wave(x, t)
      
      operation
  }
  
  def hasNextOperation: Boolean = input.hasNext
  
  /**
   * First param is distance.  Second is time.
   * 
   * @return The current wave function for the machine.
   */
  def waveFunction: (Double, Double)=>Double = wave
  def outputWave: Double=>Double = (t: Double) => wave(listenerPosition, t)
  
  def ouputIterator: Iterator[OutputSymbol] = new Iterator[OutputSymbol] {
    private var nextSymbol: Option[OutputSymbol] = None	
    
    def hasNext = nextSymbol.isDefined
    def next = {
      val returnMe = nextSymbol.get
      
      // Run the machine until there's another output symbol
      do {
        nextOperation
        nextSymbol = outputListener.listen(outputWave)
      } while (nextSymbol.isEmpty)
      
      returnMe
    }
  }
}


