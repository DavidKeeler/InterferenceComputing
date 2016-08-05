  package frequencycomputing

  import javax.swing.JFrame
  import org.jfree.chart.JFreeChart
  import org.jfree.data.xy.XYDataset
  import org.jfree.data.xy.XYDataItem
  import org.jfree.data.xy.XYSeries
  import org.jfree.data.xy.XYSeriesCollection
  import org.jfree.chart.ChartFactory
  import org.jfree.chart.plot.XYPlot
  import org.jfree.chart.ChartPanel
  import java.awt.Dimension
  import java.awt.event.ActionListener
  import javax.swing.Timer
  import java.awt.event.ActionEvent
  import org.jfree.chart.axis.NumberAxis
  import javax.swing.JPanel

  object PlotFrame {
    private val dataPoints = 2000

    def main(args: Array[String]) {
      import MusicBox._

      val instructions = Seq[Operation](Input(1.0, 0.0))

      val machine = new MusicBox(instructions.iterator)

      val plotFrame = createUI(machine)
      plotFrame.pack
      plotFrame.setVisible(true)

      while (machine.hasNextOperation) {
        machine.nextOperation
      }
    }

    private def createUI(machine: MusicBox): JFrame = {
      val plotFrame = new JFrame()

      val panel = new JPanel()
      panel.setPreferredSize(new Dimension(800, 700))

      // Standing wave
      val chart = createChart(0, Math.PI)
      val chartPanel = new ChartPanel(chart)
      chartPanel.setPreferredSize(new Dimension(800, 300))
      panel.add(chartPanel)

      // Output chart
      val outputChart = createChart(0, 2 * Math.PI)
      val outputChartPanel = new ChartPanel(outputChart)
      outputChartPanel.setPreferredSize(new Dimension(800, 300))
      panel.add(outputChartPanel)

      // Add animation
      val outputPlot = outputChart.getPlot match {
        case x: XYPlot => x
      }
      updateOutputChart(outputPlot, machine, 0, 2 * Math.PI)

      val plot = chart.getPlot match {
        case x: XYPlot => x
      }
      updateChart(plot, machine, 0, Math.PI)

      plotFrame.add(panel)
      plotFrame
    }

    private def updateChart(plot: XYPlot, machine: MusicBox, minRange: Double, maxRange: Double) {
      val interval = 5								// milleseconds
      val startTime: Double = System.currentTimeMillis

      val updateChart = new ActionListener() {
      	  def actionPerformed(evt: ActionEvent) {
      	    val timeChange: Double = (System.currentTimeMillis - startTime)
      	    val t: Double = timeChange/300
      	    val updatedDataSet = createData(machine, t, minRange, maxRange)
      	    plot.setDataset(updatedDataSet)
        }
      }

      new Timer(interval, updateChart).start
    }

    private def updateOutputChart(plot: XYPlot, machine: MusicBox, minRange: Double, maxRange: Double) {
      val interval = 5								// milleseconds
      val startTime: Double = System.currentTimeMillis

      val updateChart = new ActionListener() {
      	  def actionPerformed(evt: ActionEvent) {
      	    val timeChange: Double = (System.currentTimeMillis - startTime)
      	    val t: Double = timeChange/300

      	    val updatedDataSet = createOutputData(machine, t, minRange: Double, maxRange: Double)
      	    plot.setDataset(updatedDataSet)
        }
      }

      new Timer(interval, updateChart).start();
    }

    private def createChart(minRange: Double, maxRange: Double): JFreeChart = {
      val xLabel = ""
      val yLabel = ""

      val emptyDataSet = new XYSeriesCollection()
      val chart = ChartFactory.createXYLineChart("", xLabel, yLabel, emptyDataSet)
      val plot = chart.getPlot.asInstanceOf[XYPlot]
      val xAxis = plot.getDomainAxis.asInstanceOf[NumberAxis]
      val yAxis = plot.getRangeAxis.asInstanceOf[NumberAxis]

      xAxis.setRange(minRange, maxRange)
      yAxis.setRange(-2, 2)

      chart
    }

    private def createData(machine: MusicBox, time: Double, minRange: Double, maxRange: Double): XYDataset = {
      val stepSize = (maxRange - minRange)/dataPoints
      val xValues =
      	for (index <- 0 to dataPoints) yield {
      	  minRange + index * stepSize
      	}
      val data = xValues.map {
        x =>
          val y = machine.waveFunction(time)
          new XYDataItem(x, y)
      }
      val series = new XYSeries("Testing")
      for (point <- data)
      		series.add(point)

      new XYSeriesCollection(series)
    }

    private def createOutputData(machine: MusicBox, time: Double, minRange: Double, maxRange: Double): XYDataset = {
      val stepSize = (maxRange - minRange)/dataPoints

      val xValues =
  	    	for (index <- 0 to dataPoints) yield {
  	    	  index * stepSize
  	    	}
      val data = xValues.map {
        x =>
          val y = machine.outputWave(time + x)
          new XYDataItem(x, y)
      }

      val series = new XYSeries("Testing")
      for (point <- data)
        series.add(point)

      new XYSeriesCollection(series)
    }


  }









