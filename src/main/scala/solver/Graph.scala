package solver

import org.jzy3d.chart.{ Chart, ChartLauncher }

import org.jzy3d.colors.Color

import org.jzy3d.maths.Coord3d
import org.jzy3d.plot3d.primitives.Scatter
import org.jzy3d.plot3d.rendering.canvas.Quality

import java.awt.Rectangle

object ScatterDemoAWT {

  def init(gr: List[(Double, Double, Double)]): Unit = {
    val size   = gr.size
    val points = new Array[Coord3d](size)
    val colors = new Array[Color](size)
    for (i <- 0 until size) {
      val (x, y, z) = gr(i)
      points(i) = new Coord3d(x, y, z)
      colors(i) = Color.RED
    }
    val scatter = new Scatter(points, colors)
    val chart   = new Chart(Quality.Advanced)
    chart.getScene.getGraph.add(scatter)
    //ChartLauncher.openStaticChart(chart, new Rectangle(0, 0, 600, 600), "name")
    ChartLauncher.screenshot(chart, "data/screenshots/" + "dd" + ".png")
  }
}
