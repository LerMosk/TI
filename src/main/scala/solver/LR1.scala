package solver

import java.io.FileInputStream
import scala.io.Source

object LR1 extends App {
  val inFile = args(0)
  val exp = args(1).toDouble
  val matrix = Source.fromInputStream(new FileInputStream(inFile))
    .getLines().map(_.split(';').map(_.trim.toInt)).toArray
  println(MatrixGameSolver.analytic(matrix))
  println(MatrixGameSolver.braunRobinson(matrix, exp))
}
