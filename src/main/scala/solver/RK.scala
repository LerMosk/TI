package solver

import java.io.FileInputStream
import scala.io.Source

object RK extends App {
  val inFile = args(0)
  val matrix = Source.fromInputStream(new FileInputStream(inFile))
    .getLines().map(_.split(';').map(_.trim.toDouble)).toArray
 // println(MatrixGameSolver.analytic(matrix))
  println(MatrixGameSolver.monotone(matrix))
}
