package solver

import breeze.linalg.DenseMatrix

object Utils {

  def printMatrix(arr: Array[Array[Double]]): Unit =
    println(arr.map(_.map(_.formatted("%3.3f")).mkString(" ")).mkString("\n"))

  def printMatrix(matrix: DenseMatrix[Double]): Unit =
    printMatrix(matrix.toArray.grouped(matrix.cols).toArray)

}
