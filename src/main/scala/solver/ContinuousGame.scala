package solver

class ContinuousGame(a: Double, b: Double, c: Double, d: Double, e: Double) {
  def h(x: Double, y: Double): Double =
    a * x * x + b * y * y + c * x * y + d * x + e * y

  lazy val analytic: ResultContinuous = {
    val y = (c * d - 2 * a * e) / (4 * a * b - c * c)
    val x = -(c * y + d) / (2 * a)
    ResultContinuous(x, y, h(x, y))
  }

  def numerical(n: Int): ResultContinuous = {
    val arr: Array[Array[Double]] = Array.fill(n + 1, n + 1)(0.0)
    for (i <- 0 to n; j <- 0 to n)
      arr(i)(j) = h(i.toDouble / n, j.toDouble / n)
    printMatrix(arr)
    val res = MatrixGameSolver.cleanStrategies(arr) getOrElse MatrixGameSolver.braunRobinson(arr, 0.1)
    val x = getRes(res.x, n)
    val y = getRes(res.y, n)
    ResultContinuous(x, y, h(x, y))
  }

  private def getRes(probability: Array[Double], n: Int): Double =
    probability.zipWithIndex.maxBy(_._1)._2.toDouble / n

  private def printMatrix(arr: Array[Array[Double]]): Unit =
    println(arr.map(_.map(_.formatted("%3.3f")).mkString(" ")).mkString("\n"))

}
