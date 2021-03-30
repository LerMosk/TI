package solver

import breeze.linalg.{DenseMatrix, DenseVector, argmax, argmin, inv, max, min}
import scalax.chart.api._

import scala.annotation.tailrec
import scala.reflect.ClassTag

object MatrixGameSolver {
  private def toDenseMatrix[T: ClassTag](m: Array[Array[T]]): DenseMatrix[T] =
    new DenseMatrix(m.length, m(0).length, m.transpose.flatten)

  val cleanStrategies: Array[Array[Double]] => Option[Result] = matrix => {
    val m = toDenseMatrix(matrix)
    val maxi = new DenseVector(Array.range(0, m.cols).map(i => max(m(::, i))))
    val mini = new DenseVector(Array.range(0, m.cols).map(i => min(m(i, ::))))
    if(max(mini) == min(maxi)) {
      println("Saddle point:")
      Some(Result(
        Array.fill(m.cols)(0.0).updated(argmax(mini), 1.0),
        Array.fill(m.cols)(0.0).updated(argmin(maxi), 1.0)
      ))
    } else {
      println("There is no saddle point")
      None
    }
  }

  val analytic: Array[Array[Int]] => Result = matrix => {
    val m = toDenseMatrix(matrix)
    println(s"Analytic game solv:\n$m")
    val mInv = inv(m)
    println(s"C-1:\n$mInv")
    val u = DenseVector.ones[Double](m.rows)
    val v = 1 / (u.t * mInv * u)
    println(s"V = $v")
    val x = u.t * mInv * v
    val y = mInv * u * v
    Result(x.inner.data, y.data)
  }

  def braunRobinson(m: Array[Array[Double]], eps: Double): Result = {
    val matrix = toDenseMatrix(m)
    println(s"Solution by method Braun-Robinson")
    val aChoicesCount = Array.fill(matrix.rows)(0)
    val bChoicesCount = Array.fill(matrix.cols)(0)

    @tailrec
    def braunRobinsonStep(br: BrStep[Double]): Result = {
      val BrStep(step, xRow, yRow, vMaxCol, vMinCol, expSeq) = br
      val aChoice = argmax(xRow)
      val bChoice = yRow.valuesIterator.indexOf(min(yRow))
      aChoicesCount(aChoice) += 1
      bChoicesCount(bChoice) += 1
      val xRowNew = matrix(::, bChoice) + xRow
      val yRowNew = matrix(aChoice, ::).t + yRow

      val vMax = max(xRowNew) / step
      val vMin = min(yRowNew) / step
      val vMaxColNew = vMaxCol + vMax
      val vMinColNew = vMinCol + vMin
      val epsCalc = vMaxColNew.min - vMinColNew.max
      epsCalc match {
        case e if e <= eps =>
          Result(aChoicesCount.map(_.toDouble / step), bChoicesCount.map(_.toDouble / step))
        case _ =>
          braunRobinsonStep(BrStep(step + 1, xRowNew, yRowNew, vMaxColNew, vMinColNew, expSeq :+ epsCalc))
      }
    }

    braunRobinsonStep(BrStep(
      step = 1,
      xRow = DenseVector.zeros[Double](matrix.rows),
      yRow = DenseVector.zeros[Double](matrix.cols),
      vMaxCol = Set.empty,
      vMinCol = Set.empty,
      expSeq = Seq.empty[Double]
    ))
  }

  val braunRobinson: (Array[Array[Int]], Double) => Result = (m, eps) => {
    val matrix = toDenseMatrix(m)
    println(s"Braun-Robinson game solv\n$matrix")
    println(s"   k  A  B${" " * 5}x0${" " * 5}x1${" " * 5}x2${" " * 5}y0${" " * 5}y1${" " * 5}y2   Vmax   Vmin  eps")
    val aChoicesCount = Array.fill(matrix.rows)(0)
    val bChoicesCount = Array.fill(matrix.cols)(0)

    @tailrec
    def braunRobinsonStep(br: BrStep[Int]): (Result, Seq[Double]) = {
      val BrStep(step, xRow, yRow, vMaxCol, vMinCol, expSeq) = br
      val aChoice = argmax(xRow)
      val bChoice = yRow.valuesIterator.indexOf(min(yRow))
      aChoicesCount(aChoice) += 1
      bChoicesCount(bChoice) += 1
      val xRowNew = matrix(::, bChoice) + xRow
      val yRowNew = matrix(aChoice, ::).t + yRow

      val vMax = max(xRowNew).toDouble / step
      val vMin = min(yRowNew).toDouble / step
      val vMaxColNew = vMaxCol + vMax
      val vMinColNew = vMinCol + vMin
      val epsCalc = vMaxColNew.min - vMinColNew.max
      println(f"$step%4s x$aChoice y$bChoice${xRowNew.data.map(_.formatted("%7d")).mkString}${yRowNew.data.map(_.formatted("%7d")).mkString} $vMax%6.3f $vMin%6.3f $epsCalc%6.3f")
      epsCalc match {
        case e if e <= eps =>
          println(s"Result eps = $e")
          (Result(aChoicesCount.map(_.toDouble / step), bChoicesCount.map(_.toDouble / step)), expSeq :+ epsCalc)
        case _ =>
          braunRobinsonStep(BrStep(step + 1, xRowNew, yRowNew, vMaxColNew, vMinColNew, expSeq :+ epsCalc))
      }
    }

    val (res, epsGraph) = braunRobinsonStep(BrStep(
      step = 1,
      xRow = DenseVector.zeros[Int](matrix.rows),
      yRow = DenseVector.zeros[Int](matrix.cols),
      vMaxCol = Set.empty,
      vMinCol = Set.empty,
      expSeq = Seq.empty[Double]
    ))

    val chart = XYLineChart(epsGraph.zipWithIndex.map(_.swap), title = "eps", legend = false)
    chart.show()
    res
  }

  case class BrStep[T](step: Int,
                    xRow: DenseVector[T],
                    yRow: DenseVector[T],
                    vMaxCol: Set[Double],
                    vMinCol: Set[Double],
                    expSeq: Seq[Double])

}
