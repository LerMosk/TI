package solver

import breeze.linalg.{ Axis, DenseMatrix, DenseVector, argmax, argmin, inv, max, min }
import breeze.numerics.abs
import scalax.chart.api._
import solver.Utils.printMatrix

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

  val analytic: Array[Array[Double]] => Result = matrix => {
    val m = toDenseMatrix(matrix)
    analytic(m)
  }

  def analytic(m: DenseMatrix[Double]): Result = {
    println(s"Analytic game solv:\n$m")
    val mInv = inv(m)
    println(s"C-1:\n$mInv")
    val u1 = DenseVector.ones[Double](m.rows)
    val u2 = DenseVector.ones[Double](m.cols)
    val v = 1 / (u1.t * mInv * u2)
    println(s"V = $v")
    val x = u1.t * mInv * v
    val y = mInv * u2 * v
    Result(x.inner.data, y.data)
  }

  def monotone(matrix: Array[Array[Double]]): Seq[Double] = {
    val m = toDenseMatrix(matrix)
    val eps = 0.001

    @tailrec
    def step(s: Int, xPrev: DenseVector[Double], cPrev: DenseVector[Double], J: Seq[Int]): Seq[Double] = {
      val stepMatrix = DenseVector.horzcat(J.map(m(::, _)): _*)
      println(s"\nNew step $s")
      println(s"New step matrix:")
      printMatrix(stepMatrix)
      val x_  = if(stepMatrix.cols == 1) {
        val dv = DenseVector.zeros[Double](stepMatrix.rows)
          dv.update(argmax(stepMatrix(::, 0)), 1)
        dv
      } else DenseVector(braunRobinson(stepMatrix, eps).x)
      val c_ = x_.toScalaVector().zipWithIndex.map{ case (xi, ind) => xi * m(ind, ::).t}.reduce(_ + _)
      println(s"x_$s = ${x_}\nc_$s = $c_")
      val m2 = DenseVector.horzcat(c_, cPrev).t
      val Array(a, a_) = braunRobinson(m2, eps).x
      println()
      printMatrix(m2)
      println(s"a$s = $a\n(1-a$s) = $a_")
      if (a_  < 0.05) xPrev.toScalaVector()
      else {
        val xNew = xPrev * a_ + x_ * a
        val cNew = cPrev * a_ + c_ * a
        val minC = min(cNew)
        val JNew = cNew.iterator.collect{case (ind, v) if abs(v - minC) < 0.01 => ind}.toList
        println(s"\nStep $s result:\nx$s = $xNew\nc$s = $cNew\nJ$s = $JNew")
        step(s + 1, xNew, cNew, JNew)
      }
    }

    step(1, DenseVector(1, 0, 0), m(0, ::).t, Seq(argmin(m(0, ::))))
  }

  def removeDominicRows(m: DenseMatrix[Double]):(Seq[Int], DenseMatrix[Double]) = {
    var remove = Seq.empty[Int]
    for (i <- 0 until m.rows; j <- 0 until m.rows) {
      if (i != j) {
        if (m(i, ::).inner.toScalaVector().zip(m(j, ::).inner.toScalaVector()).forall { case (v1, v2) => v1 > v2 }) {
          remove = j +: remove
        }
      }
    }
    (remove.distinct, m.delete(remove.distinct, Axis._0))
  }

  def removeDominicCols(m: DenseMatrix[Double]):(Seq[Int], DenseMatrix[Double]) = {
    var remove = Seq.empty[Int]
    for (i <- 0 until m.cols; j <- 0 until m.cols) {
      if (i != j) {
        if (m(::, i).toScalaVector().zip(m(::, j).toScalaVector()).forall { case (v1, v2) => v1 > v2 }) {
          remove = i +: remove
        }
      }
    }
    (remove.distinct, m.delete(remove.distinct, Axis._1))
  }

  def braunRobinson(matrix: Array[Array[Double]], eps: Double): Result =
    braunRobinson(toDenseMatrix(matrix), eps)

  def braunRobinson(matrix: DenseMatrix[Double], eps: Double): Result = {
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
