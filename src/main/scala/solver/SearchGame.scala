package solver

import breeze.numerics.{ abs, cos, pow, sin, sqrt }
import spire.math.pi

object SearchGame {
  type Point = (Double, Double, Double)

  def analytic(s: Int, r: Double, R: Double): Double =
    s.toDouble / 2 * (1 - sqrt(1 - ((r * r) / (R * R))))

  def numeric(s: Int, r: Double, R: Double): Double = {
    val e = 200

    def eval(step: Int, sum: Int): Double = {
      val v = randomPoint(R)
      println(s"v = ${v.f}")
      val m = List.fill(s)(randomPoint(R))
      println(s"m = ${m.map(_.f)}")
      val res  = if (m.exists(checkHit(_, v, r))) 1 else 0
      val kNew = (sum + res).toDouble / step
      println(s"Step $step, kNew = ${kNew.f}")
      if (step > e) kNew
      else eval(step + 1, sum + res)
    }

    eval(1, 0)
  }

  private def checkHit(m: Point, v: Point, r: Double): Boolean = {
    val (x1, y1, z1) = v
    val (x2, y2, z2) = m
    val distance     = sqrt(pow(x1 - x2, 2) + pow(y1 - y2, 2) - pow(z1 - z2, 2))
    distance <= r
  }

  private def randomPoint(R: Double): Point = {
    val x = random(-R, R)
    val y = random(-sqrt(R * R - x * x), sqrt(R * R - x * x))
    val z = sqrt(R * R - x * x - y * y)
    (x, y, if (math.random < 0.5) z else -z)
  }

  private def randomPoints(h: Double, s: Int): List[Point] = {
    val r = h * h / 2
    val n = s / 3
    var X = List.empty[Double]
    var Y = List.empty[Double]
    var k = 0
    for (_ <- 0 until n) {
      val x1  = r * (1 - 2 * math.random)
      val y1  = r * (1 - 2 * math.random);
      val ro2 = x1 * x1 + y1 * y1
      if (ro2 <= r * r) {
        k = k + 1
        X = X :+ x1
        Y = Y :+ y1
      }
    }

    val indices = List.range(0, s - k).map(_ + math.random)
    var u       = indices.map(i => math.acos(2 * i * i / s - k))
    val v       = indices.map(pi * (1 + pow(5, 0.5)) * _)
    u = u.map(_ / pi / 2).map(_ * sqrt(h))

    val x = u.zip(v).map(i => i._1 * math.cos(i._2))
    val y = u.zip(v).map(i => i._1 * math.sin(i._2))
    val z = u.map(pow(_, 2))
    x.zip(y.zip(z)).map { case (xx, (yy, zz)) => (xx, yy, zz) } ++: X.zip(Y).map(i => (i._1, i._2, h))
  }

  private def random(from: Double = 0, to: Double = 1): Double =
    from + math.random * (to + (if (from < 0) abs(from) else 0))

  implicit class Format(d: Double) {
    def f: String = d.formatted("%3.3f")
  }

  implicit class FormatPoint(p: Point) {
    def f: String = f"(${p._1}%3.3f, ${p._2}%3.3f, ${p._3}%3.3f)"
  }

  def main(args: Array[String]): Unit = {
    val R = 10
    val r = 8
    val s = 3
    //println(s"Analytic ${analytic(s, r, R)}")
    //println(s"Numeric ${numeric(s, r, R)}")
    //println(s"Numeric ${numericEllipse(4, 5, 6, r, s)}")
    ScatterDemoAWT.init(randomPoints(7, 10))
  }
}
