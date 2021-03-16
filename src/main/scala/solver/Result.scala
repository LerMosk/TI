package solver

case class Result(x: Array[Double], y: Array[Double]) {
  override def toString: String = s"x = ${x.mkString(", ")}\ny = ${y.mkString(", ")}"
}


