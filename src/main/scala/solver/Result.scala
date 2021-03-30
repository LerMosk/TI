package solver

case class Result(x: Array[Double], y: Array[Double]) {
  override def toString: String =
    s"x = ${x.map(_.formatted("%3.3f")).mkString(", ")}\ny = ${y.map(_.formatted("%3.3f")).mkString(", ")}"
}


