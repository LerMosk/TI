package solver

case class ResultContinuous(x: Double, y: Double, H: Double) {
  override def toString: String = f"x = $x%6.3f, y = $y%6.3f, H = $H%6.3f"
}
