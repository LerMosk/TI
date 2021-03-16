package solver

object LR2 extends App {
  val gameSolver = new ContinuousGame(-6, 32.0/5, 16, -16.0/5, -64.0/5)
  println("Analytic solve:")
  println(gameSolver.analytic)
  println("\nNumerical solve:")
  for(n <- 1 to 10) {
    println(s"\nN = $n")
    println(gameSolver.numerical(n))
  }
}
