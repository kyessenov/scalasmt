package cap.scalasmt

/**
 * Angelic choice statement for arithmetic.
 */
object ChoiceStmt {
  import SMT._

  def choose(spec: IntVar => Formula): Int = {
    val x = Var.makeInt
    val out = solve(spec(x))
    out(x).toInt;
  }

  def choose(spec: (IntVar, IntVar) => Formula): (Int, Int) = {
    val x = Var.makeInt
    val y = Var.makeInt
    val out = solve(spec(x, y))
    (out(x).toInt, out(y).toInt)
  }
}
