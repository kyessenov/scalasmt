package cap.scalasmt

/**
 * Angelic choice statement for arithmetic.
 */
object ChoiceStmt {
  import SMT._

  def choose(spec: Var => Formula): Int = {
    val x = Var.make;
    solve(spec(x))
    assignDefault(x)
  }

  def choose(spec: (Var, Var) => Formula): (Int, Int) = {
    val x = Var.make;
    val y = Var.make;
    solve(spec(x, y))
    (assignDefault(x), assignDefault(y))
  }
}
