package cap.scalasmt

/**
 * Angelic choice statement for arithmetic.
 */
object ChoiceStmt {
  import SMT._

  def choose(spec: IntVar => Formula): Int = {
    val x = IntVar.make
    val out = solve(spec(x), EmptyEnv)
    out(x);
  }

  def choose(spec: (IntVar, IntVar) => Formula): (Int, Int) = {
    val x = IntVar.make
    val y = IntVar.make
    val out = solve(spec(x, y), EmptyEnv)
    (out(x), out(y))
  }
}
