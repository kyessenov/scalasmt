package cap.scalasmt

/**
 * Angelic choice statement for arithmetic.
 */
object ChoiceStmt {
  import SMT._

  def choose(spec: IntVar => Formula): Int = {
    val x = IntVar.make;
    solve(spec(x))
    assignDefault(x)
  }

  def choose(spec: (IntVar, IntVar) => Formula): (Int, Int) = {
    val x = IntVar.make;
    val y = IntVar.make;
    solve(spec(x, y))
    (assignDefault(x), assignDefault(y))
  }
}
