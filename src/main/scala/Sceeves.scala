package cap.scalasmt

trait Sceeves {
  private var CONSTRAINTS: List[Formula] = Nil
  private var VARS: List[Var] = Nil

  private def mkVar = {    
    val v = IntVar.make;
    VARS = v :: VARS;
    v
  }

  private def mkConstraint(f: Formula) {
    CONSTRAINTS = f :: CONSTRAINTS
  }

  private def solve {
    val vs = VARS.filter(! _.assigned);
    // relevant constraints
    if (CONSTRAINTS.size > 0) {
      val f = CONSTRAINTS.reverse.reduceLeft((f, vs) => f && vs);
      // solve
      SMT.solve(f);
      // assign default value
      for (v @ IntVar(_) <- vs) SMT.assignDefault(v)
      // clean environment
      for (f <- CONSTRAINTS) assert (f.eval);
      for (v <- VARS) assert (v.assigned);
      CONSTRAINTS = Nil;
      VARS = Nil;
    }
  }

  def pick(spec: IntVar => Formula): IntVar = {
    val x = mkVar;
    mkConstraint(spec(x));
    x
  }
    
  def concretize(e: Expr) = {
    solve;
    e match {
      case e: IntExpr => e.eval;
    }
  }

  /**
   * Avoid using assert keyword since implicit conversion from boolean does not play
   * well.
   */
  def assume(f: Formula) {
    mkConstraint(f);
  }
}
