package cap.scalasmt

trait Sceeves {
  private var CONSTRAINTS: List[Formula] = Nil
  private var VARS: List[Var] = Nil

  private def mkVar = {    
    val v = Var.make;
    VARS = v :: VARS;
    v
  }

  private def mkConstraint(f: Formula) {
    CONSTRAINTS = f :: CONSTRAINTS
  }

  def defer(spec: Var => Formula): Var = {
    val x = mkVar;
    mkConstraint(spec(x));
    x
  }
    
  def defer(spec: (Var, Var) => Formula): (Var, Var) = {
    val x = mkVar;
    val y = mkVar;
    mkConstraint(spec(x, y));
    (x, y);
  }

  private def solve (v: Traversable[Var]) {
    val vs = v.filter(! _.assigned);
    for (v <- vs) assert (VARS contains v);
    // relevant constraints
    val vcs = CONSTRAINTS.filter(c => vs.exists(v => c.vars contains v)).reverse; 
    val f = vcs.foldLeft(TrueF: Formula)((f, vs) => f && vs);
    // solve
    SMT.solve(f);
    // assign default value
    for (v <- vs) SMT.assignDefault(v)
    // clean environment
    for (f <- CONSTRAINTS) assert (f.eval);
    CONSTRAINTS = Nil;
    VARS = Nil;
  }

  def concretize(e: Expr) = {
    solve(VARS);
    e.eval;
  }

  def assert(f: Formula) {
    mkConstraint(f);
  }
}
