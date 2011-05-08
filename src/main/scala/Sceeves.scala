package cap.scalasmt

/** 
 * Constraint environment for Sceeves.
 * Note change to terminology: pick = defer, assume = assert (since overriding assert is a bad idea given the implicits.
 */
object Inconsistent extends RuntimeException("cannot build a model")
trait Sceeves {
  private var CONSTRAINTS: List[Formula] = Nil
  private var DEFAULTS: Map[IntVar, Int] = Map();

  private def solve(fs: List[Formula]) = 
    try {
      SMT.solve(fs);
      true;
    } catch {
      case SMT.UnsatException => false;
    }
  
  private def solve(e: IntExpr) {
    if (CONSTRAINTS.size > 0) {
      // try with defaults
      val defaults = solve (CONSTRAINTS ++ 
        (for (v: IntVar <- e.vars.collect{case v: IntVar => v}; 
             if (! v.assigned);
             if (DEFAULTS.contains(v)))
             yield v === DEFAULTS(v))
       )
      
      if (! defaults) {
        if (! solve(CONSTRAINTS))
          throw Inconsistent;
      }
            
      for (c <- CONSTRAINTS;
           v <- c.vars;
           if ! v.assigned) v match {
        case v: IntVar => v.value = IntVar.DEFAULT;
      }

      // clean environment
      for (f <- CONSTRAINTS) 
        assert(f.eval);
      CONSTRAINTS = Nil;
      DEFAULTS = DEFAULTS.filterKeys(! _.assigned);
    }
  }

  def pick(spec: IntVar => Formula): IntVar = {
    val x = IntVar.make;
    assume(spec(x));
    x
  }

  def pick(default: Int, spec: IntVar => Formula): IntVar = {
    val x = pick(spec);
    DEFAULTS = DEFAULTS + (x -> default);
    x
  }
    
  def concretize(e: IntExpr) = {solve(e); e.eval}
  def assume(f: Formula) = CONSTRAINTS = f :: CONSTRAINTS;
}
