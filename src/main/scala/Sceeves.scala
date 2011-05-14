package cap.scalasmt

/** 
 * Constraint environment for Sceeves.
 * Note change to terminology: pick = defer, assume = assert (since overriding assert is a bad idea given the implicits.
 * Variable bindings are disassociated from AST and kept here. Note that this prevents them from being garbage collected.
 */
object Inconsistent extends RuntimeException("cannot build a model")
trait Sceeves {
  private var CONSTRAINTS: List[Formula] = Nil
  private var DEFAULTS: Map[IntVar, Int] = Map()
  private var ENV: Environment = EmptyEnv

  private def solve(env: Environment) = 
    try {
      Some(SMT.solve(CONSTRAINTS, env))
    } catch {
      case SMT.UnsatException => None
    }

  private def DEFAULT_ENV = DEFAULTS.keys.foldLeft(ENV) {
    (env, i) => 
      if (env.has(i)) env else env + (i -> DEFAULTS(i))      
    }
  
  private def solve {
    if (CONSTRAINTS.size > 0) {
      ENV = solve(DEFAULT_ENV) match {
        case None => solve(ENV) match {
          case None => throw Inconsistent;
          case Some(env) => env;
        }
        case Some(env) => env;
      }
      
      for (f <- CONSTRAINTS) 
        assert(f.eval(ENV))
      CONSTRAINTS = Nil;
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
    
  def concretize(e: IntExpr): Int = {solve; e.eval(ENV)}
  def concretize(e: IntExpr, context: Var[Int], v: Int): Int = 
    new Sceeves {
      CONSTRAINTS = this.CONSTRAINTS;
      DEFAULTS = this.DEFAULTS;
      ENV = this.ENV + (context -> v);
    }.concretize(e);      
  def assume(f: Formula) = CONSTRAINTS = f :: CONSTRAINTS
  def assign[T](i: Var[T], v: T) {
    assert (! ENV.has(i))
    ENV = ENV + (i -> v)
  }
}
