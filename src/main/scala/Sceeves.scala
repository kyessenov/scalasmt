package cap.scalasmt

/** 
 * Constraint environment for Sceeves.
 * Note change to terminology: pick = defer, assume = assert (since overriding assert is a bad idea given the implicits.
 * Variable bindings are disassociated from AST and kept here. Note that this prevents them from being garbage collected.
 */
object Inconsistent extends RuntimeException("cannot build a model")
trait Sceeves {
  private var CONSTRAINTS: List[Formula] = Nil
  private var DEFAULTS: Environment = EmptyEnv
  private var ENV: Environment = EmptyEnv

  private def solve(env: Environment) = 
    try {
      Some(SMT.solve(CONSTRAINTS, env))
    } catch {
      case SMT.UnsatException => None
    }

  // TODO: fight Scala's type inference
  private def defaults = DEFAULTS.vars.foldLeft(ENV) {
    (env, i) => 
      if (env.has(i)) 
        env
      else i match {
        case i: Var[Int] => 
          val v = DEFAULTS(i);
          env + (i -> v)
      }
    }
  
  private def solve {
    if (CONSTRAINTS.size > 0) {
      // try with defaults
      ENV = solve(defaults) match {
        case None => solve(ENV) match {
          case None => throw Inconsistent;
          case Some(env) => env;
        }
        case Some(env) => env;
      }
      
      // clean environment
      for (f <- CONSTRAINTS) 
        if (! f.eval(ENV))
          throw Inconsistent;
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
  def assign[T](i: Var[T], v: T) {ENV = ENV + (i -> v)}
}
