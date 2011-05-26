package cap.scalasmt

/** 
 * Constraint environment for Sceeves.
 * Note change to terminology: pick = defer, assume = assert (since overriding assert is a bad idea given the implicits.
 * Variable bindings are disassociated from AST and kept here. Note that this prevents them from being garbage collected.
 */
object Inconsistent extends RuntimeException("cannot build a model")
trait Sceeves {
  private var CONSTRAINTS: List[Formula] = Nil
  private var DEFAULTS: Map[IntVar, IntExpr] = Map()
  private var ENV: Environment = EmptyEnv

  private def solve(fs: List[Formula], env: Environment) = 
    try {
      Some(SMT.solve(fs, env))
    } catch {
      case SMT.UnsatException => None
    }

  private def WITH_DEFAULTS = 
    (CONSTRAINTS ++ DEFAULTS.filter(t => ! ENV.has(t._1)).map{case (iv, ie) => iv === ie}, ENV)
  
  private def resolve {
    if (CONSTRAINTS.size > 0) {
      ENV = (solve _).tupled(WITH_DEFAULTS) match {
        case None => solve(CONSTRAINTS, ENV) match {
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

  private def duplicate = {
    val that = new Sceeves {};
    that.CONSTRAINTS = this.CONSTRAINTS;
    that.DEFAULTS = this.DEFAULTS;
    that.ENV = this.ENV;
    that;
  }

  def pick: IntVar = IntVar.make;
  def pick(spec: IntVar => Formula): IntVar = {val x = pick; assume(spec(x)); x}
  def pick(default: IntExpr, spec: IntVar => Formula): IntVar = {
    val x = pick(spec);
    DEFAULTS = DEFAULTS + (x -> default);
    x
  }  
  def assume(f: Formula) = CONSTRAINTS = f :: CONSTRAINTS
  def concretize[T](e: Expr[T]): T = {resolve; e.eval(ENV)}
  def concretize[T,U](i: IntVar, v: IntExpr, e: Expr[T]): T = {
    val that = duplicate;
    that.assume(i === v);
    that.concretize(e);
  }
  def assign[T](i: IntVar, v: BigInt) {
    assert (! ENV.has(i))
    ENV = ENV + (i -> v)
  }
}
