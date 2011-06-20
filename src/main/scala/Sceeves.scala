package cap.scalasmt

import cap.scalasmt.{Environment => Env}

/** 
 * Constraint environment for Sceeves.
 * Note change to terminology: pick = defer, assume = assert (since overriding assert is a bad idea given the implicits.)
 * Variable bindings are disassociated from AST and kept here. Note that this prevents them from being garbage collected.
 */
case class Assign[T](v: Var[T], e: Expr[T]) 

trait Sceeves {
  type Defaults = List[Assign[_]]

  private var CONSTRAINTS: List[Formula] = Nil
  private var DEFAULTS: Defaults = Nil
  private var ENV: Env = DefaultEnv
  private var VARS: List[Var[_]] = Nil

  private def assign[T](a: Assign[T]) = a.v match {
    // type erasure makes it impossible to be type safe here
    case v: IntVar => v === a.e.asInstanceOf[IntExpr]
    case v: BoolVar => v <==> a.e.asInstanceOf[Formula]
    case v: AtomVar => v === a.e.asInstanceOf[ObjectExpr]
    case v: AtomSetVar => v === a.e.asInstanceOf[RelExpr]
  }

  private def solve(fs: List[Formula], defs: Defaults, env: Env): Env =
    fs match {
      case Nil => env
      case _ => 
        val defaults = defs.map(assign(_));
        SMT.solve(fs, defaults)(env)
    }

  def pick(spec: IntVar => Formula = _ => true, default: IntExpr = null) = {
    val x = Var.makeInt; 
    VARS = x :: VARS;
    assume(spec(x)); defaultAssign(x, default);
    x
  }

  def pickBool(spec: BoolVar => Formula = _ => true, default: Formula = null) = {
    val x = Var.makeBool;
    VARS = x :: VARS;
    assume(spec(x)); defaultAssign(x, default)
    x
  }

  def pickAtom(spec: AtomVar => Formula = _ => true, default: ObjectExpr = null) = {
    val x = Var.makeAtom;
    VARS = x :: VARS;
    assume(spec(x)); defaultAssign(x, default)
    x
  }
  
  def assume(f: Formula) {
    CONSTRAINTS = f :: CONSTRAINTS
  }

  def defaultAssign[T](v: Var[T], e: Expr[T]) {
    if (e != null) DEFAULTS = Assign(v, e) :: DEFAULTS
  }

  def concretize[T](e: Expr[T]): T = {
    ENV = solve(CONSTRAINTS, DEFAULTS, ENV)
    for (f <- CONSTRAINTS) assert(f.eval(ENV))
    CONSTRAINTS = Nil
    DEFAULTS = DEFAULTS.filter(a => ! ENV.has(a.v))
    e.eval(ENV)
  }
    
  def concretize[T](f: Formula, e: Expr[T]): T = 
    e.eval(solve(f :: CONSTRAINTS, DEFAULTS, ENV))

  def concretize[T](v: AtomVar, o: ObjectExpr, e: Expr[T]): T = 
    concretize(v === o, e);
  def concretize[T](v: IntVar, i: IntExpr, e: Expr[T]): T = 
    concretize(v === i, e);
  def concretize[T](v: BoolVar, f: Formula, e: Expr[T]): T = 
    concretize(v <==> f, e);
}


