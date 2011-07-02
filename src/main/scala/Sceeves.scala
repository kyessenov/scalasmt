package cap.scalasmt

import cap.scalasmt.{Environment => Env}

/** 
 * Constraint environment for Sceeves.
 */

trait Sceeves {
  case class Assign[T](v: Var[T], e: Expr[T]) {
    def eval = (v === e) match {case f: Formula => f}
  }
  type Defaults = List[Assign[_]]
  type Constraints = List[Formula]

  private var CONSTRAINTS: Constraints = Nil
  private var DEFAULTS: Defaults = Nil
  private var ENV: Env = DefaultEnv

  private def solve(fs: List[Formula]) =  
    SMT.solve(fs, DEFAULTS.map(_.eval))(ENV)
  
  def pick(spec: IntVar => Formula = _ => true, default: IntExpr = null) = {
    val x = Var.makeInt; 
    assume(spec(x)); 
    byDefault(x, default);
    x
  }

  def pickBool(spec: BoolVar => Formula = _ => true, default: Formula = null) = {
    val x = Var.makeBool;
    assume(spec(x)); 
    byDefault(x, default)
    x
  }

  def pickObject(spec: ObjectVar => Formula = _ => true, default: ObjectExpr = null) = {
    val x = Var.makeObject;
    assume(spec(x)); 
    byDefault(x, default)
    x
  }
  
  def assume(f: Formula) {
    CONSTRAINTS = f :: CONSTRAINTS
  }

  private def byDefault[T](v: Var[T], e: Expr[T]) = Option(e) match {
    case Some(e) => DEFAULTS = Assign(v, e) :: DEFAULTS
    case _ => 
  }

  def concretize[T](e: Expr[T]): T = {
    if (CONSTRAINTS.size > 0) {  
      ENV = solve(CONSTRAINTS);
      CONSTRAINTS = Nil;
      DEFAULTS = DEFAULTS.filter(d => ! ENV.has(d.v));
    }
    e.eval(ENV)
  }
    
  def concretize[T](f: Formula, e: Expr[T]): T = {
    val v = e.eval(solve(f :: CONSTRAINTS));
    // TODO: record the entire state (like absence of objects) into context to make this work
    // CONSTRAINTS = (f ==> (e === e.constant(v))) :: CONSTRAINTS;
    v
  }
}


