package cap.scalasmt

import cap.scalasmt.{Environment => Env}

/** 
 * Constraint environment for Sceeves.
 * Note change to terminology: pick = defer, assume = assert (since overriding assert is a bad idea given the implicits.)
 * There is no garbage collection.
 */

trait Sceeves {
  case class Assign[T](v: Var[T], e: Expr[T]) {
    def eval = (v === e)
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
    if (! ENV.hasAll(e.vars)) {  
      ENV = solve(CONSTRAINTS);
      CONSTRAINTS = Nil;
      DEFAULTS = DEFAULTS.filter(d => ! ENV.has(d.v));
    }
    e.eval(ENV)
  }
    
  def concretize[T](f: Formula, e: Expr[T]): T = {
    val env = solve(f :: CONSTRAINTS);
    val v = e.eval(env);
    CONSTRAINTS = (f ==> (e === (e match {
        case _: IntExpr => IntVal(v)
        case _: Formula => BoolVal(v)
        case _: ObjectExpr => Object(v)
        case _ => throw new RuntimeException("not implemented")
      }))) :: CONSTRAINTS;
    v
  }

  def concretize[T](v: ObjectVar, o: ObjectExpr, e: Expr[T]): T = 
    concretize(v === o, e);
  def concretize[T](v: IntVar, i: IntExpr, e: Expr[T]): T = 
    concretize(v === i, e);
  def concretize[T](v: BoolVar, f: Formula, e: Expr[T]): T = 
    concretize(v <==> f, e);
}


