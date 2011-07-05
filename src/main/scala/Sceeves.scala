package cap.scalasmt

import cap.scalasmt.{Environment => Env}

/** 
 * Constraint environment for Sceeves.
 */

trait Sceeves {
  type Defaults = List[Formula]
  type Constraints = List[Formula]

  private var CONSTRAINTS: Constraints = Nil
  private var DEFAULTS: Defaults = Nil
  private var SCOPE: Set[Atom] = Set()
  private var ENV: Env = DefaultEnv

  private def solve(fs: List[Formula]) =  
    SMT.solve(fs, DEFAULTS, SCOPE)(ENV)
  
  def pick(spec: IntVar => Formula = _ => true): IntVar = {
    val x = Var.makeInt; 
    assume(spec(x)); 
    x
  }

  def pickBool(spec: BoolVar => Formula = _ => true): BoolVar = {
    val x = Var.makeBool; 
    assume(spec(x)); 
    x
  }

  def pickObject(spec: ObjectVar => Formula = _ => true): ObjectVar = {
    val x = Var.makeObject; 
    assume(spec(x)); 
    x
  }

  def pick(spec: IntVar => Formula, default: IntExpr): IntVar = {
    val x = pick(spec); 
    usually(x === default); 
    x
  }

  def pickBool(spec: BoolVar => Formula, default: Formula): BoolVar = {
    val x = pickBool(spec); 
    usually(x === default); 
    x
  }

  def pickObject(spec: ObjectVar => Formula, default: ObjectExpr[Atom]): ObjectVar = {
    val x = pickObject(spec); 
    usually(x === default); 
    x
  }
  
  def assume(f: Formula) {
    CONSTRAINTS = f :: CONSTRAINTS
  }

  def usually(f: Formula) {
    DEFAULTS = f :: DEFAULTS
  }

  def register(a: Atom) {
    SCOPE = SCOPE + a;
  }

  def concretize[T](e: Expr[T]): T = {
    if (CONSTRAINTS.size > 0) {  
      ENV = solve(CONSTRAINTS);
      CONSTRAINTS = Nil;
    }
    e.eval(ENV)
  }
    
  def concretize[T](f: Formula, e: Expr[T]): T = {
    val v = e.eval(solve(f :: CONSTRAINTS));
    usually(f ==> ((e === e.constant(v)) match {case f: Formula => f}));
    v
  }
}


