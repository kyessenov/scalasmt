package cap.scalasmt

/**
 * AST transformers.
 */

/**
 * Partial evaluation with  
 * constant and equality propagation.
 */
object Partial {
  def eqs(f: Formula)(implicit env: Environment) = {
    var out = env;
    for (c <- f.clauses) c match {    
      case ObjectEq(v: ObjectVar, Object(o)) => out = out + (v -> o)
      case ObjectEq(Object(o), v: ObjectVar) => out = out + (v -> o)
      case IntEq(v: IntVar, Constant(i)) => out = out + (v -> i)
      case IntEq(Constant(i), v: IntVar) => out = out + (v -> i)
      case _ =>
    }
    out
  }

  def eval(f: Formula)(implicit env: Environment): Formula = 
    {f match {
      case BoolConditional(a, b, c) => 
        val sa = eval(a); 
        BoolConditional(sa, eval(b)(eqs(sa)), eval(c)(eqs(eval(! sa))))
      case And(a, b) => And(eval(a), eval(b))
      case Or(a, b) => Or(eval(a), eval(b))
      case Not(f) => Not(eval(f))
      case GT(a, b) => GT(eval(a), eval(b))
      case LT(a, b) => LT(eval(a), eval(b))
      case Geq(a, b) => Geq(eval(a), eval(b))
      case Leq(a, b) => Leq(eval(a), eval(b))
      case IntEq(a, b) => IntEq(eval(a), eval(b))
      case f: RelFormula => f
      case ObjectEq(a, b) => ObjectEq(eval(a), eval(b))
      case b: BoolVar => b
      case TrueF => TrueF
      case FalseF => FalseF
    }} match {
      case f if env.hasAll(f.vars) => f.eval
      case BoolConditional(TrueF, thn, _) => thn
      case BoolConditional(FalseF, _, els) => els
      case BoolConditional(_, a, b) if a == b => a
      case And(FalseF, _) => false
      case And(_, FalseF) => false
      case And(TrueF, x) => x
      case And(x, TrueF) => x
      case Or(FalseF, x) => x
      case Or(x, FalseF) => x
      case Or(TrueF, x) => true
      case Or(x, TrueF) => true
      case Not(Not(f)) => f
      case ObjectEq(a, b) if (a == b) => true
      case IntEq(a, b) if (a == b) => true
      case f => f
    }


  def eval(e: IntExpr)(implicit env: Environment): IntExpr = 
    {e match {
      case IntConditional(a, b, c) => 
        val sa = eval(a) 
        IntConditional(sa, eval(b)(eqs(sa)), eval(c)(eqs(eval(! sa))))
      case Plus(a, b) => Plus(eval(a), eval(b))
      case Minus(a, b) => Minus(eval(a), eval(b))
      case Times(a, b) => Times(eval(a), eval(b))
      case e => e
    }} match {
      case e if env.hasAll(e.vars) => e.eval
      case IntConditional(TrueF, thn, _) => thn
      case IntConditional(FalseF, _, els) => els
      case IntConditional(_, a, b) if a == b => a
      case e => e
    }

  def eval(e: ObjectExpr)(implicit env: Environment): ObjectExpr =
    {e match {
      case ObjectConditional(a, b, c) => 
        val sa = eval(a)
        ObjectConditional(sa, eval(b)(eqs(sa)), eval(c)(eqs(eval(! sa))))
      case e => e
    }} match {
      case e if env.hasAll(e.vars) => e.eval
      case ObjectConditional(TrueF, thn, _) => thn
      case ObjectConditional(FalseF, _, els) => els
      case ObjectConditional(_, a, b) if a == b => a
      case e => e
    }
}

