package cap.scalasmt

/*
 * A DSL for arithmetic constraints.
 * @author kuat
 */
sealed abstract class Formula {
  def &&(that: Formula) = And(this, that)
  def ||(that: Formula) = Or(this, that)
  // TODO: what is the precedence?
  def ==> (that: Formula) = Or(Not(this), that)
  def ===(that: Formula) = (this && that) || (! this && ! that);
  def unary_! = Not(this)

  def vars: Set[Var] = this match {
    case c: BinaryFormula => c.left.vars ++ c.right.vars
    case Not(sub) => sub.vars
    case c: BinaryAtomic => c.left.vars ++ c.right.vars
    case TrueF => Set()
    case FalseF => Set()     
  }
  def eval: Boolean = this match {
    case And(l, r) => l.eval && r.eval;
    case Or(l, r) => l.eval || r.eval;
    case Not(s) => ! s.eval;
    case Eq(l, r) => l.eval == r.eval;
    case Leq(l, r) => l.eval <= r.eval;
    case Geq(l, r) => l.eval >= r.eval;
    case LT(l, r) => l.eval < r.eval;
    case GT(l, r) => l.eval > r.eval;
    case TrueF => true;
    case FalseF => false;
  }  
}
sealed abstract class BinaryFormula extends Formula {
  def left: Formula;
  def right: Formula;
}
case class And(left: Formula, right: Formula = TrueF) extends BinaryFormula
case class Or(left: Formula, right: Formula = FalseF) extends BinaryFormula
case class Not(sub: Formula) extends Formula
// atomic formulas
sealed abstract class Atomic extends Formula 
sealed abstract class BinaryAtomic extends Atomic {
  def left: Expr;
  def right: Expr;
}
sealed abstract class IntFormula extends BinaryAtomic
case class Eq(left: IntExpr, right: IntExpr) extends IntFormula
case class Leq(left: IntExpr, right: IntExpr) extends IntFormula
case class Geq(left: IntExpr, right: IntExpr) extends IntFormula
case class LT(left: IntExpr, right: IntExpr) extends IntFormula
case class GT(left: IntExpr, right: IntExpr) extends IntFormula
case object TrueF extends Atomic
case object FalseF extends Atomic
// expressions
sealed abstract class Expr {
  def vars: Set[Var]
} 
sealed abstract class IntExpr extends Expr {
  def ===(that: IntExpr) = Eq(this, that)
  def !==(that: IntExpr) = ! (this === that)
  def <=(that: IntExpr) = Leq(this, that)
  def >=(that: IntExpr) = Geq(this, that)
  def <(that: IntExpr) = LT(this, that)
  def >(that: IntExpr) = GT(this, that)
  def unary_- = Minus(Num(0), this)
  def +(that: IntExpr) = Plus(this, that)
  def -(that: IntExpr) = Minus(this, that)
  def *(that: IntExpr) = Times(this, that)

  override def vars = this match {
    case c: BinaryIntExpr => c.left.vars ++ c.right.vars;
    case IfThenElse(cond, thn, els) => cond.vars ++ thn.vars ++ els.vars;
    case v: IntVar => Set(v)
    case n: Num => Set()
  }
  // TODO: make aware of overflow
  def eval: Int = this match { 
    case Plus(l, r) => l.eval + r.eval;
    case Minus(l, r) => l.eval - r.eval;
    case Times(l, r) => l.eval * r.eval;
    case IfThenElse(cond, thn, els) => if (cond.eval) thn.eval else els.eval;
    case Num(i) => i
    case v: IntVar => v.value
  }
}
sealed abstract class BinaryIntExpr extends IntExpr {
  def left: Expr;
  def right: Expr;
}
case class Plus(left: IntExpr, right: IntExpr) extends BinaryIntExpr
case class Minus(left: IntExpr, right: IntExpr) extends BinaryIntExpr
case class Times(left: IntExpr, right: IntExpr) extends BinaryIntExpr
case class IfThenElse(cond: Formula, thn: IntExpr, els: IntExpr) extends IntExpr
case class Num(i: Int) extends IntExpr
trait Var {
  def id: Int;
  def assigned: Boolean;
}
// invariant: one var per id
object IntVar {
  private var COUNTER = 0;
  def make = {
    COUNTER = COUNTER + 1;
    IntVar(COUNTER);
  }
}
case class IntVar private(id: Int) extends IntExpr with Var {
  override def toString = "ivar" + id;
  def copy: IntVar = throw new RuntimeException;

  private var rep: Option[Int] = None
  def assigned = rep.isDefined
  def value_=(i: Int) {
    assert (! assigned);
    rep = Some(i);
  }
  def value = rep match {
    case Some(i) => i;
    case None => throw new RuntimeException("no value assigned to " + this);
  }
}

// compiler looks up implicit conversions here 
object Expr {
  implicit def fromInt(i: Int) = Num(i)  
}
object Formula {
  implicit def fromBool(i: Boolean) = if (i) TrueF else FalseF
  implicit def fromFormulaList(l: Traversable[Formula]) = 
    l.foldLeft(TrueF: Formula)((l, f) => l && f)
}
object `package` {
  def IF(cond: Formula)(thn: IntExpr) = new {def ELSE(els: IntExpr) = IfThenElse(cond, thn, els)}
  def DISTINCT(vs: Traversable[IntVar]) = 
    for (vs1 <- vs; vs2 <- vs; if (vs1 != vs2)) yield ( ! (vs1 === vs2))
}

