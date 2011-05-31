package cap.scalasmt

/*
 * A DSL for logical constraints.
 * @author kuat
 */

/**
 * Expressions.
 */
@serializable sealed trait Expr[T] {
  def vars: Set[Var[_]]
  def eval(implicit env: Environment): T
}
sealed trait Ite[T] extends Expr[T] {
  def cond: Expr[Boolean]
  def thn: Expr[T]
  def els: Expr[T]
  def vars = cond.vars ++ thn.vars ++ els.vars
  def eval(implicit env: Environment): T = if (cond.eval) thn.eval else els.eval
}
sealed trait Var[T] extends Expr[T] {
  def id: String
  def default: T
  def vars: Set[Var[_]] = Set(this)
  def eval(implicit env: Environment) = env(this)
}
object Var {
  private var COUNTER = 0
  private def inc {COUNTER = COUNTER + 1}
  def makeInt = {inc; IntVar(COUNTER.toString)}
  def makeBool = {inc; BoolVar(COUNTER.toString)}
} 
sealed trait BinaryExpr[T <: Expr[_]] {
  def left: T
  def right: T
  def vars = left.vars ++ right.vars
}

/** 
 * Boolean expressions and algebra.
 */
sealed abstract class Formula extends Expr[Boolean] {
  def &&(that: Formula) = And(this, that)
  def ||(that: Formula) = Or(this, that)
  def ==> (that: Formula) = Or(Not(this), that)
  def ===(that: Formula) = (this && that) || (! this && ! that);
  def unary_! = Not(this)
  def ?(thn: Formula) = new {def !(els: Formula) = BoolConditional(Formula.this, thn, els)}
  def ?(thn: IntExpr) = new {def !(els: IntExpr) = IntConditional(Formula.this, thn, els)}

  def clauses: List[Formula] = this match {
    case And(a,b) => a.clauses ++ b.clauses
    case _ => this :: Nil
  }
}
sealed abstract class BinaryFormula extends Formula with BinaryExpr[Formula] 
case class And(left: Formula, right: Formula = TrueF) extends BinaryFormula {
  def eval(implicit env: Environment) = left.eval && right.eval
}
case class Or(left: Formula, right: Formula = FalseF) extends BinaryFormula {
  def eval(implicit env: Environment) = left.eval || right.eval
}
case class BoolConditional(cond: Formula, thn: Formula, els: Formula) extends Formula with Ite[Boolean]
case class Not(sub: Formula) extends Formula {
  def vars = sub.vars
  def eval(implicit env: Environment) = ! sub.eval
}

/**
 * Atomic predicates.
 */
case object TrueF extends Formula {
  def vars = Set()
  def eval(implicit env: Environment) = true
}
case object FalseF extends Formula {
  def vars = Set()
  def eval(implicit env: Environment) = false
}
sealed abstract class IntFormula extends Formula with BinaryExpr[IntExpr] 
case class Eq(left: IntExpr, right: IntExpr) extends IntFormula {
  def eval(implicit env: Environment) = left.eval == right.eval
}
case class Leq(left: IntExpr, right: IntExpr) extends IntFormula {
  def eval(implicit env: Environment) = left.eval <= right.eval
}
case class Geq(left: IntExpr, right: IntExpr) extends IntFormula {
  def eval(implicit env: Environment) = left.eval >= right.eval
}
case class LT(left: IntExpr, right: IntExpr) extends IntFormula {
  def eval(implicit env: Environment) = left.eval < right.eval
}
case class GT(left: IntExpr, right: IntExpr) extends IntFormula {
  def eval(implicit env: Environment) = left.eval > right.eval
}
case class BoolVar(id: String) extends Formula with Var[Boolean] {
  def default = true
  override def toString = "b" + id
}
sealed abstract class AtomFormula extends Formula with BinaryExpr[AtomExpr]
case class AEq(left: AtomExpr, right: AtomExpr) extends AtomFormula {
  def eval(implicit env: Environment) = left.eval == right.eval
}
case class ASub(left: AtomExpr, right: AtomExpr) extends AtomFormula {
  def eval(implicit env: Environment) = left.eval.subsetOf(right.eval)
}


/**
 * Integer expression with Peano arithmetic.
 */
sealed abstract class IntExpr extends Expr[BigInt] {
  def ===(that: IntExpr) = Eq(this, that)
  def !==(that: IntExpr) = ! (this === that)
  def <=(that: IntExpr) = Leq(this, that)
  def >=(that: IntExpr) = Geq(this, that)
  def <(that: IntExpr) = LT(this, that)
  def >(that: IntExpr) = GT(this, that)
  def unary_- = Minus(Constant(0), this)
  def +(that: IntExpr) = Plus(this, that)
  def -(that: IntExpr) = Minus(this, that)
  def *(that: IntExpr) = Times(this, that)
}
sealed abstract class BinaryIntExpr extends IntExpr with BinaryExpr[IntExpr]
case class Plus(left: IntExpr, right: IntExpr) extends BinaryIntExpr {
  def eval(implicit env: Environment) = left.eval + right.eval
}
case class Minus(left: IntExpr, right: IntExpr) extends BinaryIntExpr {
  def eval(implicit env: Environment) = left.eval - right.eval
}
case class Times(left: IntExpr, right: IntExpr) extends BinaryIntExpr {
  def eval(implicit env: Environment) = left.eval * right.eval
}
case class Constant(i: BigInt) extends IntExpr {
  def vars = Set()
  def eval(implicit env: Environment) = i
}
case class IntConditional(cond: Formula, thn: IntExpr, els: IntExpr) extends IntExpr with Ite[BigInt]
case class IntVar(id: String) extends IntExpr with Var[BigInt] {
  def default = 0
  override def toString = "i" + id
}


/**
 * Relational algebra.
 */
sealed abstract class AtomExpr extends Expr[Set[Atom]] {
  def ===(that: AtomExpr) = AEq(this, that)
  def in(that: AtomExpr) = ASub(this, that)
  def &(that: AtomExpr) = Intersect(this, that)
  def ++(that: AtomExpr) = Union(this, that)
  def --(that: AtomExpr) = Diff(this, that)
  def apply(name: String) = Join(this, FieldDesc(name)) 
}
sealed abstract class Atom extends AtomExpr 
case class AtomObject(o: AnyRef) extends Atom {
  def vars = Set()
  def eval(implicit env: Environment) = Set(this)
}
case class AtomVar(id: String) extends Atom with Var[Set[Atom]] {
  def default = Set(AtomObject(null))
  override def toString = "a" + id
}
case class AtomSet(elts: Set[Atom]) extends AtomExpr {
  def vars = elts.collect{case a: AtomVar => a}
  def eval(implicit env: Environment) = elts
}
case class FieldDesc(name: String)
case class Join(left: AtomExpr, f: FieldDesc) extends AtomExpr {
  def vars = left.vars
  def eval(implicit env: Environment) = 
    (for (atom <- left.eval) yield {
      val o = atom match {
        case AtomObject(o) => o
        case v: AtomVar => val value = env(v)
          assert(value.size == 1)
          value.head match {
            case AtomObject(o) => o
            case _ => throw new RuntimeException("unexpected value of an atom var")
          }
      }

      try {
        Some(AtomObject(o.getClass.getDeclaredMethod(f.name).invoke(o)))
      } catch {
        case _: NoSuchMethodException => None
      }
    }).flatten 
}
case class Union(left: AtomExpr, right: AtomExpr) extends AtomExpr with BinaryExpr[AtomExpr] {
  def eval(implicit inv: Environment) = left.eval ++ right.eval
}
case class Diff(left: AtomExpr, right: AtomExpr) extends AtomExpr with BinaryExpr[AtomExpr] {
  def eval(implicit inv: Environment) = left.eval diff right.eval
}
case class Intersect(left: AtomExpr, right: AtomExpr) extends AtomExpr with BinaryExpr[AtomExpr] {
  def eval(implicit inv: Environment) = left.eval & right.eval
}

/** 
 * Environment.
 */
@serializable trait Environment {
  def vars: Set[Var[_]]
  def +[T](b: (Var[T], T)): Environment = Binding(b._1, b._2, this)
  def has[T](i: Var[T]): Boolean
  def apply[T](i: Var[T]): T
}
object EmptyEnv extends Environment {
  def vars = Set()
  def has[T](i: Var[T]) = false
  // Variables outside the environment evaluate to default value.
  def apply[T](i: Var[T]) = i.default
}
case class Binding[U](bi: Var[U], bv: U, parent: Environment) extends Environment {
  def vars = parent.vars + bi
  def has[T](i: Var[T]) = (i == bi) || parent.has(i)
  def apply[T](i: Var[T]) = if (i == bi) bv.asInstanceOf[T] else parent(i)
}

/**
 * Implicit conversions.
 */
object IntExpr {
  implicit def fromBigInt(i: BigInt) = Constant(i)  
  implicit def fromInt(i: Int) = Constant(i)
}
object Formula {
  implicit def fromBool(i: Boolean) = if (i) TrueF else FalseF
  implicit def fromFormulaList(l: Traversable[Formula]) = 
    l.foldLeft(TrueF: Formula)((l, f) => l && f)
}
object AtomExpr {
  implicit def fromAnyRef(o: AnyRef) = AtomObject(o)
}
object `package` {
  def IF(cond: Formula)(thn: IntExpr) = new {def ELSE(els: IntExpr) = cond ? thn ! els}
  def IF(cond: Formula)(thn: Formula) = new {def ELSE(els: Formula) = cond ? thn ! els}
  def DISTINCT(vs: Traversable[IntExpr]) = 
    for (vs1 <- vs; vs2 <- vs; if (vs1 != vs2)) yield ( ! (vs1 === vs2))
}

