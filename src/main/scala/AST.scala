package cap.scalasmt

/*
 * A DSL for logical constraints.
 * @author kuat
 */

/**
 * Expressions.
 */
@serializable sealed trait Expr[+T] {
  def vars: Set[Var[_]]
  def eval(implicit env: Environment = EmptyEnv): T
}
sealed trait Ite[T] extends Expr[T] {
  def cond: Expr[Boolean]
  def thn: Expr[T]
  def els: Expr[T]
  def vars = cond.vars ++ thn.vars ++ els.vars
  def eval(implicit env: Environment)= if (cond.eval) thn.eval else els.eval
}
sealed trait Var[T] extends Expr[T] {
  def id: String
  def default: T
  def vars: Set[Var[_]] = Set(this)
  def eval(implicit env: Environment) = env(this)
}
object Var {
  private var COUNTER = 0
  private def inc = {COUNTER = COUNTER + 1; COUNTER.toString}
  def makeInt = IntVar(inc)
  def makeBool = BoolVar(inc)
  def makeAtom = AtomVar(inc)
  def makeAtomSet = AtomSetVar(inc)
} 
sealed trait BinaryExpr[T <: Expr[_]] {
  assert (left != null)
  assert (right != null)
  def left: T
  def right: T
  def vars = left.vars ++ right.vars
}
sealed trait Eq[T <: Expr[_]] extends Expr[Boolean] with BinaryExpr[T] {
  def eval(implicit env: Environment) = left.eval == right.eval 
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
  def ?(thn: ObjectExpr) = new {def !(els: ObjectExpr) = AtomConditional(Formula.this, thn, els)}

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
case class IntEq(left: IntExpr, right: IntExpr) extends IntFormula with Eq[IntExpr] 
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
case class ObjectEq(left: ObjectExpr, right: ObjectExpr) extends Formula with Eq[ObjectExpr]
sealed abstract class RelFormula extends Formula with BinaryExpr[RelExpr]
case class RelEq(left: RelExpr, right: RelExpr) extends RelFormula with Eq[RelExpr]
case class RelSub(left: RelExpr, right: RelExpr) extends RelFormula {
  def eval(implicit env: Environment) = left.eval.subsetOf(right.eval)
}


/**
 * Integer expression with Peano arithmetic.
 */
sealed abstract class IntExpr extends Expr[BigInt] {
  def ===(that: IntExpr) = IntEq(this, that)
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
 * Atom equality theory.
 */
trait Atom extends AnyRef
sealed abstract class ObjectExpr extends Expr[Atom] { 
  def ===(that: ObjectExpr) = ObjectEq(this, that)
  def ++(that: ObjectExpr) = Union(Singleton(this), Singleton(that))
}
case class AtomConditional(cond: Formula, thn: ObjectExpr, els: ObjectExpr) extends ObjectExpr with Ite[Atom]
case class Object(o: Atom) extends ObjectExpr {
  def vars = Set()
  def eval(implicit env: Environment) = o
}
case class AtomVar(id: String) extends ObjectExpr with Var[Atom] {
  def default = null
  override def toString = "a" + id
}

/**
 * Relational algebra.
 */
sealed abstract class RelExpr extends Expr[Set[Atom]] {
  def ===(that: RelExpr) = RelEq(this, that)
  def in(that: RelExpr) = RelSub(this, that)
  def &(that: RelExpr) = Intersect(this, that)
  def ++(that: RelExpr) = Union(this, that)
  def --(that: RelExpr) = Diff(this, that)
  def ~(name: Symbol) = Join(this, FieldDesc(name.name)) 
}
case class Singleton(sub: ObjectExpr) extends RelExpr {
  def vars = sub.vars
  def eval(implicit env: Environment) = Set(sub.eval)
}
case class ObjectSet(elts: Traversable[_ <: Atom]) extends RelExpr {
  def vars = Set()
  def eval(implicit env: Environment) = elts.toSet[Atom]
}
case class AtomSetVar(id: String) extends RelExpr with Var[Set[Atom]] {
  def default = Set()
  override def toString = "s" + id
}
case class FieldDesc(name: String)
case class Join(root: RelExpr, f: FieldDesc) extends RelExpr {
  def vars = root.vars
  def eval(implicit env: Environment) = 
    (for (o <- root.eval; if o != null) yield {
      try {
        o.getClass.getDeclaredMethod(f.name).invoke(o) match {
          case null => Some(null)
          case o: Atom => Some(o)
          case _ => None
        }
      } catch {
        case _: NoSuchMethodException => None
      }
    }).flatten 
}
sealed abstract class BinaryRelExpr extends RelExpr with BinaryExpr[RelExpr]
case class Union(left: RelExpr, right: RelExpr) extends BinaryRelExpr {
  def eval(implicit inv: Environment) = left.eval ++ right.eval
}
case class Diff(left: RelExpr, right: RelExpr) extends BinaryRelExpr {
  def eval(implicit inv: Environment) = left.eval -- right.eval
}
case class Intersect(left: RelExpr, right: RelExpr) extends BinaryRelExpr {
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
  def apply[T](i: Var[T]) = throw new RuntimeException("no binding")
}
object DefaultEnv extends Environment {
  def vars = Set()
  def has[T](i: Var[T]) = false
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
object ObjectExpr {
  implicit def fromRef(o: Atom) = Object(o)
}
object RelExpr {
  implicit def fromRef(o: Atom) = Singleton(Object(o))
  implicit def fromSingleton(o: ObjectExpr) = Singleton(o)
  implicit def fromRefSet(s: Traversable[_ <: Atom]) = ObjectSet(s)
}
object `package` {
  def IF(cond: Formula)(thn: IntExpr) = new {def ELSE(els: IntExpr) = cond ? thn ! els}
  def IF(cond: Formula)(thn: Formula) = new {def ELSE(els: Formula) = cond ? thn ! els}
  def DISTINCT(vs: Traversable[IntExpr]) = 
    for (vs1 <- vs; vs2 <- vs; if (vs1 != vs2)) yield ( ! (vs1 === vs2))
  def NULL = Object(null)
}

