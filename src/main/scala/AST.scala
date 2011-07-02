package cap.scalasmt

/*
 * A DSL for logical constraints.
 * @author kuat
 */

/**
 * Expressions.
 */
sealed trait Expr[T] extends Serializable {
  def vars: Set[Var[_]]
  def eval(implicit env: Environment = EmptyEnv): T
  def ===(that: Expr[T]): Expr[Boolean]
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
  private def inc() = {COUNTER = COUNTER + 1; COUNTER.toString}
  def makeInt = IntVar(inc())
  def makeBool = BoolVar(inc())
  def makeObject = ObjectVar(inc())
  def makeObjectSet = ObjectSetVar(inc())
} 
sealed trait BinaryExpr[T <: Expr[_]] {
  assert (left != null)
  assert (right != null)
  def left: T
  def right: T
  def vars = left.vars ++ right.vars
}
sealed trait UnaryExpr[T <: Expr[_]] {
  assert (sub != null)
  def sub: T
  def vars = sub.vars
}
sealed trait Eq[T <: Expr[_]] extends Expr[Boolean] with BinaryExpr[T] {
  def eval(implicit env: Environment) = left.eval == right.eval 
}
sealed trait Constant[T] extends Expr[T] {
  protected def v: T
  def vars = Set()
  def eval(implicit env: Environment) = v
} 



/** 
 * Boolean expressions and algebra.
 */
sealed abstract class Formula extends Expr[Boolean] {
  def ===(that: Expr[Boolean]): Formula = 
    that match {case that: Formula => BoolEq(this, that)}
  def &&(that: Formula) = And(this, that)
  def ||(that: Formula) = Or(this, that)
  def ==> (that: Formula) = Or(Not(this), that)
  def <==> (that: Formula) = ===(that)
  def unary_! = Not(this)
  def ?(thn: Formula) = new {def !(els: Formula) = BoolConditional(Formula.this, thn, els)}
  def ?(thn: IntExpr) = new {def !(els: IntExpr) = IntConditional(Formula.this, thn, els)}
  def ?(thn: ObjectExpr[Atom]) = new {def !(els: ObjectExpr[Atom]) = ObjectConditional(Formula.this, thn, els)}

  def clauses: List[Formula] = this match {
    case And(a,b) => a.clauses ++ b.clauses
    case _ => this :: Nil
  }
}
sealed abstract class BinaryFormula extends Formula with BinaryExpr[Formula] 
case class And(left: Formula, right: Formula) extends BinaryFormula {
  def eval(implicit env: Environment) = left.eval && right.eval
}
case class Or(left: Formula, right: Formula) extends BinaryFormula {
  def eval(implicit env: Environment) = left.eval || right.eval
}
case class BoolConditional(cond: Formula, thn: Formula, els: Formula) extends Formula with Ite[Boolean]
case class Not(sub: Formula) extends Formula with UnaryExpr[Formula] {
  def eval(implicit env: Environment) = ! sub.eval
}

/**
 * Atomic predicates.
 */
case class BoolVal(v: Boolean) extends Formula with Constant[Boolean]
sealed abstract class IntFormula extends Formula with BinaryExpr[IntExpr] 
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
sealed abstract class RelFormula extends Formula with BinaryExpr[RelExpr]
case class RelSub(left: RelExpr, right: RelExpr) extends RelFormula {
  def eval(implicit env: Environment) = left.eval.subsetOf(right.eval)
}
case class BoolVar(id: String) extends Formula with Var[Boolean] {
  def default = true
  override def toString = "b" + id
}

/** 
 * Equality atomic predicates.
 */
case class BoolEq(left: Formula, right: Formula) extends Formula with Eq[Formula]
case class IntEq(left: IntExpr, right: IntExpr) extends IntFormula with Eq[IntExpr] 
case class ObjectEq(left: ObjectExpr[Atom], right: ObjectExpr[Atom]) extends Formula with Eq[ObjectExpr[Atom]]
case class RelEq(left: RelExpr, right: RelExpr) extends RelFormula with Eq[RelExpr]

/**
 * Integer expression with Peano arithmetic.
 */
sealed abstract class IntExpr extends Expr[BigInt] {
  def ===(that: Expr[BigInt]): Formula = 
    that match {case that: IntExpr => IntEq(this, that)}
  def !==(that: IntExpr) = ! (this === that)
  def <=(that: IntExpr) = Leq(this, that)
  def >=(that: IntExpr) = Geq(this, that)
  def <(that: IntExpr) = LT(this, that)
  def >(that: IntExpr) = GT(this, that)
  def unary_- = Minus(IntVal(0), this)
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
case class IntVal(v: BigInt) extends IntExpr with Constant[BigInt]
case class IntConditional(cond: Formula, thn: IntExpr, els: IntExpr) extends IntExpr with Ite[BigInt]
case class IntVar(id: String) extends IntExpr with Var[BigInt] {
  def default = 0
  override def toString = "i" + id
}
case class ObjectIntField(root: ObjectExpr[Atom], f: IntFieldDesc) extends IntExpr {
  def vars = root.vars + IntVar("global" + f.name)
  def eval(implicit env: Environment) = f(root.eval) match {
    case Some(e: IntExpr) => e.eval
    case None => 0
  }
}

/**
 * Object and field expressions.
 */
trait Atom extends AnyRef {
  // Must respect equality but uniquely identify the object
  def uniq = toString.hashCode
}
sealed abstract class ObjectExpr[+T <: Atom] extends Expr[Atom] { 
  def ===(that: Expr[Atom]): Formula = 
    that match {case that: ObjectExpr[_] => ObjectEq(this, that)}
  def ~(f: Symbol) = ObjectIntField(this, IntFieldDesc(f.name))
  def /(f: Symbol) = ObjectField(this, ObjectFieldDesc(f.name))
  def eval(implicit env: Environment): T
}
case class ObjectConditional[T <: Atom](cond: Formula, thn: ObjectExpr[T], els: ObjectExpr[T]) extends ObjectExpr[T] with Ite[Atom] {
  override def eval(implicit env: Environment): T = if (cond.eval) thn.eval else els.eval
}
case class Object[T <: Atom](v: T) extends ObjectExpr[T] with Constant[Atom] {
  override def eval(implicit env: Environment): T = v
}
case class ObjectVar(id: String) extends ObjectExpr[Atom] with Var[Atom] {
  def default = null
  override def toString = "a" + id
}
case class ObjectField(root: ObjectExpr[Atom], f: ObjectFieldDesc) extends ObjectExpr[Atom] {
  def vars = root.vars + ObjectVar("global" + f.name)
  def eval(implicit env: Environment) = f(root.eval) match {
    case Some(o: ObjectExpr[_]) => o.eval
    case None => null
  }
}

/**
 * Object fields.
 */
sealed trait FieldDesc[U] {
  def name: String
  /** Evaluate to an expression */
  def apply(o: Atom): Option[Expr[U]]
  /** Value for elements not in the domain */
  def default: U
  /** Read from the heap.*/
  protected def read(o: Atom) = if (o == null) None else
    try {
      val fid = o.getClass.getDeclaredField(name);
      fid.setAccessible(true);
      if ((fid.getModifiers | java.lang.reflect.Modifier.FINAL) == 0)
        throw new RuntimeException("non-final fields are disallowed")
      try Some(fid.get(o)) finally fid.setAccessible(false);
    } catch {
      case _: NoSuchFieldException => None 
    }
}
case class IntFieldDesc(name: String) extends FieldDesc[BigInt] {
  override def default = 0
  override def apply(o: Atom) = read(o) match {
    case Some(e: IntExpr) => Some(e)    
    case Some(e: BigInt) => Some(IntVal(e))
    case _ => None
  }
}
case class ObjectFieldDesc(name: String) extends FieldDesc[Atom] {
  override def default = null
  override def apply(o: Atom) = read(o) match {
    case Some(null) => Some(Object(null))
    case Some(o: Atom) => Some(Object(o))
    case Some(o: ObjectExpr[_]) => Some(o: ObjectExpr[Atom])
    case _ => None
  }
}

/**
 * Relational algebra.
 */
sealed abstract class RelExpr extends Expr[Set[Atom]] {
  def ===(that: Expr[Set[Atom]]): Formula = 
    that match {case that: RelExpr => RelEq(this, that)}
  def in(that: RelExpr) = RelSub(this, that)
  def &(that: RelExpr) = Intersect(this, that)
  def ++(that: RelExpr) = Union(this, that)
  def --(that: RelExpr) = Diff(this, that)
  def ><(f: Symbol) = RelJoin(this, ObjectFieldDesc(f.name)) 
}
case class Singleton(sub: ObjectExpr[Atom]) extends RelExpr with UnaryExpr[ObjectExpr[Atom]] {
  def eval(implicit env: Environment) = Set(sub.eval)
}
case class ObjectSet(v: Set[Atom]) extends RelExpr with Constant[Set[Atom]] 
case class ObjectSetVar(id: String) extends RelExpr with Var[Set[Atom]] {
  def default = Set()
  override def toString = "s" + id
}
case class RelJoin(root: RelExpr, f: ObjectFieldDesc) extends RelExpr {
  def vars = root.vars
  def eval(implicit env: Environment) = (for (o <- root.eval) yield f(o)).flatten.map(_.eval)
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
sealed trait Environment {
  def vars: Set[Var[_]]
  def +[T](b: (Var[T], T)): Environment = Binding(b._1, b._2, this)
  def has[T](i: Var[T]): Boolean
  def apply[T](i: Var[T]): T
  def hasAll(vs: Traversable[Var[_]]) = vs.forall(has(_))
}
class UnboundVarException(i: Var[_]) extends RuntimeException("unbound variable: " + i) 
object EmptyEnv extends Environment {
  def vars = Set()
  def has[T](i: Var[T]) = false
  def apply[T](i: Var[T]) = throw new UnboundVarException(i)
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
object Expr {
  implicit def fromBigInt(i: BigInt) = IntVal(i)  
  implicit def fromInt(i: Int) = IntVal(i)
  implicit def fromBool(b: Boolean) = BoolVal(b)
  implicit def fromAtom[T <: Atom](o: T) = Object(o)
}
object Formula {
  implicit def fromList(vs: Traversable[Formula]) = vs.foldLeft(true: Formula)(_ && _)
}
object RelExpr {
  implicit def fromRef(o: Atom) = Singleton(Object(o))
  implicit def fromSingleton(o: ObjectExpr[Atom]) = Singleton(o)
  implicit def fromRefSet(s: Traversable[_ <: Atom]) = ObjectSet(s.toSet)
}
object `package` {
  case class IF(cond: Formula) {
    def apply(thn: IntExpr) = new {def ELSE(els: IntExpr) = cond ? thn ! els}
  }
  def DISTINCT[T <% IntExpr](vs: Traversable[T]) = 
    for (vs1 <- vs; vs2 <- vs; if (vs1 != vs2)) yield ( ! (vs1 === vs2))
  def NULL = Object(null)
  def OR(vs: Traversable[Formula]) = vs.foldLeft(false: Formula)(_ || _)
  def AND(vs: Traversable[Formula]) = vs.foldLeft(true: Formula)(_ && _)
  def CONTAINS[T <% IntExpr](vs: Traversable[T], i: IntExpr) = 
    OR(for (v <- vs) yield v === i)
  def CONTAINS[T <% ObjectExpr[Atom]](vs: Traversable[T], i: ObjectExpr[Atom]) = 
    OR(for (v <- vs) yield v === i)
}


