package cap.jeeves

/*
 * A library for using ScalaSMT for privacy, using symbolic varaibles to
 * represent sensitive values.
 * @author jeanyang, kuat
 */

import cap.scalasmt._
import scala.collection.immutable.Map;
import scala.collection.mutable.{Map => MMap};
import scala.collection.mutable.HashMap;

trait JeevesLib extends Sceeves {
  trait JeevesRecord extends Atom {
    register(this)
  }
  type LevelVar = BoolVar;
  type Symbolic = ObjectExpr[Atom];
  
  sealed trait Level
  object HIGH extends Level
  object LOW extends Level
  implicit def level2sym(l: Level): Formula = l match {
    case HIGH => true
    case LOW => false
  }

  val CONTEXT: Symbolic = pickObject();
  
  private var POLICIES: List[(LevelVar, Level, () => Formula)] = Nil

  def mkLevel(): LevelVar = pickBool(_ => true, HIGH)

  def mkSensitiveInt(lvar: LevelVar, high: IntExpr, low: IntExpr = -1): IntVar = {
    val v = pick();;
    assume(v === (lvar ? high ! low));
    v;
  }

  def mkSensitiveObject(lvar: LevelVar, high: Symbolic, low: Symbolic = NULL): Symbolic = {
    val v = pickObject()
    assume(v === (lvar ? high ! low));
    v;
  } 

  def policy(lvar: LevelVar, f: Formula, value: Level = LOW) {
    assume(f ==> (lvar === value));
  }
  def policy(lvar: LevelVar, f: () => Formula) {
    POLICIES = (lvar, LOW, f) :: POLICIES
  }
  
  override def assume(f: Formula) = super.assume(Partial.eval(f)(EmptyEnv))

  def concretize[T](ctx: Symbolic, e: Expr[T]) = {
    val context = (CONTEXT === ctx) && AND(POLICIES.map{
        case (lvar, level, f) => f() ==> (lvar === level)
      })
    super.concretize(context, e);
  }

  /**
   * Collections of symbolic values.
   */ 

  def concretize[T](ctx: Symbolic, e: (Expr[T], Expr[T])): (T, T) = 
    (concretize(ctx, e._1), concretize(ctx, e._2))

  def concretize[T <: JeevesRecord](ctx: Symbolic, lst: List[Symbolic]): List[T] = 
    for (o <- lst;
      t = concretize(ctx, o).asInstanceOf[T];
      if (t != null))
      yield t;

  def filter[T >: Null <: JeevesRecord](lst: List[T], filter: T => Formula) : List[Symbolic] = 
    for(o <- lst) yield filter(o) match {    
      case BoolVal(true) => 
        o: Symbolic
      case BoolVal(false) => 
        null: Symbolic
      case f =>
        pickObject(_ === (IF (f) {o} ELSE {NULL}))
    }
}

