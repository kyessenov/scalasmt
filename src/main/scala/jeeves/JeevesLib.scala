package cap.jeeves

/*
 * A library for using ScalaSMT for privacy, using symbolic varaibles to
 * represent sensitive values.
 * @author jeanyang
 */

import cap.scalasmt._
import scala.collection.immutable.Map;
import scala.collection.mutable.{Map => MMap};
import scala.collection.mutable.HashMap;

object JeevesLib extends Sceeves {
  trait JeevesRecord extends Atom
  type LevelVar = BoolVar;
  type Symbolic = ObjectExpr[Atom];

  val HIGH = true
  val LOW = false

  val CONTEXT: Symbolic = pickObject();

  def mkLevel(): LevelVar = pickBool(default = LOW)

  def mkSensitiveInt(lvar: LevelVar, high: IntExpr, low: IntExpr = -1): IntVar = {
    val v = pick(default = low);
    assume(lvar ==> (v === high));
    v;
  }

  def mkSensitiveObject(lvar: LevelVar, high: Symbolic, low: Symbolic = NULL): Symbolic = {
    val v = pickObject(default = low);
    assume(lvar ==> (v === high));
    v;
  } 

  def policy(lvar: LevelVar, f: Formula) {
    assume(f ==> lvar);
  }
  def policy(lvar: LevelVar, f: () => Formula) {
    POLICIES = (lvar, f) :: POLICIES
  }
  

  /** 
   * Extend Sceeves functionality in a specific way.
   */
  private var POLICIES: List[(LevelVar, () => Formula)] = Nil

  def concretize[T](ctx: Symbolic, e: Expr[T]) = {
    val context = (CONTEXT === ctx) && AND(POLICIES.map{case (lvar, f) => f() ==> lvar})
    super.concretize(context, e);
  }

  /**
   * Collections of symbolic values.
   */ 
  def concretize[T <: JeevesRecord](ctx: Symbolic, lst: List[Symbolic]): List[T] = 
    for (o <- lst;
      t = concretize(ctx, o).asInstanceOf[T];
      if (t != null))
      yield t;

  def filter[T <: JeevesRecord](lst: List[T], filter: T => Formula) : List[Symbolic] = 
    {for (o <- lst; 
      f = filter(o)) 
      yield f match {    
        case BoolVal(true) => 
          Some(Object(o))
        case BoolVal(false) => 
          None
        case f =>
          val r = pickObject(default = NULL);
          assume (f ==> (r === o))
          Some(r)
      }
    }.flatten
  

}

