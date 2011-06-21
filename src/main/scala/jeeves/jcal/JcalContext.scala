package cap.jeeves.jcal

/*
 * User records for jconf case study.
 * @author jeanyang
 */

import cap.scalasmt._
import scala.collection.mutable.Map;
import cap.jeeves.JeevesLib._

object Viewer {
  val low   : LevelTy = 0
  val high  : LevelTy = 1
  val levels = List(low, high)
}

class JcalContext( val id : BigInt, val groups : List[IntExpr] ) extends Atom {

  override def toString = "jcac" + id
  override def hashCode = id.toInt
  override def equals(that: Any) =
    if (that.isInstanceOf[JcalContext])
      (id == that.asInstanceOf[JcalContext].id) 
    else 
      false
}