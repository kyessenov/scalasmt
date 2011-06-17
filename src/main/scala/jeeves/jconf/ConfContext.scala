package cap.jeeves.jconf

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

class ConfContext( val id : BigInt, val name : BigInt
                 , val status : BigInt
                 , val stage : BigInt ) extends Atom {

  override def toString = "jcc" + id
  override def hashCode = id.toInt
  override def equals(that: Any) =
    if (that.isInstanceOf[ConfContext])
      (id == that.asInstanceOf[ConfContext].id) 
    else 
      false
}
