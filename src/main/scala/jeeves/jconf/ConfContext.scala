package cap.jeeves.jconf

/*
 * User records for jconf case study.
 * @author jeanyang
 */

import cap.scalasmt._
import scala.collection.mutable.Map;
import UserLevels._
import cap.jeeves.JeevesLib._

class ConfContext( val id : BigInt
                 , val status : BigInt
                 , val confStage : BigInt ) extends Atom {

  override def toString = "jcc" + id
  override def hashCode = id.toInt
  override def equals(that: Any) =
    if (that.isInstanceOf[ConfContext])
      (id == that.asInstanceOf[ConfContext].id) 
    else 
      false
}
