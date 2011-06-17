package cap.jeeves.jconf

/*
 * User records for jconf case study.
 * @author jeanyang
 */

import cap.scalasmt._
import scala.collection.mutable.Map;
import cap.jeeves.JeevesLib._

object UserStatus {
  val authorL  : BigInt = 0
  val reviewerL : BigInt = 1
  val pcL      : BigInt = 2
}

/* Conference User */
class ConfUser( val id : BigInt
              , _name : IntExpr, _pwd : IntExpr, _email : IntExpr
              , val status : BigInt, val context : AtomVar ) extends Atom {

  // TODO: Who can see the names depends on what stage it is and what the role
  // is.
  val name = {
    val level = pick(default = Viewer.low);
    assume((context~'status >= UserStatus.reviewerL) ==>
            (level === Viewer.high));
    mkSensitive(level, _name);
  }
  val pwd = {
    val level = pick(default = Viewer.low);
    mkSensitive(level, _pwd);
  }
  val email = {
    val level = pick(default = Viewer.low);
    assume((context~'status === UserStatus.pcL) ==> (level === Viewer.high));
    mkSensitive(level, _email);
  }

  private def mkSensitive(levelVar : IntVar, v : IntExpr) : IntExpr = {
    assume(CONTAINS(Viewer.levels, levelVar));
    assume((context~'id === id) ==> (levelVar === Viewer.high));
    mkSensitiveValue(Viewer.levels, levelVar, v, Viewer.high)
  }

  override def toString = "jcu" + id
  override def hashCode = id.toInt
  override def equals(that: Any) =
    if (that.isInstanceOf[ConfUser])
      (id == that.asInstanceOf[ConfUser].id) 
    else 
      false
}
