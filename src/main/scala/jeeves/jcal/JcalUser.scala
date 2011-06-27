package cap.jeeves.jcal

/*
 * User records for jconf case study.
 * @author jeanyang
 */

import cap.scalasmt._
import scala.collection.mutable.Map;
import cap.jeeves.JeevesLib._

class JcalUser( val id : BigInt
             , _pwd : BigInt
             , groups : List[IntExpr]
             , val context : ObjectVar ) extends Atom {
  val contextLevel = pick(default = Viewer.low);
  assume((context~'id === id) ==> (contextLevel === Viewer.high));

  val pwd = {
    val level = pick(default = Viewer.low);
    mkSensitive(level, _pwd);
  }

  private def mkSensitive(levelVar : IntVar, v : IntExpr) : IntExpr = {
    assume(CONTAINS(Viewer.levels, levelVar));
    assume((context~'id === id) ==> (levelVar === Viewer.high));
    mkSensitiveValue(Viewer.levels, levelVar, v, Viewer.high)
  }

  // TODO: Groups
  /*
  // This only operates over logical variables.
  def addFriend (friendId : BigInt) = {
    (context~'id === friendId) ==> (contextLevel === Viewer.high);
  }
  */

  override def toString = "gu" + id
  override def hashCode = id.toInt
  override def equals(that: Any) =
    if (that.isInstanceOf[JcalUser])
      (id == that.asInstanceOf[JcalUser].id) 
    else 
      false
}
