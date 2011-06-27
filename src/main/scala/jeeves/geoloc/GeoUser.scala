package cap.jeeves.geoloc

/*
 * User records for jconf case study.
 * @author jeanyang
 */

import cap.scalasmt._
import scala.collection.mutable.Map;
import cap.jeeves.JeevesLib._

class GeoUser( val id : BigInt
             , _pwd : BigInt
             , val context : ObjectVar ) extends Atom {
  val contextLevel = pick(default = Viewer.low);
  assume((context~'id === id) ==> (contextLevel === Viewer.high));

  val pwd = {
    val level = pick(default = Viewer.low);
    mkSensitive(level, _pwd);
  }

  private def mkSensitive(levelVar : IntVar, v : IntExpr) : IntExpr = {
    assume((context~'id === id) ==> (levelVar === Viewer.high));
    mkSensitiveValue(levelVar, v)
  }

  // This only operates over logical variables.
  def addFriend (friendId : BigInt) = {
    (context~'id === friendId) ==> (contextLevel === Viewer.high);
  }

  override def toString = "gu" + id
  override def hashCode = id.toInt
  override def equals(that: Any) =
    if (that.isInstanceOf[GeoUser])
      (id == that.asInstanceOf[GeoUser].id) 
    else 
      false
}
