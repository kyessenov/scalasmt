package cap.jeeves.geoloc

/*
 * User records for jconf case study.
 * @author jeanyang
 */

import cap.scalasmt._
import cap.jeeves.JeevesLib._

import scala.collection.mutable.Map

class GeoLocation( val id : BigInt
                 // TODO: Where do we want to store the friends permission?
                 , _userId : BigInt
                 , _xCoord : BigInt, _yCoord : BigInt
                 , _location : BigInt
                 , contextLevel : IntVar
                 , context : ObjectVar ) extends Atom {
  val contextHigh = contextLevel === Viewer.high

  // Reveal the user location.
  val userId = {
    val level = pick(default = Viewer.low);
    assume(contextHigh ==> (level === Viewer.high));
    mkSensitive(level, _userId);
  }

  def abs (v : IntExpr) : IntExpr = (v >= 0) ? {v} ! {-v}
  def distance(x : IntExpr, y : IntExpr) =  abs(x - _xCoord) + abs(y - yCoord)
  val isNear = (distance(context~'xCoord, context~'yCoord) < 10);

  // Reveal coordinates.
  val coordLevel = pick(default = Viewer.low);
  assume((contextHigh && isNear) ==> (coordLevel === Viewer.high));
  val xCoord = mkSensitive(coordLevel, _userId);
  val yCoord = mkSensitive(coordLevel, _userId);

  // Reveal location.
  val location = {
    val level = pick(default = Viewer.low);
    assume((contextHigh && isNear)  ==> (level === Viewer.high));
    mkSensitive(level, _location);
  }

  private def mkSensitive(levelVar : IntVar, v : IntExpr) : IntExpr = {
    assume(CONTAINS(Viewer.levels, levelVar));
    mkSensitiveValue(Viewer.levels, levelVar, v, Viewer.high)
  }

  override def toString = "gl" + id
  override def hashCode = id.toInt
  override def equals(that: Any) =
    if (that.isInstanceOf[GeoLocation])
      (id == that.asInstanceOf[GeoLocation].id) 
    else 
      false
}
