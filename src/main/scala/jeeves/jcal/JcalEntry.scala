package cap.jeeves.geoloc

/*
 * Calendar entry records for jcal case study.
 * @author jeanyang
 */

import cap.scalasmt._
import cap.jeeves.JeevesLib._

import scala.collection.mutable.Map

// TODO: How do we do groups?
class JcalEntry( val id : BigInt
                 // TODO: Where do we want to store the friends permission?
                 , _userId : BigInt
                 , _month : BigInt, _day : BigInt
                 , _hour : BigInt, _minute : BigInt, _duration : BigInt
                 , _location : BigInt
                 // Who is invited...
                 , guests : List[IntExpr], groups : List[IntExpr]
                 , context : ObjectVar ) extends Atom {
  val contextLevel : IntVar =
    pick( x => (CONTAINS(guests, context~'id) ==> (x === Viewer.high))
        , Viewer.low)
  // TODO: Do we need objects to have object fields?
  // groups.foreach(g => assume())

  // Reveal the user location.
  val userId = {
    val level = pick(default = Viewer.low);
    mkSensitive(level, _userId);
  }

  private def mkSensitive(levelVar : IntVar, v : IntExpr) : IntExpr = {
    assume(CONTAINS(Viewer.levels, levelVar));
    mkSensitiveValue(Viewer.levels, levelVar, v, Viewer.high)
  }

  override def toString = "jce" + id
  override def hashCode = id.toInt
  override def equals(that: Any) =
    if (that.isInstanceOf[JcalEntry])
      (id == that.asInstanceOf[JcalEntry].id) 
    else 
      false
}
