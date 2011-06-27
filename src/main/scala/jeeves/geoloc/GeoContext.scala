package cap.jeeves.geoloc

/*
 * User records for jconf case study.
 * @author jeanyang
 */

import cap.scalasmt._
import scala.collection.mutable.Map;
import cap.jeeves.JeevesLib._

class GeoContext( val id : BigInt, val name : BigInt
                 , val xCoord : BigInt, val yCoord : BigInt
                 , val loc : BigInt ) extends Atom {

  override def toString = "jcc" + id
  override def hashCode = id.toInt
  override def equals(that: Any) =
    if (that.isInstanceOf[GeoContext])
      (id == that.asInstanceOf[GeoContext].id) 
    else 
      false
}
