package cap.jeeves.geoloc

/*
 * User records for jconf case study.
 * @author jeanyang
 */

import cap.scalasmt._
import scala.collection.mutable.Map;
import cap.jeeves.JeevesLib._

object UserLevels {
  val userDefaultL : LevelTy = 0;
  val selfL        : LevelTy = 1;
  val levels = List(userDefaultL, selfL);
}

class GeoUser( val id : BigInt
              , _name : IntExpr, _pwd : IntExpr
              , _xCoord : BigInt, _yCoord : BigInt
              , context : AtomVar ) extends Atom {

  val level = pick(default = UserLevels.userDefaultL)

  val name = mkSensitive(_name, UserLevels.selfL);
  val pwd = mkSensitive(_pwd, UserLevels.selfL);
 
  /* Set initial privacy levels. */
  assume((context~'id === id) <==> (level === UserLevels.selfL))
  assume(CONTAINS(UserLevels.levels, level))

  private def mkSensitive (v : IntExpr, p : LevelTy) =
    if (p > UserLevels.userDefaultL)
      mkSensitiveValue(UserLevels.levels, level, v, p);
    else v

  override def toString = "jcu" + id
  override def hashCode = id.toInt
  override def equals(that: Any) =
    if (that.isInstanceOf[GeoUser])
      (id == that.asInstanceOf[GeoUser].id) 
    else 
      false
}
