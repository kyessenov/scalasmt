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

object UserLevels {
  val userDefaultL : LevelTy = 0;
  val reviewerL    : LevelTy = 1;
  val pcL          : LevelTy = 2;
  val selfL        : LevelTy = 3;
  val levels = List(userDefaultL, reviewerL, pcL, selfL);
}

/* Conference User */
class ConfUser( val id : BigInt
              , _name : IntExpr, _pwd : IntExpr, _email : IntExpr
              , val status : BigInt, val context : AtomVar ) extends Atom {

  val level = pick(default = UserLevels.userDefaultL)

  val name = mkSensitive(_name, UserLevels.reviewerL);
  val pwd = mkSensitive(_pwd, UserLevels.selfL);
  val email = mkSensitive(_email, UserLevels.pcL);
 
  /* Set initial privacy levels. */
  assume((context~'id === id) ==> (level === UserLevels.selfL))
  assume(((!(context~'id === id)) &&
          (context~'status === UserStatus.reviewerL)) ==>
          (level === UserLevels.reviewerL))
  assume(((!(context~'id === id)) && (context~'status === UserStatus.pcL)) ==>
          (level === UserLevels.pcL))
  assume(CONTAINS(UserLevels.levels, level))

  private def mkSensitive (v : IntExpr, p : LevelTy) =
    if (p > UserLevels.userDefaultL)
      mkSensitiveValue(UserLevels.levels, level, v, p);
    else v

  override def toString = "jcu" + id
  override def hashCode = id.toInt
  override def equals(that: Any) =
    if (that.isInstanceOf[ConfUser])
      (id == that.asInstanceOf[ConfUser].id) 
    else 
      false
}
