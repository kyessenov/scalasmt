package cap.jeeves.jconf

/*
 * User records for jconf case study.
 * @author jeanyang
 */

import cap.scalasmt._
import scala.collection.mutable.Map;
import UserLevels._
import cap.jeeves.JeevesLib._

object UserStatus {
  val author : BigInt = 0
  val reviewer : BigInt = 1
  val pc : BigInt = 2
}

import UserStatus._

class ConfUser( _name : IntExpr, val namep : LevelTy
                , _pwd : IntExpr, val pwdp : LevelTy
                , val id : BigInt, val usernamep : LevelTy
                , _email : IntExpr, val emailp : LevelTy
                , val status : BigInt
                /* Pass in prviacy level and context variables. */
                , val context : AtomVar ) extends Atom {

  val level = pick(default = userDefaultL)

  val name = mkSensitive(_name, namep);
  val pwd = mkSensitive(_pwd, pwdp);
  val username = mkSensitive(id, usernamep);
  val email = mkSensitive(_email, emailp);
 
  /* Set initial privacy levels. */
  assume((context~'id === id) <==> (level === selfL))
  assume(CONTAINS(levels, level))

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
