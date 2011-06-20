package cap.jeeves.socialnet

/*
 * User records for jeeves social net case study.
 * @author jeanyang
 */

import cap.scalasmt._
import scala.collection.mutable.Map;
import UserLevels._
import cap.jeeves.JeevesLib._

/* NOTE: We will not be using this with beans for now... */
class UserRecord( _name : IntExpr, val namep : LevelTy
                , _pwd : IntExpr, val pwdp : LevelTy
                , val id : BigInt, val usernamep : LevelTy
                , _email : IntExpr, val emailp : LevelTy
                , _network : IntExpr, val networkp : LevelTy
                , val friendsp : LevelTy
                , val context : AtomVar ) extends Atom {
  val level : IntVar = pick(default = defaultL);

  val name = mkSensitive(_name, namep);
  val pwd = mkSensitive(_pwd, pwdp);
  val username = mkSensitive(id, usernamep);
  val email = mkSensitive(_email, emailp);
  val network = mkSensitive(_network, networkp);
  var friends : List[IntExpr] = Nil
 
  /* Set initial privacy levels. */
  assume((context~'id === id) <==> (level === selfL))
  assume(CONTAINS(levels, level))

  private def mkSensitive (v : IntExpr, p : LevelTy) =
    if (p > UserLevels.defaultL)
      mkSensitiveValue(UserLevels.levels, level, v, p);
    else v

  /* Define getters and setters. */
  def isFriends(u : IntExpr) : Formula = CONTAINS(friends, u) 
  def addFriend (friend : IntExpr) {
    val newfriend = mkSensitive(friend, friendsp);
    friends = newfriend :: friends
    assume((context~'id === friend) ==> (level === friendsL))
  }

  override def toString = "u" + id
  override def hashCode = id.toInt
  override def equals(that: Any) =
    if (that.isInstanceOf[UserRecord])
      (id == that.asInstanceOf[UserRecord].id) 
    else 
      false
}
