package cap.jeeves

/*
 * User records for jeeves social net case study.
 * @author jeanyang
 */

import cap.scalasmt._
import scala.collection.mutable.Map;
import UserLevels._
import JeevesLib._

/* NOTE: We will not be using this with beans for now... */
class UserRecord(val id : BigInt
                , _name : IntExpr, val namep : LevelTy
                , _pwd : IntExpr, val pwdp : LevelTy
                , _username : IntExpr, val usernamep : LevelTy
                , _email : IntExpr, val emailp : LevelTy
                , _network : IntExpr, val networkp : LevelTy
                , _friends : List[IntExpr], val friendsp : LevelTy
                , val context : AtomVar ) extends Atom {
  val level : IntVar = pick(default = defaultL);

  /* Invariant: The variables are always symbolic expressions kept up to date with the permission. */
  val name = mkSensitive(_name, namep);
  val pwd = mkSensitive(_pwd, pwdp);
  val username = mkSensitive(_username, usernamep);
  val email = mkSensitive(_email, emailp);
  val network = mkSensitive(_network, networkp);

  // TODO: No longer permit default friends
  var friends = _friends.map(mkSensitive(_, friendsp));
 
  /* Set privacy levels. */
  assume((context~'id === id) <==> (level === selfL))
  assume(CONTAINS(levels, level))

  private def mkSensitive (v : IntExpr, p : LevelTy) =
    mkSensitiveValue(UserLevels.levels, level, v, p);

  /* Define getters and setters. */
  def getFriends () : List[IntExpr] = friends
  def isFriends(u : IntExpr) : Formula = CONTAINS(friends, u) 
  def addFriend (friend : IntExpr) {
    val newfriend = mkSensitive(friend, friendsp);
    friends = newfriend :: friends
    assume((context~'id === friend) ==> (level === friendsL))
  }

  override def hashCode = id.toInt
  override def equals(that: Any) =
    if (that.isInstanceOf[UserRecord])
      (id == that.asInstanceOf[UserRecord].id) 
    else 
      false
}
