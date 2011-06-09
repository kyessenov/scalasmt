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
class UserRecord(val realuname : BigInt
                , _name : IntExpr, val namep : LevelTy
                , _pwd : IntExpr, val pwdp : BigInt
                , _username : IntExpr, val usernamep : BigInt
                , _email : IntExpr, val emailp : BigInt
                , _network : IntExpr, val networkp : BigInt
                , _friends : List[IntExpr], val friendsp : BigInt
                , val context : AtomVar ) extends Atom {
  val level : IntVar = pick(defaultL);

  /* Invariant: The variables are always symbolic expressions kept up to date with the permission. */
  val name = mkSensitive(_name, namep);
  val pwd = mkSensitive(_pwd, pwdp);
  val username = mkSensitive(_username, usernamep);
  val email = mkSensitive(_email, emailp);
  val network = mkSensitive(_network, networkp);

  // TODO: No longer permit default friends
  private var friends = _friends.map(mkSensitive(_, friendsp));
  private var actualFriends : List[BigInt]= Nil
 
  /* Set privacy levels. */
  assume((context~'realuname === realuname) <==> (level === selfL))
  assume(level === selfL || level === friendsL || level === defaultL)

  private def mkSensitive (v : IntExpr, p : BigInt) =
    mkSensitiveValue(UserLevels.levels, level, v, p);

  /* Define getters and setters. */
  def getFriends () : List[IntExpr] = friends
  def getActualFriends() : List[BigInt] = actualFriends
  def isFriends(u : IntExpr) : Formula = { 
    val isFriends = pickBool;
    friends.foreach {
      case friend =>
      assume (u === friend ==> isFriends === true);
    }
    isFriends
  }

  def addFriend (user : IntExpr, actualUser : BigInt) {
    val newfriend = mkSensitive(user, friendsp);
    friends = newfriend :: friends
    assume((context~'realuname === user) ==> (level === friendsL))
    actualFriends = actualUser :: actualFriends
  }

  override def hashCode = realuname.toInt
  override def equals(that: Any) =
    if (that.isInstanceOf[UserRecord])
      (realuname == that.asInstanceOf[UserRecord].realuname) 
    else 
      false
}
