package cap.jeeves.socialnet

/*
 * User records for jeeves social net case study.
 * @author jeanyang
 */

import cap.scalasmt._
import scala.collection.mutable.Map;
import cap.jeeves.JeevesLib._

object UserLevel extends Enumeration {
  type UserLevel = Value
  val Self, Friends, Anyone = Value
}

import UserLevel._

/* NOTE: We will not be using this with beans for now... */
class UserRecord( _name : IntExpr, val namep : UserLevel = Anyone
                , _pwd : IntExpr, val pwdp : UserLevel = Anyone
                , val id : BigInt, val usernamep : UserLevel = Anyone
                , _email : IntExpr, val emailp : UserLevel = Anyone
                , _network : IntExpr, val networkp : UserLevel = Anyone
                , val friendsp : UserLevel = Anyone
                , val context : ObjectVar ) extends Atom {
  val isFriends : Formula = pickBool(default = false);

  val name = {
    val level : IntVar = pick(default = Viewer.low);
    mkSensitive(level, _name, namep);
  }
  val pwd = {
    val level : IntVar = pick(default = Viewer.low);
    mkSensitive(level, _pwd, pwdp);
  }
  val username = {
    val level : IntVar = pick(default = Viewer.low);
    mkSensitive(level, id, usernamep);
  }
  val email = {
    val level : IntVar = pick(default = Viewer.low);
    mkSensitive(level, _email, emailp);
  }
  val network = {
    val level : IntVar = pick(default = Viewer.low);
    mkSensitive(level, _network, networkp);
  }
  var friends : List[IntExpr] = Nil
 
  private def mkSensitive (level : IntVar, v : IntExpr, p : UserLevel)
  : IntExpr = {
    assume((context~'id === id) ==> (level === Viewer.high))
    if (p == Anyone) { v
    } else {
      val sv = mkSensitiveValue(level, v);
      p match {
        case Self => ()
//          assume((context~'id === id) ==> (level === Viewer.high))
        case Friends =>
          assume(isFriends ==> (level === Viewer.high))
      }
      sv
    }
  }

  /* Define getters and setters. */
  def isFriends(u : IntExpr) : Formula = CONTAINS(friends, u) 
  def addFriend (friend : IntExpr) {
    val level = pick(default = Viewer.low);
    val newfriend = mkSensitive(level, friend, friendsp);
    friends = newfriend :: friends

    assume((context~'id === friend) ==> isFriends)
  }

  override def toString = "u" + id
  override def hashCode = id.toInt
  override def equals(that: Any) =
    if (that.isInstanceOf[UserRecord])
      (id == that.asInstanceOf[UserRecord].id) 
    else 
      false
}
