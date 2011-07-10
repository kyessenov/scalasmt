package cap.jeeves.socialnet

/*
 * User records for jeeves social net case study.
 * @author jeanyang, kuat
 */

import cap.scalasmt._
import collection.immutable.ListSet;
import SocialNetBackend._

import Expr._

case class Name(s: String) extends JeevesRecord
case class Email(s: String) extends JeevesRecord
case class Network(s: String) extends JeevesRecord

sealed trait UserLevel 
object Anyone extends UserLevel
object Self extends UserLevel
object Friends extends UserLevel

class UserRecord(
  nameV: Name, 
  nameL: UserLevel,
  emailV: Email,
  emailL: UserLevel, 
  networkV: Network, 
  networkL: UserLevel, 
  friendsL: UserLevel) 
extends JeevesRecord {
  private var friends: Set[UserRecord] = Set()
  private var X: BigInt = 0;
  private var Y: BigInt = 0;

  /** Mutators */
  def add(u: UserRecord) {friends = friends + u}
  def remove(u: UserRecord) {friends = friends - u}
  def setLocation(x: BigInt, y: BigInt) {this.X = x; this.Y = y;}

  /** Observers */
  val name = mkSensitive[Name](level(nameL), nameV)
  val email = mkSensitive[Email](level(emailL), emailV)
  val network = mkSensitive[Network](level(networkL), networkV);
  def getFriends() = {
    val l = level(friendsL);
    friends.map(mkSensitive[UserRecord](l, _))
  }
  def isFriends(u: UserRecord) = getFriends.has(u)
  def location() = {
    val l = mkLevel();
    policy(l, DISTANCE(CONTEXT, this) >= 10, LOW)
    (mkSensitiveInt(l, X), mkSensitiveInt(l, Y))
  }

  /** Helpers */
  private def level(ul: UserLevel) = {
    val l = mkLevel();
    val me = CONTEXT === this;
    ul match {
      case Anyone => 
      case Self => policy(l, ! me, LOW)
      case Friends => 
        policy(l, ! (me || friends.has(CONTEXT)),  LOW);
    }
    l
  }

  private def DISTANCE(a: Symbolic, b: Symbolic) = 
    ABS(a.X - b.X) + ABS(a.Y - b.Y) 
}
