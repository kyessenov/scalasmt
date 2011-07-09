package cap.jeeves.socialnet

/*
 * User records for jeeves social net case study.
 * @author jeanyang, kuat
 */

import cap.scalasmt._
import collection.immutable.ListSet;
import SocialNetBackend._

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
  private var friends : ListSet[UserRecord] = ListSet()
  private var X: BigInt = 0;
  private var Y: BigInt = 0;

  /** Mutators */
  def add(u: UserRecord) {friends = friends + u}
  def remove(u: UserRecord) {friends = friends - u}
  def setLocation(x: BigInt, y: BigInt) {this.X = x; this.Y = y;}

  /** Observers */
  val name = mkSensitiveObject(level(nameL), nameV)
  val email = mkSensitiveObject(level(emailL), emailV)
  val network = mkSensitiveObject(level(networkL), networkV);
  def getFriends() = {
    val l = level(friendsL);
    friends.map(mkSensitiveObject(l, _)).toList
  }
  def isFriends(u: UserRecord) = CONTAINS(getFriends, u)
  def location() = {
    val l = mkLevel();
    policy(l, () => DISTANCE(CONTEXT, this) >= 10, LOW)
    (mkSensitiveInt(l, Y), mkSensitiveInt(l, X))
  }

  /** Helpers */
  private def level(l: UserLevel): LevelVar = {
    val level = mkLevel();
    l match {
      case Anyone => 
      case Self => 
        policy(level, ! (CONTEXT === this), LOW)
      case Friends => 
        policy(level, () => ! CONTAINS(friends, CONTEXT) && ! (CONTEXT === this),  LOW);
    }
    level
  }

  private def DISTANCE(a: Symbolic, b: Symbolic) = 
    ABS(a.X - b.X) + ABS(a.Y - b.Y) 
}
