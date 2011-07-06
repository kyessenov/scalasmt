package cap.jeeves.socialnet

/*
 * User records for jeeves social net case study.
 * @author jeanyang, kuat
 */

import cap.scalasmt._
import collection.immutable.ListSet;
import SocialNetBackend._

case class Name(s: String) extends JeevesRecord
case class Network(s: String) extends JeevesRecord

sealed trait UserLevel 
object Anyone extends UserLevel
object Self extends UserLevel
object Friends extends UserLevel

class UserRecord(
  nameV: Name, nameL: UserLevel, 
  networkV: Network, networkL: UserLevel, 
  friendsL: UserLevel) 
extends JeevesRecord {
  private var friends : ListSet[UserRecord] = ListSet()

  /** State mutation */
  def add(u: UserRecord) {friends = friends + u}
  def remove(u: UserRecord) {friends = friends - u}

  /** Observers */
  val name = mkSensitiveObject(level(nameL), nameV)
  val network = mkSensitiveObject(level(networkL), networkV);
  def getFriends() = {
    val l = level(friendsL);
    friends.map(mkSensitiveObject(l, _)).toList
  }
  def isFriends(u: UserRecord) = CONTAINS(getFriends, u)

  /** Helpers */
  private def level(l: UserLevel): LevelVar = {
    val level = mkLevel();
    l match {
      case Anyone => 
        policy(level, true)
      case Self => 
        policy(level, CONTEXT === this)
      case Friends => 
        policy(level, CONTEXT === this)
        policy(level, () => CONTAINS(friends, CONTEXT));
    }
    level
  }
}
