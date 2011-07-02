package cap.jeeves.socialnet

import scala.collection.mutable.HashMap;
import scala.collection.mutable.Set;

import cap.scalasmt._
import cap.jeeves._
import JeevesLib._

object SocialNetBackend {
  private var users: List[UserRecord] = Nil;

  /* Database functions. */
  def addUser(u: UserRecord) {
    users = u :: users
  }
  
  def addFriend(record1: UserRecord, record2: UserRecord) {
    record1.add(record2);
    record2.add(record1);
  }

  def removeFriend(record1: UserRecord, record2: UserRecord) {
    record1.remove(record2);
    record2.remove(record1);
  }

  /* This function demonstrates how we can work with symbolic objects and do
   * additional operations on them without worrying about permissions. */
  def getFriendNetworks(user: UserRecord) =
    user.getFriends().map(_ / 'network)

  def getUsersByNetwork (network : Network) : List[Symbolic] = 
    filter(users, (u: UserRecord) => u.network === network)

  /* What if we wanted to have a series of operations on the friends of
   * symbolic friends? */
  /*
  def getFriendsOfFriends (user : BigInt) : List[IntExpr] = {
    // This is still concrete.
    val friends = getFriends(user);
    val networks = friends.foldLeft (Set.empty[IntExpr]) 
    friends
  }
  */
}
