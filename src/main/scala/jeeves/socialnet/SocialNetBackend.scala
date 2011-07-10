package cap.jeeves.socialnet

import scala.collection.mutable.HashMap;
import scala.collection.mutable.Set;

import cap.scalasmt._
import cap.jeeves._

/**
 * External interface to social network.
 * @author kuat
 */
object SocialNetBackend extends JeevesLib {
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
    user.getFriends().map(_.network)

  def getUsersByNetwork(network : Network) : List[Symbolic] = 
    filter(users, (u: UserRecord) => u.network === network)

  def invite(a: Symbolic, b: Symbolic) = {
    concretize(a, b.email)
  }
  
  // send email to multiple people at the same time
} 
