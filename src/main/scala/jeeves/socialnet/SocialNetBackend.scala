package cap.jeeves.socialnet

import scala.collection.mutable.HashMap;
import scala.collection.mutable.Set;

import cap.scalasmt._
import cap.jeeves._

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
    user.getFriends().map(_ / 'network)

  def getUsersByNetwork(network : Network) : List[Symbolic] = 
    filter(users, (u: UserRecord) => u.network === network)

  /** Email user's friends user's name. */
  def announceLocation(user: UserRecord) = 
    for (f <- user.getFriends()) {
      val to = concretize(user, f/'email).asInstanceOf[Email];
      val body = concretize(f, user.name).asInstanceOf[Name];
      email(to, body);
    }

  def email(to: Email, body: Name) = {}
  
  // send email to multiple people at the same time
} 
