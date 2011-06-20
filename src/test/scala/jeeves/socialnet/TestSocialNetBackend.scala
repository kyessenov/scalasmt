package test.cap.jeeves

import cap.scalasmt._
import cap.jeeves._
import cap.jeeves.socialnet._
import JeevesLib._
import SocialNetBackend._
import org.scalatest.FunSuite
import org.scalatest.Assertions.{expect}
import scala.collection.immutable.Map

class ExampleSocialNetBackend extends FunSuite {
  val user0 =
    addUser( "Jean Yang",         UserLevels.friendsL
           , "xyz",               UserLevels.selfL
           , "jeanyang",          UserLevels.friendsL
           , "jeanyang@mit.edu",  UserLevels.friendsL
           , "MIT",               UserLevels.friendsL
           , UserLevels.friendsL );
  val uid0 = user0.id
  val user1 =
    addUser ( "Kuat Yessenov",    UserLevels.friendsL
            , "abc",              UserLevels.selfL
            , "kuaty",            UserLevels.friendsL
            , "kuat@mit.edu",     UserLevels.friendsL
            , "MIT",              UserLevels.friendsL
            , UserLevels.friendsL );
  val uid1 = user1.id
  val user2 =
    addUser ( "Rishabh Singh",    UserLevels.friendsL
            , "abc",              UserLevels.selfL
            , "rishabh",          UserLevels.friendsL
            , "rishabh@mit.edu",  UserLevels.friendsL
            , "MIT",              UserLevels.friendsL
            , UserLevels.friendsL );
  val uid2 = user2.id

  addFriend(uid0, uid1)

  test ("getUser using default user") {
    val otherUser0 = getUser(uid0);
    expect (true) { user0.equals(otherUser0) };
  }

  test ("isFriends") {
    expect (true) { getBool(uid0, isFriends(uid0, uid1)) };
    expect (true) { getBool(uid1, isFriends(uid0, uid1)) };
  }

  test ("friends") {
    val friends = getFriends(uid0);
    val friendStrs = printStringList(uid1, friends);
    expect (true) { friendStrs.contains("kuaty") };
  }

  test ("view sensitive data non-friend") {
    expect (false) { concretize(snbContext, user0, isFriends(uid0, uid2)) };
    expect (-1) { concretize(snbContext, user0, user2.network) };
    expect (fromString("MIT")) {
      concretize(snbContext, user1, user1.network)
    };
  }

  test ("getFriendNetworks") {
    val networks = getFriendNetworks(uid0);
    val networkStrs = printStringList(uid1, networks);
    expect (true) { networkStrs.contains("MIT") } ;
  }

  test("getUsersByNetworks view by friend") {
    val friends = getUsersByNetwork("MIT");
    val concreteFriends = getConcreteRecordList(uid0, friends);
    concreteFriends.foreach(x => println(x.id));
    expect (2) { concreteFriends.length };
    expect (true)
      { concreteFriends.exists(x => x.id == fromString("jeanyang")) };
    expect (true)
      { concreteFriends.exists(x => x.id == fromString("kuaty")) };
  }
}
