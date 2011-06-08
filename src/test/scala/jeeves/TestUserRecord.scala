package test.cap.jeeves

import cap.jeeves._
import cap.scalasmt._
import org.scalatest.FunSuite
import org.scalatest.Assertions.{expect}
import java.sql

import JeevesLib._

class ExampleUserRecord extends FunSuite {
  val context = pickAtom;

  test ("equals self") {
    val u =
      new UserRecord(0
                  , Constant(0), 0  // name, namep
                  , Constant(0), 0  // pwd, pwdp
                  , Constant(0), 0  // username, usernamep
                  , Constant(0), 0  // email, emailp
                  , Constant(0), 0  // network, networkp
                  , List(), 0  // friends, friendsp
                  , context );
    expect(true) { u.equals(u)  };
  }

  test ("actual friend list") {
    val u =
      new UserRecord(0
                  , Constant(0), 0  // name, namep
                  , Constant(0), 0  // pwd, pwdp
                  , Constant(0), 0  // username, usernamep
                  , Constant(0), 0  // email, emailp
                  , Constant(0), 0  // network, networkp
                  , List(), 0  // friends, friendsp
                  , context );
    u.addFriend(Constant(1), 1 : BigInt);
    val friends = u.getActualFriends();
    expect (1) { friends.length };
    expect (1) { friends.head }
  }

  test ("friend list") {
    val u =
      new UserRecord(0
                  , Constant(0), 0  // name, namep
                  , Constant(0), 0  // pwd, pwdp
                  , Constant(0), 0  // username, usernamep
                  , Constant(0), 0  // email, emailp
                  , Constant(0), 0  // network, networkp
                  , List(), 0  // friends, friendsp
                  , context );
    u.addFriend(Constant(1), 1 : BigInt);
    val friends = u.getFriends();
    expect (1) { friends.length };
    expect (1) { concretize(context, u, friends.head) }
  }

  test ("isFriends") {
    val u =
      new UserRecord(0
                  , Constant(0), 0  // name, namep
                  , Constant(0), 0  // pwd, pwdp
                  , Constant(0), 0  // username, usernamep
                  , Constant(0), 0  // email, emailp
                  , Constant(0), 0  // network, networkp
                  , List(), UserLevels.friendsL  // friends, friendsp
                  , context );
    u.addFriend(Constant(1), 1: BigInt);
    expect(true) { concretize(context, u, u.isFriends(1)) };
  }

  // NOTE[JY]: This is probably going to fail...
  test ("symbolic isFriends 1") {
    val u =
      new UserRecord(0
                  , Constant(0), 0  // name, namep
                  , Constant(0), 0  // pwd, pwdp
                  , Constant(0), 0  // username, usernamep
                  , Constant(0), 0  // email, emailp
                  , Constant(0), 0  // network, networkp
                  , List(), 0  // friends, friendsp
                  , context );
    val friend1 = pick (x => x === 1)
    u.addFriend(friend1, 1);
    // This one is kind of funny...
    expect(true) { concretize(context, u, u.isFriends(1)) };
  }

  test ("symbolic isFriends 2") {
    val u =
      new UserRecord(0
                  , Constant(0), 0  // name, namep
                  , Constant(0), 0  // pwd, pwdp
                  , Constant(0), 0  // username, usernamep
                  , Constant(0), 0  // email, emailp
                  , Constant(0), 0  // network, networkp
                  , List(), 0  // friends, friendsp
                  , context );
    val friend1 = pick (x => x === 1)
    u.addFriend(1, 1 : BigInt);
    expect(true) { concretize(context, u, u.isFriends(friend1)) };
  }

  test ("privacy level: self") {
    val u =
      new UserRecord(0
                  , Constant(0), 0  // name, namep
                  , Constant(0), 0  // pwd, pwdp
                  , Constant(0), 0  // username, usernamep
                  , Constant(0), 0  // email, emailp
                  , Constant(0), 0  // network, networkp
                  , List(), 0  // friends, friendsp
                  , context );
    expect (UserLevels.selfL) { concretize(context, u, u.level) };
  }

  test ("privacy level: friends") {
    val u0 =
      new UserRecord(5
                  , Constant(0), 0  // name, namep
                  , Constant(0), 0  // pwd, pwdp
                  , Constant(5), 0  // username, usernamep
                  , Constant(0), 0  // email, emailp
                  , Constant(0), 0  // network, networkp
                  , List(), 0  // friends, friendsp
                  , context );
    val u1 = 
      new UserRecord(6
                  , Constant(0), 0  // name, namep
                  , Constant(0), 0  // pwd, pwdp
                  , Constant(6), 0  // username, usernamep
                  , Constant(0), 0  // email, emailp
                  , Constant(0), 0  // network, networkp
                  , List(), 0  // friends, friendsp
                  , context );
    u0.addFriend(u1.getUsername(), 6 : BigInt);
    u1.addFriend(u0.getUsername(), 5 : BigInt);
    expect (true) { concretize(context, u0, u0.isActualFriends(6)) };
    expect (true) {
      concretize(context, u1, u0.isActualFriends(context ~ '__username))
    };
    expect (true) { concretize(context, u1, u1.isActualFriends(5)) };
    expect (true) {
      concretize(context, u0, u1.isActualFriends(context ~ '__username))
    };
    expect (UserLevels.friendsL) { concretize(context, u1, u0.level); }
    expect (UserLevels.friendsL) { concretize(context, u0, u1.level); }
  }

  test ("privacy level: default") {
    val u3 =
      new UserRecord(3
                  , Constant(0), 0  // name, namep
                  , Constant(0), 0  // pwd, pwdp
                  , Constant(3), 0  // username, usernamep
                  , Constant(0), 0  // email, emailp
                  , Constant(0), 0  // network, networkp
                  , List(), 0  // friends, friendsp
                  , context );
    val u4 =
      new UserRecord(4
                  , Constant(0), 0  // name, namep
                  , Constant(0), 0  // pwd, pwdp
                  , Constant(4), 0  // username, usernamep
                  , Constant(0), 0  // email, emailp
                  , Constant(0), 0  // network, networkp
                  , List(), 0  // friends, friendsp
                  , context );
    expect (false) { concretize(context, u3, u3.isActualFriends(4)) };
    expect (false) { concretize(context, u4, u4.isActualFriends(3)) };
    expect (UserLevels.defaultL) { concretize(context, u4, u3.level); }
    expect (UserLevels.defaultL) { concretize(context, u3, u4.level); }
  }

  /*
  test ("equals symbolic self") {
    val u =
      new UserRecord(0
                  , Constant(0), 0  // name, namep
                  , Constant(0), 0  // pwd, pwdp
                  , Constant(0), 0  // username, usernamep
                  , Constant(0), 0  // email, emailp
                  , Constant(0), 0  // network, networkp
                  , List(), UserLevels.friendsL  // friends, friendsp
                  , context );
    val v = pickAtom;
    assume(v === u);  // TODO: Figure out what this === does.
    expect (true) { concretize(context, u, v).equals(v) };
  }
  */
}
