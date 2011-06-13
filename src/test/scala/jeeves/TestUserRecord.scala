package test.cap.jeeves

import cap.jeeves._
import cap.scalasmt._
import org.scalatest.FunSuite
import org.scalatest.Assertions.{expect}
import java.sql

import JeevesLib._

class ExampleUserRecord extends FunSuite {
  val context = pickAtom();

  test ("equals self") {
    val u =
      new UserRecord( Constant(0), 0  // name, namep
                    , Constant(0), 0  // pwd, pwdp
                    , 0, 0  // username, usernamep
                    , Constant(0), 0  // email, emailp
                    , Constant(0), 0  // network, networkp
                    , 0  // friends, friendsp
                  , context );
    expect(true) { u.equals(u)  };
  }

  test ("actual friend list") {
    val u =
      new UserRecord( Constant(0), 0  // name, namep
                    , Constant(0), 0  // pwd, pwdp
                    , 0, 0  // username, usernamep
                    , Constant(0), 0  // email, emailp
                    , Constant(0), 0  // network, networkp
                    , 0  // friends, friendsp
                  , context );
    u.addFriend(1);
    val friends = u.getFriends();
    expect (1) { friends.length };
    expect (1) { concretize(context, u, friends.head) }
  }

  test ("friend list") {
    val u =
      new UserRecord( Constant(0), 0  // name, namep
                    , Constant(0), 0  // pwd, pwdp
                    , 0, 0  // username, usernamep
                    , Constant(0), 0  // email, emailp
                    , Constant(0), 0  // network, networkp
                    , 0  // friends, friendsp
                  , context );
    u.addFriend(1);
    val friends = u.getFriends();
    expect (1) { friends.length };
    expect (1) { concretize(context, u, friends.head) }
  }

  test ("isFriends") {
    val u =
      new UserRecord( Constant(0), 0  // name, namep
                    , Constant(0), 0  // pwd, pwdp
                    , 0, 0  // username, usernamep
                    , Constant(0), 0  // email, emailp
                    , Constant(0), 0  // network, networkp
                    , UserLevels.friendsL  // friends, friendsp
                    , context );
    u.addFriend(1);
    expect(true) { concretize(context, u, u.isFriends(1)) };
  }

  // NOTE[JY]: This is probably going to fail...
  test ("symbolic isFriends 1") {
    val u =
      new UserRecord( Constant(0), 0  // name, namep
                    , Constant(0), 0  // pwd, pwdp
                    , 0, 0  // username, usernamep
                    , Constant(0), 0  // email, emailp
                    , Constant(0), 0  // network, networkp
                    , 0  // friends, friendsp
                    , context );
    val friend1 = pick (x => x === 1)
    u.addFriend(friend1);
    // This one is kind of funny...
    expect(true) { concretize(context, u, u.isFriends(1)) };
  }

  test ("symbolic isFriends 2") {
    val u =
      new UserRecord( Constant(0), 0  // name, namep
                    , Constant(0), 0  // pwd, pwdp
                    , 0, 0  // username, usernamep
                    , Constant(0), 0  // email, emailp
                    , Constant(0), 0  // network, networkp
                    , 0  // friends, friendsp
                    , context );
    val friend1 = pick (x => x === 1)
    u.addFriend(1)
    expect(true) { concretize(context, u, u.isFriends(friend1)) };
  }

  test ("privacy level: self") {
    val u =
      new UserRecord( Constant(0), 0  // name, namep
                    , Constant(0), 0  // pwd, pwdp
                    , 0, 0  // username, usernamep
                    , Constant(0), 0  // email, emailp
                    , Constant(0), 0  // network, networkp
                    , 0  // friends, friendsp
                    , context );
    expect (UserLevels.selfL) { concretize(context, u, u.level) };
  }

  test ("privacy level: friends") {
    val u0 =
      new UserRecord( Constant(0), 0  // name, namep
                    , Constant(0), 0  // pwd, pwdp
                    , 5, 0  // username, usernamep
                    , Constant(0), 0  // email, emailp
                    , Constant(0), 0  // network, networkp
                    , 0  // friends, friendsp
                    , context );
    val u1 = 
      new UserRecord( Constant(0), 0  // name, namep
                    , Constant(0), 0  // pwd, pwdp
                    , 6, 0  // username, usernamep
                    , Constant(0), 0  // email, emailp
                    , Constant(0), 0  // network, networkp
                    , 0  // friends, friendsp
                    , context );
    u0.addFriend(u1.username)
    u1.addFriend(u0.username)
    expect (UserLevels.friendsL) { concretize(context, u1, u0.level); } 
    expect (UserLevels.friendsL) { concretize(context, u0, u1.level); }
  }

  test ("privacy level: default") {
    val u3 =
      new UserRecord( Constant(0), 0  // name, namep
                    , Constant(0), 0  // pwd, pwdp
                    , 3, 0  // username, usernamep
                    , Constant(0), 0  // email, emailp
                    , Constant(0), 0  // network, networkp
                    , 0  // friends, friendsp
                    , context );
    val u4 =
      new UserRecord( Constant(0), 0  // name, namep
                    , Constant(0), 0  // pwd, pwdp
                    , 4, 0  // username, usernamep
                    , Constant(0), 0  // email, emailp
                    , Constant(0), 0  // network, networkp
                    , 0  // friends, friendsp
                    , context );
    expect (UserLevels.defaultL) { concretize(context, u4, u3.level); }
    expect (UserLevels.defaultL) { concretize(context, u3, u4.level); }
  }

  test ("privacy levels") {
    val u0 =
      new UserRecord( Constant(0), 0  // name, namep
                    , Constant(0), UserLevels.selfL  // pwd, pwdp
                    , 5, 0  // username, usernamep
                    , Constant(0), 0  // email, emailp
                    , Constant(0), 0  // network, networkp
                    , UserLevels.friendsL  // friends, friendsp
                    , context );
    val u1 =
      new UserRecord( Constant(0), 0  // name, namep
                    , Constant(0), 0  // pwd, pwdp
                    , 6, 0  // username, usernamep
                    , Constant(0), 0  // email, emailp
                    , Constant(0), 0  // network, networkp
                    , UserLevels.friendsL  // friends, friendsp
                    , context );
    u0.addFriend(u1.username)
    u1.addFriend(u0.username)

    // self level
    expect (0) { concretize(context, u0, u0.pwd) };
    expect (-1) { concretize(context, u1, u0.pwd) };

    expect (6) { concretize(context, u1, (u0.getFriends()).head) }
    expect (true) { concretize(context, u1, u0.isFriends(u1.username)) }
  }
}
