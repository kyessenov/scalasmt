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
                  , 0, 0  // name, namep
                  , 0, 0  // pwd, pwdp
                  , 0, 0  // username, usernamep
                  , 0, 0  // email, emailp
                  , 0, 0  // network, networkp
                  , List(), 0  // friends, friendsp
                  , context );
    expect(true) { u.equals(u)  };
  }

  test ("isFriends") {
    val u =
      new UserRecord(0
                  , 0, 0  // name, namep
                  , 0, 0  // pwd, pwdp
                  , 0, 0  // username, usernamep
                  , 0, 0  // email, emailp
                  , 0, 0  // network, networkp
                  , List(), UserLevels.friendsL  // friends, friendsp
                  , context );
    u.addFriend(1);
    expect(true) { concretize(context, u, u.isFriends(1)) };
  }

  // NOTE[JY]: This is probably going to fail...
  test ("symbolic isFriends 1") {
    val u =
      new UserRecord(0
                  , 0, 0  // name, namep
                  , 0, 0  // pwd, pwdp
                  , 0, 0  // username, usernamep
                  , 0, 0  // email, emailp
                  , 0, 0  // network, networkp
                  , List(), 0  // friends, friendsp
                  , context );
    val friend1 = pick (x => x === 1)
    u.addFriend(friend1);
    // This one is kind of funny...
    expect(true) { u.isFriends(1) };
  }

  test ("symbolic isFriends 2") {
    val u =
      new UserRecord(0
                  , 0, 0  // name, namep
                  , 0, 0  // pwd, pwdp
                  , 0, 0  // username, usernamep
                  , 0, 0  // email, emailp
                  , 0, 0  // network, networkp
                  , List(), 0  // friends, friendsp
                  , context );
    val friend1 = pick (x => x === 1)
    u.addFriend(1);
    // TODO: Put a context.
    expect(true) { concretize(u.isFriends(friend1)) };
  }

  test ("privacy levels") {
    val u1 =
      new UserRecord(0
                  , 0, 0  // name, namep
                  , 0, 0  // pwd, pwdp
                  , 0, 0  // username, usernamep
                  , 0, 0  // email, emailp
                  , 0, 0  // network, networkp
                  , List(), 0  // friends, friendsp
                  , context );
    val u2 = 
      new UserRecord(0
                  , 0, 0  // name, namep
                  , 0, 0  // pwd, pwdp
                  , 0, 0  // username, usernamep
                  , 0, 0  // email, emailp
                  , 0, 0  // network, networkp
                  , List(), 0  // friends, friendsp
                  , context );

  }

  test ("equals symbolic self") {

  }


}
