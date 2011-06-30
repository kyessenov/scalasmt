package test.cap.jeeves

import cap.jeeves._
import cap.jeeves.socialnet._
import cap.scalasmt._
import org.scalatest.FunSuite
import org.scalatest.Assertions.{expect}
import java.sql

import JeevesLib._
import UserLevel._

class ExampleUserRecord extends FunSuite {
  val ctxt = pickObject();
  private def mkDummyUser (uid : BigInt) : UserRecord =
    new UserRecord( _name = 0, _pwd = 0
                  , id = uid
                  , _email = 0, _network = 0
                  , context = ctxt );

  test ("equals self") {
    val u = mkDummyUser(0);
    expect(true) { u.equals(u)  };
  }

  test ("actual friend list") {
    val u = mkDummyUser(0);
    u.addFriend(1);
    val friends = u.friends;
    expect (1) { friends.length };
    expect (1) { concretize(ctxt, u, friends.head) }
  }

  test ("friend list") {
    val u = mkDummyUser(0);
    u.addFriend(1);
    val friends = u.friends;
    expect (1) { friends.length };
    expect (1) { concretize(ctxt, u, friends.head) }
  }

  test ("isFriends") {
    val u =
      new UserRecord( _name = 0
                    , _pwd = 0
                    , id = 0
                    , _email = 0
                    , _network = 0
                    , friendsp = Friends  // friends, friendsp
                    , context = ctxt );
    u.addFriend(1);
    expect(true) { concretize(ctxt, u, u.isFriends(1)) };
  }

  // NOTE[JY]: This is probably going to fail...
  test ("symbolic isFriends 1") {
    val u = mkDummyUser(0);
    val friend1 = pick (x => x === 1)
    u.addFriend(friend1);
    // This one is kind of funny...
    expect(true) { concretize(ctxt, u, u.isFriends(1)) };
  }

  test ("symbolic isFriends 2") {
    val u = mkDummyUser(0);
    val friend1 = pick (x => x === 1)
    u.addFriend(1)
    expect(true) { concretize(ctxt, u, u.isFriends(friend1)) };
  }

  test ("privacy level: self") {
    val u =
      new UserRecord( _name = Constant(0), namep = Self
                    , _pwd = Constant(0), pwdp = Self
                    , id = 0, usernamep = Self
                    , _email = Constant(0), emailp = Self
                    , _network = Constant(0), networkp = Self
                    , context = ctxt );
    val other = mkDummyUser(1)
    expect (0) { concretize(ctxt, u, u.name) };
    expect (-1) { concretize(ctxt, other, u.name) };
  }

  test ("privacy level: friends") {
    val u0 =
     new UserRecord( _name = Constant(0), namep = Friends
                    , _pwd = Constant(0), pwdp = Self
                    , id = 0, usernamep = Self
                    , _email = Constant(0), emailp = Self
                    , _network = Constant(0), networkp = Self
                    , context = ctxt );
    val u1 = mkDummyUser(6);
    u0.addFriend(u1.username)
    expect (0) { concretize(ctxt, u1, u0.name); }
    expect (-1) { concretize(ctxt, u1, u0.pwd); }
  }
}
