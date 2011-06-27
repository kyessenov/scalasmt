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
    new UserRecord( _name = Constant(0), _pwd = Constant(0)
                  , id = uid
                  , _email = Constant(0), _network = Constant(0)
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
      new UserRecord( _name = Constant(0)
                    , _pwd = Constant(0)
                    , id = 0
                    , _email = Constant(0)
                    , _network = Constant(0)
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
    val u = mkDummyUser(0);
//    expect (Viewer.high) { concretize(ctxt, u, u.level) };
  }

  test ("privacy level: friends") {
    val u0 = mkDummyUser(5);
    val u1 = mkDummyUser(6);
    u0.addFriend(u1.username)
    u1.addFriend(u0.username)
    expect (true) { concretize(ctxt, u1, u0.isFriends); }
    // expect (Viewer.high) { concretize(ctxt, u1, u0.level); }
    expect (true) { concretize(ctxt, u0, u1.isFriends); }
    // expect (Viewer.high) { concretize(ctxt, u0, u1.level); }
  }

  test ("privacy level: default") {
    val u3 = mkDummyUser(3);
    val u4 = mkDummyUser(4);
    // expect (Viewer.low) { concretize(ctxt, u4, u3.level); }
    // expect (Viewer.low) { concretize(ctxt, u3, u4.level); }
  }

  test ("privacy levels") {
    val u0 = mkDummyUser(5);
    val u1 = mkDummyUser(6);
    u0.addFriend(u1.username)
    u1.addFriend(u0.username)

    // self level
    expect (0) { concretize(ctxt, u0, u0.pwd) };
//    expect (-1) { concretize(ctxt, u1, u0.pwd) };

    expect (6) { concretize(ctxt, u1, (u0.friends).head) }
    expect (true) { concretize(ctxt, u1, u0.isFriends(u1.username)) }
  }
}
