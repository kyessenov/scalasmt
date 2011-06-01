package test.cap.scalasmt

import cap.scalasmt._
import org.scalatest.FunSuite
import org.scalatest.Assertions.{expect}
import java.sql

class ExampleUserRecord extends FunSuite with Sceeves {
  test ("equalsSelf") {
    val context = pick (_ => true);
    val u =
      new UserRecord(0
                  , Constant(0), 0  // name, namep
                  , Constant(0), 0  // pwd, pwdp
                  , Constant(0), 0  // username, usernamep
                  , Constant(0), 0  // email, emailp
                  , Constant(0), 0  // network, networkp
                  , List(), 0  // friends, friendsp
                  , context, List(0));
    expect(true) { u.equals(u)  };
  }
  // TODO: More substantive tests.
}
