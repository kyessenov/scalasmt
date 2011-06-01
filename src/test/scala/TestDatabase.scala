package test.cap.scalasmt

import cap.scalasmt._
import org.scalatest.FunSuite
import org.scalatest.Assertions.{expect}
import java.sql

class ExampleDatabase extends FunSuite with Sceeves {
  private val context = pick(_ => true);
  private val uRecord =
    new UserRecord( 0
                  , Constant(0), 0  // name, namep
                  , Constant(0), 0  // pwd, pwdp
                  , Constant(0), 0  // username, usernamep
                  , Constant(0), 0  // email, emailp
                  , Constant(0), 0  // network, networkp
                  , List(), 0  // friends, friendsp
                  , context, List(0))

  test ("putGetUserRecord") {
    val db = new Database();
    db.putEntry(uRecord.getUname(), uRecord);
    expect(true) {
      uRecord.equals(db.getEntry(uRecord.getUname()).asInstanceOf[UserRecord]) };
  }
}
