package test.cap.scalasmt

import cap.scalasmt._
import cap.scalasmt.RelExpr._
import org.scalatest.FunSuite
import org.scalatest.Assertions.{expect}
import java.sql

import JeevesLib._

class ExampleDatabase extends FunSuite {
  private val context = pick(_ => true);
  private val uRecord =
    new UserRecord( 0
                  , 0, 0  // name, namep
                  , 0, 0  // pwd, pwdp
                  , 0, 0  // username, usernamep
                  , 0, 0  // email, emailp
                  , 0, 0  // network, networkp
                  , List(), 0  // friends, friendsp
                  , context, List(0))

  private def mkTestDB() : Database = {
    val db = new Database();
    db.putEntry(uRecord.getUname(), uRecord);
    db
  }

  test ("put and get") {
    val db = mkTestDB();
    val entry = concretize(db.getEntry(uRecord.getUname()));
    expect(true) { uRecord.equals(entry) };
  }

  test ("get record symbolic key") {
    val db = mkTestDB();
    val idx = pick(x => x === uRecord.getUname());
    val entry = db.getEntry(idx);
    expect(uRecord) { concretize(entry) }
  }
}
