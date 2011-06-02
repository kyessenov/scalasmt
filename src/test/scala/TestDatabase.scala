package test.cap.scalasmt

import cap.scalasmt._
import cap.scalasmt.RelExpr._
import org.scalatest.FunSuite
import org.scalatest.Assertions.{expect}
import java.sql

import JeevesLib._

class ExampleDatabase extends FunSuite {
  private val context = pickAtom;
  private val uRecord =
    new UserRecord( 0
                  , Constant(0), 0  // name, namep
                  , Constant(0), 0  // pwd, pwdp
                  , Constant(0), 0  // username, usernamep
                  , Constant(0), 0  // email, emailp
                  , Constant(0), 0  // network, networkp
                  , List(), 0  // friends, friendsp
                  , context )

  private def mkTestDB() : Database = {
    val db = new Database();
    db.putEntry(uRecord.getUname(), uRecord);
    db
  }

  test ("put and get") {
    val db = mkTestDB();
    val entry = db.getEntry(uRecord.getUname());
    expect(true) { uRecord.equals(concretize(entry)) };
  }

  test ("get record symbolic key") {
    val db = mkTestDB();
    val idx = pick(x => x === uRecord.getUname());
    val entry = db.getEntry(idx);
    expect(uRecord) { concretize(entry) }
  }

  test ("symbolic record field") {
    val db = mkTestDB();
    val idx = pick(x => x === uRecord.getUname());
    val entry = db.getEntry(idx);
    val x = pick(x => (entry ~ '__pwd === 0) ==> (x === 42));
    expect(42) { concretize(context, uRecord, x) }
  }

  test ("symbolic record getter") {
    val db = mkTestDB();
    val idx = pick(x => x === uRecord.getUname());
    val entry = db.getEntry(idx);
    val x = pick(x => (entry ~ 'getPwd === 0) ==> (x === 42));
    expect(42) { concretize(context, uRecord, x) }
  }
}
