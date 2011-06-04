package test.cap.jeeves

import cap.scalasmt._
import cap.scalasmt.RelExpr._
import org.scalatest.FunSuite
import org.scalatest.Assertions.{expect}
import java.sql

import cap.jeeves._
import JeevesLib._

class ExampleDatabase extends FunSuite {
  private val context = pickAtom;
  private val uRecord =
    new UserRecord( 0
                  , 0, 0  // name, namep
                  , 1337, 0  // pwd, pwdp
                  , 0, 0  // username, usernamep
                  , 0, 0  // email, emailp
                  , 0, 0  // network, networkp
                  , Nil, 0  // friends, friendsp
                  , context )

  private def mkTestDB() : Database = {
    val db = new Database();
    db.putEntry(uRecord.getUname(), uRecord);
    db
  }

  test ("put and get") {
    val db = mkTestDB();
    val entry = db.getEntry(uRecord.getUname());
    expect(true) { uRecord.equals(concretize(context, uRecord, entry)) };
  }

  test ("get record symbolic key") {
    val db = mkTestDB();
    val idx = pick(x => x === uRecord.getUname());
    val entry = db.getEntry(idx);
    expect(uRecord) { concretize(context, uRecord, entry) }
  }

  test ("symbolic record field") {
    val db = mkTestDB();
    val idx = pick(_ === uRecord.getUname());
    val entry = db.getEntry(idx);
    val x = pick(x => (entry ~ '__pwd === 1337) ==> (x === 42));
    SMT.PRINT_INPUT = true;
    try expect(42) { concretize(context, uRecord, x) }
    finally SMT.PRINT_INPUT = false;
  }

  /*
  test ("symbolic record getter") {
    val db = mkTestDB();
    val idx = pick(x => x === uRecord.getUname());
    val entry = db.getEntry(idx);
    val x = pick(x => (entry ~ 'getPwd === 0) ==> (x === 42));
    expect(42) { concretize(context, uRecord, x) }
  }
  */
}
