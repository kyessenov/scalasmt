package test.cap.jeeves

import cap.jeeves.socialnet.UserRecord._
import cap.scalasmt._
import cap.scalasmt.RelExpr._
import org.scalatest.FunSuite
import org.scalatest.Assertions.{expect}
import java.sql

import cap.jeeves._
import JeevesLib._

class ExampleDatabase extends FunSuite {
  private val context = pickAtom();
  private val uRecord =
    new UserRecord( 0, 0  // name, namep
                  , 1337, 0  // pwd, pwdp
                  , 0, 0  // username, usernamep
                  , 0, 0  // email, emailp
                  , 0, 0  // network, networkp
                  , 0 // friendsp
                  , context )

  private def mkTestDB() : Database[UserRecord] = {
    val db = new Database[UserRecord]();
    db.putEntry(uRecord.id, uRecord);
    db
  }

  test ("put and get") {
    val db = mkTestDB();
    val entry = db.getEntry(uRecord.id);
    expect(true) { uRecord.equals(concretize(context, uRecord, entry)) };
  }

  test ("get record symbolic key") {
    val db = mkTestDB();
    val idx = pick(x => x === uRecord.id);
    val entry = db.getEntry(idx);
    expect(uRecord) { concretize(context, uRecord, entry) }
  }

  test ("symbolic record field") {
    val db = mkTestDB();
    val idx = pick(_ === uRecord.id);
    val entry = db.getEntry(idx);
    val x = pick(x => (entry~'pwd === 1337) ==> (x === 42));
    expect(42) { concretize(context, uRecord, x) }
  }

  test ("findEntry") {
    val db = mkTestDB();
    val f = (x : ObjectExpr) => (x~'pwd === 1337);
    val result = db.findEntry(f);
    val concreteList : List[UserRecord] =
      concretizeList(context, uRecord, result);
    expect(1) { concreteList.length };
    expect(0) { concretize(context, uRecord, (concreteList.head).username) };
    expect(0) { concretize(context, uRecord, (concreteList.head).email) };
  }

  /*
  test ("symbolic record getter") {
    val db = mkTestDB();
    val idx = pick(x => x === uRecord.id);
    val entry = db.getEntry(idx);
    val x = pick(x => (entry ~ 'getPwd === 0) ==> (x === 42));
    expect(42) { concretize(context, uRecord, x) }
  }
  */
}
