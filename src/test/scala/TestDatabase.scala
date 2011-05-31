package test.cap.scalasmt

import cap.scalasmt._
import org.scalatest.FunSuite
import org.scalatest.Assertions.{expect}
import java.sql

class ExampleDatabase extends FunSuite with Sceeves {
  private val context = pick(_ => true);
  private val uRecord =
    new UserRecord(0, List(), List(), List(), List(), List(), List(), context, List())

  test ("putGetUserRecord") {
    val db = new Database[UserRecord]();
    db.putEntry(uRecord.getUname(), uRecord);
    expect(true) { uRecord.equals(db.getEntry(uRecord.getUname())) };
  }
}
