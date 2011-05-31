package test.cap.scalasmt

import cap.scalasmt._
import org.scalatest.FunSuite
import org.scalatest.Assertions.{expect}
import java.sql

class ExampleUserRecord extends FunSuite with Sceeves {
  test ("equalsSelf") {
    val context = pick (_ => true);
    val u = new UserRecord(0, List(), List(), List(), List(), List(), List(), context, List());
    expect(true) { u.equals(u)  };
  }
  // TODO: More substantive tests.
}
