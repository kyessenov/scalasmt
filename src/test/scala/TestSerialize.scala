package test.cap.scalasmt

import cap.scalasmt._
import org.scalatest.FunSuite
import org.scalatest.Assertions.{expect}

class ExampleSerialize extends FunSuite with Sceeves {
  test ("serialize_pick") {
    val x = pick (_ === 1);
    val xSer = Persistence.serialize(x);
    val xUnser = Persistence.deserialize[IntVar](xSer);
    expect(1) {concretize(xUnser)};
  }

  // TODO: Serialize some other things.
}
