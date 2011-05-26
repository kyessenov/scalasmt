package test.cap.scalasmt

import cap.scalasmt._
import org.scalatest.FunSuite
import org.scalatest.Assertions.{expect}

class ExampleSerialize extends FunSuite with Sceeves with SceevesStr {
  test ("serialize_pick") {
    val x = pick (_ === 1);
    val x_ser = serialize[IntVar](x);
    val x_unser = unserialize[IntVar](x_ser);
    expect(1) {concretize(x_unser)};
  }

  // TODO: Serialize some other things.
}
