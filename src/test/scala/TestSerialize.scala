package test.cap.scalasmt

import cap.scalasmt._
import org.scalatest.FunSuite
import org.scalatest.Assertions.{expect}

import Persistence.{serialize, deserialize}

class ExampleSerialize extends FunSuite with Sceeves {
  test ("pick") {
    val x = pick (_ === 1);
    val x0 = deserialize[IntVar](serialize(x));
    expect(1) {concretize(x0)};
  }

  test ("conditional pick") {
    val y = pick ();
    val x = pick (_ === y);
    val x0 = deserialize[IntVar](serialize(x));
    assume (y === 1);
    expect(1) {concretize(x0)};
  }
}
