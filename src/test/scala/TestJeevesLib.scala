package test.cap.scalasmt

import cap.scalasmt._
import org.scalatest.FunSuite
import org.scalatest.Assertions.{expect}
import scala.collection.immutable.Map

class ExampleJeevesLib extends FunSuite with Sceeves {
  private val context = pick(_ => true)

  test ("sensitive") {
    val map = Map((1 : BigInt) -> (1 : BigInt), (2 : BigInt) -> (2 : BigInt), (3 : BigInt) -> (3 : BigInt));
    val x = JeevesLib.createSensitiveValue(context, map);
/*
    map foreach {
      case (keyval, valConstraint) =>
        println(keyval);
        println(valConstraint);
        assume((context === keyval) ==> (x === valConstraint))
    } */
//    val x = pick (x => (context === 0) ==> (x === 1));
    expect(1) {concretize(context, 1, x)};
    expect(1) {concretize(context, 1, x)};
  }
}
