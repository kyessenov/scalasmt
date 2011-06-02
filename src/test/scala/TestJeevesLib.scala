package test.cap.scalasmt

import cap.scalasmt._
import cap.scalasmt.JeevesLib._
import org.scalatest.FunSuite
import org.scalatest.Assertions.{expect}
import scala.collection.immutable.Map

class ExampleJeevesLib extends FunSuite {
  private val context : IntVar = pick

  test ("sensitive") {
    val map =
      Map( (1 : BigInt) -> (Constant(1))
         , (2 : BigInt) -> (Constant(2))
         , (3 : BigInt) -> (Constant(3)) );
    val x = JeevesLib.createSensitiveValue(context, map);
    expect(1) {concretize(context, 1, x)};
    expect(1) {concretize(context, 1, x)};
    expect(2) {concretize(context, 2, x)};
    expect(3) {concretize(context, 3, x)};
  }

  test ("default") {
    val map = Map(JeevesLib.default -> (Constant(1)));
    val x = JeevesLib.createSensitiveValue(context, map);
    expect(1) {concretize(x)};
  }

  test("default with other values") {
    val map =
      Map( JeevesLib.default -> Constant(1)
         , (1 : BigInt) -> Constant(2) );
//    val x = JeevesLib.createSensitiveValue(context, map);
//    expect(1) { concretize(x) };

    val y = JeevesLib.createSensitiveValue(context, map);
    expect(2) { concretize(context, 1, y) };
  }

/*
  test ("mkSensitiveValue") {
    val x =
      JeevesLib.mkSensitiveValue(
          List(2 : BigInt, 1 : BigInt, 0 : BigInt)
        , context, Constant(42), 1 : BigInt);
    expect(-1) { concretize(context, 0, x) };
    expect(42) { concretize(context, 1, x) };
    expect(42) { concretize(context, 1, x) };
  }
*/
}
