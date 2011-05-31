package test.cap.scalasmt

import cap.scalasmt._
import cap.scalasmt.JeevesLib._
import org.scalatest.FunSuite
import org.scalatest.Assertions.{expect}
import scala.collection.immutable.Map

class ExampleJeevesLib extends FunSuite {
  private val context = pick(_ => true)

  test ("sensitive") {
    val map = Map((1 : BigInt) -> (1 : BigInt), (2 : BigInt) -> (2 : BigInt), (3 : BigInt) -> (3 : BigInt));
    val x = JeevesLib.createSensitiveValue(context, map);
    expect(1) {concretize(context, 1, x)};
    expect(1) {concretize(context, 1, x)};
    expect(2) {concretize(context, 2, x)};
    expect(3) {concretize(context, 3, x)};
  }

  test ("default") {
    val map = Map(JeevesLib.default -> (1 : BigInt));
    val x = JeevesLib.createSensitiveValue(context, map);
    expect(1) {concretize(x)};
  }

}
