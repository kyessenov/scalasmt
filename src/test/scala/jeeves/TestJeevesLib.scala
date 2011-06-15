package test.cap.jeeves

import cap.scalasmt._
import cap.jeeves._
import JeevesLib._
import org.scalatest.FunSuite
import org.scalatest.Assertions.{expect}
import scala.collection.immutable.Map

class ExampleJeevesLib extends FunSuite {
  private val level = pick(default = 0)

  test ("sensitive") {
    val map: SensitiveMap =
      Map((1, 1), (2, 2), (3, 3));
    val x = JeevesLib.createSensitiveValue(level, map);
    expect(1) {concretize(level, 1, x)};
    expect(1) {concretize(level, 1, x)};
    expect(2) {concretize(level, 2, x)};
    expect(3) {concretize(level, 3, x)};
  }

  test ("default") {
    val map: SensitiveMap = 
      Map((JeevesLib.default, 1));
    val x = JeevesLib.createSensitiveValue(level, map);
    expect(1) {concretize(level, 0, x)};
  }

  test("default with other values") {
    val map: SensitiveMap =
      Map((JeevesLib.default, 1), (1, 2));
    val x = JeevesLib.createSensitiveValue(level, map);
    expect(1) { concretize(level, 0, x) };

    val y = JeevesLib.createSensitiveValue(level, map);
    expect(2) { concretize(level, 1, y) };
  }

  test ("mkSensitiveValue") {
    val x =
      JeevesLib.mkSensitiveValue(
          List(2 : BigInt, 1 : BigInt, 0 : BigInt)
        , level, Constant(42), 1 : BigInt);
    expect(-1) { concretize(level, 0, x) };
    expect(42) { concretize(level, 1, x) };
    expect(42) { concretize(level, 1, x) };
  }

  case class Node(v: Int) extends Atom
  test ("concretizeList non-null") {
    val x = pickAtom(x => x === Node(0));
    val symList = List(x);
    val level = pickAtom();
    val cList : List[Node] = concretizeList(level, x, symList);
    expect(1) { cList.length };
    expect(0) { cList.head.v };
  }

  test ("concretizeList null") {
    val x = pickAtom(default = NULL);
    val symList = List(x);
    val level = pickAtom();
    val cList : List[Node] = concretizeList(level, x, symList);
    cList.foreach(x => println(x));
    expect(0) { cList.length };
  }
}
