package test.cap.scalasmt

import org.scalatest.FunSuite
import org.scalatest.Assertions
import cap.scalasmt._
import cap.scalasmt.RelExpr._

class ExampleAtoms extends FunSuite {

  case class Dummy(id: Int)

  def eval[T](expr: Expr[T]) = expr.eval(EmptyEnv)

  test ("set operations") {
    val List(a,b,c,d,e,f) = (1 to 6).toList.map(Dummy(_))
    expect(Set(a,b,c)) {eval(((a ++ b ++ c ++ d ++ e) -- d) & (a ++ b ++ c))}
  }

  test ("object set") {
    val s @ List(a,b,c) = (1 to 3).toList.map(Dummy(_))
    expect(true) {eval(a in s)}
    expect(true) {eval(a ++ b in s)}
  }

  test ("join expression") {
    class Node {
      var sub: Node = null;
    }
    val a = new Node;
    expect(Set(null)) {eval(a('sub))}

    a.sub = a;
    expect(Set(a)) {eval(a('sub))}
    expect(Set()) {eval(Dummy(0)('sub))}

    val b = new Node;
    b.sub = b;
    expect(Set(a,b)) {eval((a ++ b)('sub))}
  }
}
