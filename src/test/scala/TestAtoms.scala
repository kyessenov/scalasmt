package test.cap.scalasmt

import org.scalatest.FunSuite
import org.scalatest.Assertions
import cap.scalasmt._
import cap.scalasmt.RelExpr._

class ExampleAtoms extends FunSuite {

  case class Dummy(id: Int) extends Atom

  def eval[T](expr: Expr[T]) = expr.eval

  test ("set operations") {
    val List(a,b,c,d,e,f) = (1 to 6).toList.map(Dummy(_))
    expect(Set(a,b,c)) {eval(((a ++ b ++ c ++ d ++ e) -- d) & (a ++ b ++ c))}
  }

  test ("object set") {
    val s @ List(a,b,c) = (1 to 3).toList.map(Dummy(_))
    expect(true) {eval(a in s)}
    expect(true) {eval(a ++ b in s)}
  }

  test ("singleton and conditional") {
    val d1 = Dummy(1);
    val d2 = Dummy(2);
    val a = Object(d1);
    val b = Object(d2);
    expect(d1) {eval(a)}
    expect(Set(d1)) {eval(a ++ a)}
    expect(d1) {eval((a === a) ? a ! b)}
    expect(Set(d1,d2)) {eval((a === a) ? (a ++ b) ! a)}
  }

  class Node(var sub: Node = null) extends Atom

  test ("join expression") {
    val a = new Node;
    expect(Set(null)) {eval(a('sub))}

    a.sub = a;
    expect(Set(a)) {eval(a('sub))}
    expect(Set()) {eval(Dummy(0)('sub))}

    val b = new Node;
    b.sub = b;
    expect(Set(a,b)) {eval((a ++ b)('sub))}
  }

  test ("SMT translation") {
    val List(a,b,c) = (1 to 3).toList.map(Dummy(_))
    SMT.solve(a in ((a ++ b) -- ((b ++ c) & b))) 
    SMT.solve(a in a)
    SMT.solve((b in (b ++ c)) && (b in (b -- c)))
  }  

  test ("SMT fields") {
    val x = new Node;
    val y = new Node;
    SMT.solve(x('sub) === NULL)
    SMT.solve(x('sub) === y('sub))
    x.sub = y;
    SMT.solve(x('sub) === y)
  }

  test ("SMT variables") {
    val a = Var.makeAtom;
    val x = new Node;
    x.sub = x;
    expect(null) {val env = SMT.solve(a === NULL); env(a)}
    expect(x) {val env = SMT.solve(x('sub) === a); env(a)} 
    expect(x) {val env = SMT.solve(a('sub) === x); env(a)}
  }
}
