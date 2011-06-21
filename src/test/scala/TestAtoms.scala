package test.cap.scalasmt

import org.scalatest.FunSuite
import org.scalatest.Assertions
import cap.scalasmt._

class ExampleAtoms extends FunSuite {

  case class Dummy(id: BigInt) extends Atom

  def eval[T](expr: Expr[T]) = expr.eval

  test ("set operations") {
    import RelExpr._
    val List(a,b,c,d,e,f) = (1 to 6).toList.map(Dummy(_))
    expect(Set(a,b,c)) {eval(((a ++ b ++ c ++ d ++ e) -- d) & (a ++ b ++ c))}
  }

  test ("object set") {
    import RelExpr._
    val s @ List(a,b,c) = (1 to 3).toList.map(Dummy(_))
    expect(true) {eval(a in s)}
    expect(true) {eval(a ++ b in s)}
  }

  test ("large object set") {
    import RelExpr._
    val k = 20
    val a = (1 to k).toList.map(Dummy(_))
    val b = (k + 1 to 2*k).toList.map(Dummy(_))
    SMT.solve(a in (a ++ b))
    SMT.solve((a & b) === Set())
  }

  test ("singleton and conditional") {
    import ObjectExpr._
    val a = Dummy(1);
    val b = Dummy(2);
    expect(a) {eval(a)}
    expect(Set(a)) {eval(a ++ a)}
    expect(Set(a, null)) {eval(a ++ NULL)}
    expect(a) {eval((a === a) ? a ! b)}
  }

  case class Node(sub: Dummy) extends Atom

  test ("relational join expression") {
    import RelExpr._

    val a = Node(null)
    expect(Set(null)) {eval(a >< 'sub)}
    expect(Set()) {eval(NULL >< 'sub)}
    
    val b = Dummy(1)
    val c = Node(b)
    expect(Set(null,b)) {eval((a ++ c) >< 'sub)}
  }

  test ("object int field") {
    import ObjectExpr._
    val a = Dummy(1);
    expect(1) {eval(a ~ 'id)}
  }

  test ("SMT set translation") {
    import RelExpr._

    val List(a,b,c) = (1 to 3).toList.map(Dummy(_))
    SMT.solve(a in ((a ++ b) -- ((b ++ c) & b))) 
    SMT.solve(a in a)
    SMT.solve((b in (b ++ c)) && (b in (b -- c)))
  }  

  test ("SMT relational join") {
    import RelExpr._

    val x = Dummy(1);
    val y = Node(x);
    SMT.solve(y >< 'sub === x)
    SMT.solve(x >< 'sub === NULL)
    SMT.solve(y >< 'sub >< 'sub === NULL)
  }

  test ("SMT variable translation") {
    val a = Var.makeObject;
    val b = Dummy(1);
    expect(b) {val env = SMT.solve( a === ((a === a) ? b ! NULL)); env(a)}
  }

  case class Record(f: IntExpr, i: BigInt) extends Atom

  test ("SMT int field constraints") {
    val i = Var.makeInt;
    val a = Var.makeObject;
    val r = Record(i, 1337);
    val env = SMT.solve( (a === r && a ~ 'f === a ~ 'i));
    expect(1337) {env(i)}; 
    expect(r) {env(a)};
  }

  test ("SMT object field constraints") {
    import ObjectExpr._
    var x = Dummy(1);
    var y = Node(x);
    SMT.solve(y / 'sub === x);
  }
}
