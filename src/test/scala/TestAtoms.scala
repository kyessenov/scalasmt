package test.cap.scalasmt

import org.scalatest.FunSuite
import org.scalatest.Assertions
import cap.scalasmt._

class ExampleAtoms extends FunSuite {

  case class Dummy(id: Int) extends Atom

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

  test ("singleton and conditional") {
    import ObjectExpr._
    val a = Dummy(1);
    val b = Dummy(2);
    expect(a) {eval(a)}
    expect(Set(a)) {eval(a ++ a)}
    expect(a) {eval((a === a) ? a ! b)}
  }

  case class Node(sub: Dummy) extends Atom

  test ("relational join expression") {
    import RelExpr._

    val a = Node(null)
    expect(Set(null)) {eval(a ~ 'sub)}
    expect(Set()) {eval(NULL ~ 'sub)}
    
    val b = Dummy(1)
    val c = Node(b)
    expect(Set(null,b)) {eval((a ++ c) ~ 'sub)}
  }

  test ("SMT translation") {
    import RelExpr._

    val List(a,b,c) = (1 to 3).toList.map(Dummy(_))
    SMT.solve(a in ((a ++ b) -- ((b ++ c) & b))) 
    SMT.solve(a in a)
    SMT.solve((b in (b ++ c)) && (b in (b -- c)))
  }  

  test ("SMT fields") {
    import RelExpr._

    val x = Dummy(1);
    val y = Node(x);
    SMT.solve(y ~ 'sub === x)
    SMT.solve(! (x ~ 'sub == y ~ 'sub))
    SMT.solve(y ~ 'sub ~ 'sub === NULL)
  }

  test ("SMT variables") {
    val a = Var.makeAtom;
    val b = Dummy(1);
    expect(b) {val env = SMT.solve( a === ((a === a) ? b ! NULL)); env(a)}
  }
}
