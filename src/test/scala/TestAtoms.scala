package test.cap.scalasmt

import org.scalatest.FunSuite
import org.scalatest.Assertions
import cap.scalasmt._
import cap.scalasmt.AtomExpr._

class ExampleAtoms extends FunSuite {

  test ("set operations") {
    val List(a,b,c,d,e,f) = (1 to 6).toList.map(_ => new Object)
    expect(Set(a,b,c).map(AtomObject(_))) {
      (((a ++ b ++ c ++ d ++ e) -- d) & (a ++ b ++ c)).eval(EmptyEnv)
    }
  }

  test ("implicit conversion") {
    val a = new Object;
    val b = new Object;
    expect(Set(AtomObject(b))) {
      ((a ++ b) -- a).eval(EmptyEnv)
    }
  }

  test ("join expression") {
    class Node {
      var sub: Node = null;
    }
    val a = new Node;
    expect(Set(AtomObject(null))) {
      a("sub").eval(EmptyEnv)
    }

    a.sub = a;
    expect(Set(AtomObject(a))) {
      a("sub").eval(EmptyEnv)
    }
  }
}
