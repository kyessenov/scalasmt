package test.cap.jeeves

import org.scalatest.FunSuite
import org.scalatest.Assertions
import cap.scalasmt._
import cap.jeeves._

class ExampleContext extends FunSuite with JeevesLib {

  case class Dummy(ID: BigInt) extends JeevesRecord

  def eval[T](expr: Expr[T]) = expr.eval

  def mkElt(x: Dummy): Symbolic = {
    val l = mkLevel();
    policy(l, !(CONTEXT.ID === IntVal(1)), LOW);
    mkSensitive(l, x, Dummy(0))
  }

  val x: Dummy = Dummy(1);
  val x_s: Symbolic = mkElt(x);
  
  val c = (1 to 3).toList.map(Dummy(_))
  val s = c.map(mkElt)


  test ("high confidentialy context") {
    expect (x) { concretize(x, x_s) }
  }

  test ("low confidentiality context") {
    expect (Dummy(0)) { concretize(NULL, x_s) }
  }

  /* Lists. */
  test ("low confidentiality context - list") {
    expect(true) { concretize(NULL, s.has(Dummy(0))) }
  }

  test ("high confidentiality context - list") {
    expect(true) { concretize[Boolean](Dummy(1), s.has(Dummy(1))) }
  }
}
