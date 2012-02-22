package test.cap.jeeves

import org.scalatest.FunSuite
import org.scalatest.Assertions
import cap.scalasmt._
import cap.jeeves._

class ExampleContext extends FunSuite with JeevesLib {

  case class Dummy(ID: BigInt) extends Atom

  def eval[T](expr: Expr[T]) = expr.eval

  def mkElt(x: Dummy): Symbolic = {
    val l = mkLevel();
    policy(l, !(CONTEXT === Dummy(1)), LOW);
    mkSensitive(l, x, Dummy(0))
  }
  val c = (1 to 3).toList.map(Dummy(_))
  val s = c.map(mkElt)

  test ("low confidentiality context") {
    expect(true) { concretize(NULL, s.has(Dummy(0))) }
  }

  test ("high confidentiality context") {
    expect(true) { concretize(Dummy(1), s.has(Dummy(1))) }
  }
}
