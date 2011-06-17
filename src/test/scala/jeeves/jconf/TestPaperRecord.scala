package test.cap.jeeves.jconf

import cap.jeeves._
import cap.jeeves.jconf._
import cap.scalasmt._
import org.scalatest.FunSuite
import org.scalatest.Assertions.{expect}

import JeevesLib._

class ExamplePaperRecord extends FunSuite {
  val context = pickAtom();

  /*
  val author0 =
    new PaperRecord( 0, Constant(0), Constant(123), Constant(0)
                , UserStatus.authorL, context);
  val reviewer0 =
    new PaperRecord( 1, Constant(1), Constant(456), Constant(0)
                , UserStatus.reviewerL, context);
  val pc0 =
    new PaperRecord( 1, Constant(1), Constant(789), Constant(0)
                , UserStatus.pcL, context);

  test ("name visibility") {
    expect (-1) { concretize(context, reviewer0, author0.pwd) };
    expect (123) { concretize(context, author0, author0.pwd) };
  }
  */
}
