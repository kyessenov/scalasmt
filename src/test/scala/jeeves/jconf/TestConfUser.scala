package test.cap.jeeves.jconf

import cap.jeeves._
import cap.jeeves.jconf._
import cap.scalasmt._
import org.scalatest.FunSuite
import org.scalatest.Assertions.{expect}

import JeevesLib._

class ExampleConfUser extends FunSuite {
  val context = pickAtom();

  val author0 =
    new ConfUser( 0, Constant(0), Constant(123), Constant(0)
                , UserStatus.authorL, context);
  val author0Status =
    new ConfContext(0, 0, UserStatus.authorL, PaperStage.submission);
  val reviewer0 =
    new ConfUser( 1, Constant(1), Constant(456), Constant(0)
                , UserStatus.reviewerL, context);
  val pc0 =
    new ConfUser( 2, Constant(1), Constant(789), Constant(0)
                , UserStatus.pcL, context);

  test ("name visibility") {
    expect (0) { concretize(context, author0Status, author0.name) };
    expect (0) { concretize(context, reviewer0, author0.name) };
//    expect (0) { concretize(context, pc0, author0.name) };
  }

  test ("pwd visibility") {
    expect (-1) { concretize(context, reviewer0, author0.pwd) };
    expect (-1) { concretize(context, pc0, author0.pwd) }
    expect (123) { concretize(context, author0, author0.pwd) };
  }
}
