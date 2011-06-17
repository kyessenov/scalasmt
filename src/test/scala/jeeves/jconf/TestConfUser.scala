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
    new ConfUser( 0, Constant(0), Constant(123), Constant(10)
                , UserStatus.authorL, context);
  val authorCtxt0 =
    new ConfContext(0, 0, UserStatus.authorL, PaperStage.submission);
  val authorCtxt1 =
    new ConfContext(3, 3, UserStatus.authorL, PaperStage.submission);
  val reviewer0 =
    new ConfUser( 1, Constant(1), Constant(456), Constant(11)
                , UserStatus.reviewerL, context);
  val reviewerCtxt0 =
    new ConfContext(1, 1, UserStatus.reviewerL, PaperStage.submission);
  val pc0 =
    new ConfUser( 2, Constant(1), Constant(789), Constant(12)
                , UserStatus.pcL, context);
  val pcCtxt0 =
    new ConfContext( 2, 2, UserStatus.pcL, PaperStage.submission);

  test ("name visibility") {
    // Reviewers and PC members can see names of submitters.
    expect (0) { concretize(context, authorCtxt0, author0.name) };
    expect (0) { concretize(context, reviewerCtxt0, author0.name) };
    expect (0) { concretize(context, pcCtxt0, author0.name) };
    expect (-1) { concretize(context, authorCtxt1, author0.name) };
  
    // TODO: Test new rules for name visibility.
  }

  test ("pwd visibility") {
    expect (-1) { concretize(context, reviewerCtxt0, author0.pwd) };
    expect (-1) { concretize(context, pcCtxt0, author0.pwd) }
    expect (123) { concretize(context, authorCtxt0, author0.pwd) };
  }

  test ("email visibility") {
    expect (10) { concretize(context, authorCtxt0, author0.email) };
  }
}
