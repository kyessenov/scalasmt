package test.cap.jeeves.jconf

import cap.jeeves._
import cap.jeeves.jconf._
import cap.scalasmt._
import org.scalatest.FunSuite
import org.scalatest.Assertions.{expect}
import scala.collection.immutable.Map

import JeevesLib._

class ExampleConfUser extends FunSuite {
  val context = pickAtom();

  val author0 =
    new ConfUser( 0, Constant(0), Constant(123), Constant(10)
                , UserStatus.authorL, context);
  private def getAuthorCtxt0 (stage : BigInt = PaperStage.submission)
  : ConfContext =
    new ConfContext(0, 0, UserStatus.authorL, stage);
  private def getAuthorCtxt1 (stage : BigInt = PaperStage.submission)
  : ConfContext =
    new ConfContext(3, 3, UserStatus.authorL, stage);

  val reviewer0 =
    new ConfUser( 1, Constant(1), Constant(456), Constant(11)
                , UserStatus.reviewerL, context);
  private def getReviewerCtxt0 (stage : BigInt = PaperStage.submission)
  : ConfContext =
    new ConfContext(1, 1, UserStatus.reviewerL, stage);

  val pc0 =
    new ConfUser( 2, Constant(1), Constant(789), Constant(12)
                , UserStatus.pcL, context);
  private def getPcCtxt0 (stage : BigInt = PaperStage.submission)
  : ConfContext =
    new ConfContext( 2, 2, UserStatus.pcL, stage);

  test ("name visibility") {
    // Reviewers and PC members can see names of submitters.
    expect (0) {
      concretize(context, getAuthorCtxt0(), author0.name) };

    val viewMap =
      Map( (PaperStage.submission, -1)
         , (PaperStage.review, -1)
         , (PaperStage.authorReveal, 0) );
    viewMap.foreach {
      case (stage, r) =>
        expect (r) {
          concretize(context, getReviewerCtxt0(stage), author0.name)
        };
        expect (r) {
          concretize(context, getPcCtxt0(stage), author0.name);
        }
    }

    // Other authors should not be able to see the current author ever.
    expect (-1) { concretize(context, getAuthorCtxt1(), author0.name) };
  }

  test ("pwd visibility") {
    expect (-1) {
      concretize(context, getReviewerCtxt0(), author0.pwd) };
    expect (-1) { concretize(context, getPcCtxt0(), author0.pwd) }
    expect (123) { concretize(context, getAuthorCtxt0(), author0.pwd) };
  }

  test ("email visibility") {
    expect (10) { concretize(context, getAuthorCtxt0(), author0.email) };
  }
}
