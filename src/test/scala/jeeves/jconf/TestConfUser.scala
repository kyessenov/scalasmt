package test.cap.jeeves.jconf

import cap.jeeves._
import cap.jeeves.jconf._
import cap.scalasmt._
import org.scalatest.FunSuite
import org.scalatest.Assertions.{expect}
import scala.collection.immutable.Map

import JeevesLib._

class ExampleConfUser extends FunSuite {
  private val context = pickObject();

  val author0Info = TestUsers.mkUser(UserStatus.authorL, context)
  val author0Name = author0Info._1;
  val author0Email = author0Info._2;
  val author0 = author0Info._3;
  private def getAuthorCtxt0 (stage : BigInt = PaperStage.submission)
  : ConfContext =
    new ConfContext(author0.id, UserStatus.authorL, stage);
  private def getAuthorCtxt1 (stage : BigInt = PaperStage.submission)
  : ConfContext = new ConfContext(3, UserStatus.authorL, stage);

  val reviewer0Info = TestUsers.mkUser(UserStatus.reviewerL, context);
  val reviewer0 = reviewer0Info._3;
  private def getReviewerCtxt0 (stage : BigInt = PaperStage.submission)
  : ConfContext = new ConfContext(reviewer0.id, UserStatus.reviewerL, stage);

  val pc0Info = TestUsers.mkUser(UserStatus.pcL, context);
  val pc0 = pc0Info._3;
  private def getPcCtxt0 (stage : BigInt = PaperStage.submission)
  : ConfContext = new ConfContext( 2, UserStatus.pcL, stage);

  test ("name visibility") {
    // Reviewers and PC members can see names of submitters.
    expect (author0Name) {
      concretize(context, getAuthorCtxt0(), author0.name) };

    val viewMap =
      Map( (PaperStage.submission, -1)
         , (PaperStage.review, -1)
         , (PaperStage.decision, author0Name) );
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
  
  test ("email visibility") {
    expect (author0Email) {
      concretize(context, getAuthorCtxt0(), author0.email)
    };
  }
}
