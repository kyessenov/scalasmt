package test.cap.jeeves.jconf

import cap.jeeves._
import cap.jeeves.jconf._
import cap.scalasmt._
import org.scalatest.FunSuite
import org.scalatest.Assertions.{expect}

import JeevesLib._

class ExamplePaperRecord extends FunSuite {
  val context = pickObject();

  val paper0 = new PaperRecord(0, 33, List(0, 3), context);

  private def getAuthorCtxt0 (stage : BigInt = PaperStage.submission)
  : ConfContext =
    new ConfContext(0, 0, UserStatus.authorL, stage);
  private def getAuthorCtxt1 (stage : BigInt = PaperStage.submission) 
  : ConfContext =
    new ConfContext(3, 3, UserStatus.authorL, stage);
  private def getAuthorCtxt2 (stage : BigInt = PaperStage.submission)
  : ConfContext =
    new ConfContext(4, 4, UserStatus.authorL, stage);

  private def getReviewerCtxt0 (stage : BigInt = PaperStage.submission)
  : ConfContext =
    new ConfContext(1, 1, UserStatus.reviewerL, stage);
  private def getPcCtxt0 (stage : BigInt = PaperStage.submission)
  : ConfContext =
    new ConfContext( 2, 2, UserStatus.pcL, stage);

  // Name visibility
  test ("name visibility") {
    expect(33) {
      concretize(context, getAuthorCtxt0(), paper0.name);
    }
    expect(-1) {
      concretize(context, getAuthorCtxt2(), paper0.name);
    }

    val viewMap =
      Map( (PaperStage.submission, 33)
         , (PaperStage.review, 33)
         , (PaperStage.authorReveal, 33) );
    viewMap.foreach {
      case (stage, r) =>
        expect (r) {
          concretize(context, getReviewerCtxt0(stage), paper0.name)
        };
        expect (r) {
          concretize(context, getPcCtxt0(stage), paper0.name);
        }
    }
  }

  // Author visibility
  test ("author list visibility") {
    expect (true) {
      concretize(context, getAuthorCtxt0(0), CONTAINS(paper0.authors, 0))
    };
    expect (true) {
      concretize(context, getAuthorCtxt1(0), CONTAINS(paper0.authors, 0))
    };
    expect (false) {
      concretize(context, getAuthorCtxt2(0), CONTAINS(paper0.authors, 0))
    };

  }
}
