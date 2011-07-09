package test.cap.jeeves.jconf

import cap.scalasmt._
import cap.jeeves._
import cap.jeeves.jconf._
import JConfBackend._

import org.scalatest.FunSuite
import org.scalatest.Assertions.{expect}
import scala.collection.immutable.Map
import scala.collection.mutable.Set
import scala.util.Random

class ExampleJConfBackend extends FunSuite {
  def mkUser(userName : String, userStatus : UserStatus) : ConfUser =
    new ConfUser(Name (userName), userStatus);

  // jconf users.
  val public0 = mkUser("public0", PublicStatus)
  private def getPublicCtxt0 (stage: PaperStage = Submission) : ConfContext =
    ConfContext(public0, AuthorStatus, stage)

  val author0 = mkUser("author0", AuthorStatus)
  private def getAuthorCtxt0 (stage : PaperStage = Submission)
  : ConfContext = new ConfContext(author0, AuthorStatus, stage);
  val author1 = mkUser("author1", AuthorStatus);
  private def getAuthorCtxt1 (stage : PaperStage = Submission)
  : ConfContext = new ConfContext(author1, AuthorStatus, stage);
  val author2 = mkUser("author2", AuthorStatus)
  private def getAuthorCtxt2 (stage : PaperStage = Submission)
  : ConfContext = new ConfContext(author2, AuthorStatus, stage);

  val reviewer0 = mkUser("reviewer0", ReviewerStatus);
  private def getReviewerCtxt0 (stage : PaperStage = Submission)
  : ConfContext = new ConfContext(reviewer0, ReviewerStatus, stage);

  val pc0 = mkUser("pc0", PCStatus);
  private def getPcCtxt0 (stage : PaperStage = Submission)
  : ConfContext = new ConfContext(pc0, PCStatus, stage);

  // papers.
  val emptyName = Title("")

  val paper0Name = Title("my paper")
  val paper0 = mkPaper(paper0Name, List(author0, author1), Nil);

  val paper1Name = Title("hello world")
  val paper1 = mkPaper(paper1Name, List(author2), List(Accepted));

  // Name visibility
  test ("name visibility") {
    // Only author on paper can see paper that hasn't been accepted.
    expect (paper0Name) { concretize(getAuthorCtxt0(), paper0.name); }
    expect (emptyName) { concretize(getAuthorCtxt2(), paper0.name); }
    expect (emptyName) { concretize(getPublicCtxt0(Public), paper0.name); }

    val viewMap =
      Map( (Submission, paper0Name)
         , (Review, paper0Name)
         , (Decision, paper0Name));
    viewMap.foreach {
      case (stage, r) =>
        expect (r) { concretize(getReviewerCtxt0(stage), paper0.name) };
        expect (r) { concretize(getPcCtxt0(stage), paper0.name); }
    }

    // Public user should be able to see paper that has been accepted.
    expect (emptyName) { concretize(getPublicCtxt0(Decision), paper1.name); }
    expect (paper1Name) { concretize(getPublicCtxt0(Public), paper1.name); }
  }

  // Author list visibility
  test ("author list") {
    expect (true) {
      concretize(getAuthorCtxt0(), CONTAINS(paper0.authors, author0))
    };
    expect (true) {
      concretize(getAuthorCtxt1(), CONTAINS(paper0.authors, author0))
    }
    expect (false) {
      concretize(getReviewerCtxt0(), CONTAINS(paper0.authors, author0));
    }
    expect (true) {
      concretize(getReviewerCtxt0(Decision), CONTAINS(paper0.authors, author0));
    }
    expect (true) {
      concretize(getPcCtxt0(Decision), CONTAINS(paper0.authors, author0));
    }
  }

  test ("tag visibility") {
    expect (false) {
      concretize(getAuthorCtxt0(Decision), paper1.hasTag(Accepted)) }
    expect (true) {
      concretize(getAuthorCtxt0(Public), paper1.hasTag(Accepted));
    }
  }

  test ("tag state change") {
    expect (false) {
      concretize(getAuthorCtxt0(Public), paper0.hasTag(Accepted));
    }
    paper0.addTag(Accepted);
    expect (true) {
      concretize(getAuthorCtxt0(Public), paper0.hasTag(Accepted));
    }
    paper0.removeTag(Accepted);
    expect (false) {
      concretize(getAuthorCtxt0(Public), paper0.hasTag(Accepted));
    }
  }

  test ("review visibility") {
    /* TODO */
  }
}
