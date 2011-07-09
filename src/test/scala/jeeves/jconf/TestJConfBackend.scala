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
  def mkUser(userName : String, userStatus : BigInt) : ConfUser =
    new ConfUser(Name (userName), userStatus);

  // jconf users.
  val author0 = mkUser("author0", UserStatus.authorL)
  private def getAuthorCtxt0 (stage : PaperStage = Submission)
  : ConfContext = new ConfContext(author0, stage);
  val author1 = mkUser("author1", UserStatus.authorL);
  private def getAuthorCtxt1 (stage : PaperStage = Submission)
  : ConfContext = new ConfContext(author1, stage);
  val author2 = mkUser("author2", UserStatus.authorL)
  private def getAuthorCtxt2 (stage : PaperStage = Submission)
  : ConfContext = new ConfContext(author2, stage);

  val reviewer0 = mkUser("reviewer0", UserStatus.reviewerL);
  private def getReviewerCtxt0 (stage : PaperStage = Submission)
  : ConfContext = new ConfContext(reviewer0, stage);

  val pc0 = mkUser("pc0", UserStatus.pcL);
  private def getPcCtxt0 (stage : PaperStage = Submission)
  : ConfContext = new ConfContext(pc0, stage);

  // papers.
  val emptyName = Title("")

  val paper0Name = Title("my paper")
  val paper0 = mkPaper(paper0Name, List(author0, author1), Nil);

  val paper1Name = Title("hello world")
  val paper1 = mkPaper(paper1Name, List(author2), List(Accepted));

  // Name visibility
  test ("name visibility") {
    expect(paper0Name) { concretize(getAuthorCtxt0(), paper0.name); }
    expect(emptyName) { concretize(getAuthorCtxt2(), paper0.name); }

    val viewMap =
      Map((Submission, paper0Name), (Review, paper0Name), (Decision, paper0Name));
    viewMap.foreach {
      case (stage, r) =>
        expect (r) {
          concretize(getReviewerCtxt0(stage), paper0.name)
        };
        expect (r) {
          concretize(getPcCtxt0(stage), paper0.name);
        }
    }
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
      concretize( getReviewerCtxt0(Submission)
                , CONTAINS(paper0.authors, author0));
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

  /*
  test ("getUser using default user") {
    val otherUser0 = getUser(uid0);
    expect (true) { user0.equals(otherUser0) };
  }

  test ("isFriends") {
    expect (true) { getBool(uid0, isFriends(uid0, uid1)) };
    expect (true) { getBool(uid1, isFriends(uid0, uid1)) };
  }

  test ("friends") {
    val friends = getFriends(uid0);
    val friendStrs = printStringList(uid1, friends);
    expect (true) { friendStrs.contains("kuaty") };
  }

  test ("view sensitive data non-friend") {
    expect (false) { concretize(snbContext, user0, isFriends(uid0, uid2)) };
    expect (-1) { concretize(snbContext, user0, user2.network) };
    expect (fromString("MIT")) {
      concretize(snbContext, user1, user1.network)
    };
  }

  test ("getFriendNetworks") {
    val networks = getFriendNetworks(uid0);
    val networkStrs = printStringList(uid1, networks);
    expect (true) { networkStrs.contains("MIT") } ;
  }

  test("getUsersByNetworks view by friend") {
    val friends = getUsersByNetwork("MIT");
    val concreteFriends = getConcreteRecordList(uid0, friends);
    concreteFriends.foreach(x => println(x.id));
    expect (2) { concreteFriends.length };
    expect (true)
      { concreteFriends.exists(x => x.id == fromString("jeanyang")) };
    expect (true)
      { concreteFriends.exists(x => x.id == fromString("kuaty")) };
  }
  */
}
