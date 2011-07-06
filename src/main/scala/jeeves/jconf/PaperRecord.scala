package cap.jeeves.jconf

/*
 * User records for jconf case study.
 * @author jeanyang
 */

import cap.scalasmt._
import scala.collection.immutable.List;
import scala.collection.mutable.Map;

import JConfBackend._

sealed trait PaperStage extends JeevesRecord
object Submission extends PaperStage
object Review extends PaperStage
object Rebuttal extends PaperStage
object Decision extends PaperStage
object Public extends PaperStage

sealed trait PaperTag extends JeevesRecord
object NeedsReview extends PaperTag
case class ReviewedBy (name : Symbolic) extends PaperTag
object Accepted extends PaperTag

case class Title (name : String) extends JeevesRecord

class PaperRecord( val id : Int
                 , _name : Title, _authors : List[ConfUser]
                 , _papertags : List[PaperTag] ) extends JeevesRecord {
  private def isPublic (curtags : List[Symbolic]) : Formula =
    (CONTEXT/'stage === Public) && CONTAINS(curtags, Accepted)

  // Some predicates...
  private val isAuthor : Formula = CONTAINS(_authors, CONTEXT/'viewer);
  private val isInternal : Formula = CONTEXT~'STATUS >= UserStatus.reviewerL

  // The name of the paper is always visible to the authors.
  val nameLevel: LevelVar = mkLevel ();
  val name : Symbolic = {
    policy (nameLevel, isAuthor);
    policy (nameLevel, CONTEXT~'status >= UserStatus.reviewerL)
    policy (nameLevel, () => isPublic(getTags ()))
    mkSensitiveObject(nameLevel, _name, Title(""))
  }

  val authorLevel: LevelVar = mkLevel ();
  val authors : List[Symbolic] = {
    policy (authorLevel, isAuthor);
    policy (authorLevel, (CONTEXT~'status >= UserStatus.reviewerL) &&
                    (CONTEXT/'stage === Decision));
    policy (authorLevel, () => isPublic(getTags ()))
    _authors.map(a => mkSensitiveObject(authorLevel, a, NULL))
  }

  /* Managing tags. */
  private def addTagPermission (tag : PaperTag) : Symbolic = {
    val level = mkLevel ();
    tag match {
      case NeedsReview =>
        policy ( level
               , isInternal && (CONTEXT/'stage === Review) )
      case ReviewedBy (reviewer) => policy ( level, isInternal )
      // Can see the "Accepted" tag if is an internal user at the decision
      // stage or if all information is visible.
      case Accepted =>
        policy ( level
               , isInternal && (CONTEXT/'stage === Decision) )
        policy ( level, CONTEXT/'stage === Public )
    }
    mkSensitiveObject(level, tag, NULL)
  }

  private var actualTags : Map[PaperTag, Symbolic] = {
    val m = Map[PaperTag, Symbolic]();
    _papertags foreach { tag => m += (tag -> addTagPermission(tag)) };
    m
  }
  def getTags () : List[Symbolic] = (actualTags.toList).map(x => x._2)
  def addTag (newtag : PaperTag) : Unit = {
    actualTags += (newtag -> addTagPermission(newtag))
  }
  def removeTag (oldtag : PaperTag) : Unit = actualTags -= oldtag
  def hasTag (tag : PaperTag) : Formula = CONTAINS(getTags (), tag)

  /* Managing reviews. */
  private var reviewIds = 0;
  private def getReviewId () : Int = {
    val id = reviewIds;
    reviewIds = reviewIds + 1;
    id
  }
  private def dummyReview = new PaperReview(-1, null, "", -1, -1, false)
  val reviews : Map[Int, Symbolic] = Map[Int, Symbolic]()
  def addReview (reviewer: ConfUser, rtext: String, score: Int, confidence: Int)
  : Symbolic = {
    val reviewId = getReviewId ();
    val r = {
      val level = mkLevel();
      val s = new PaperReview(
                reviewId, reviewer, rtext, score, confidence, isAuthor);
      policy(level, isInternal);
      policy(level, isAuthor &&
                    ((CONTEXT/'stage === Rebuttal) ||
                      (CONTEXT/'stage === Decision)));
      // The public can't see the reviews if they can see the paper name or
      // authors.
      policy(level, () => (!(nameLevel || authorLevel)) && isPublic(getTags ()));
      mkSensitiveObject(level, s, dummyReview)
    }
    reviews + (reviewId -> r);
    addTag (ReviewedBy(r/'reviewer))
    r
  }
}
