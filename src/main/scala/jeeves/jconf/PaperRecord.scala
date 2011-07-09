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
    (CONTEXT.stage === Public) && CONTAINS(curtags, Accepted)

  // Some predicates...
  private val isAuthor : Formula = CONTAINS(_authors, CONTEXT.viewer);
  private val isInternal : Formula =
    CONTEXT.viewer.status >= UserStatus.reviewerL

  // The name of the paper is always visible to the authors.
  val name : Symbolic = {
    val level = mkLevel ();
    val canSee: () => Formula =
      () => isAuthor || isInternal || isPublic(getTags ())
    policy (level, canSee, HIGH);
    policy (level, () => !(canSee ()), LOW);
    mkSensitive[Title](level, _name, Title(""))
  }

  val authors : List[Symbolic] = {
    val level = mkLevel ();
    val canSee: () => Formula = {
      () =>
        isAuthor || (isInternal && (CONTEXT.stage === Decision)) ||
          isPublic(getTags ())
    }
    policy (level, canSee, HIGH);
    policy (level, () => !(canSee ()), LOW);
    _authors.map(a => mkSensitive[ConfUser](level, a, NULL))
  }

  /* Managing tags. */
  private def addTagPermission (tag : PaperTag) : Symbolic = {
    val level = mkLevel ();
    tag match {
      case NeedsReview =>
        val canSee : Formula = isInternal && CONTEXT.stage === Review;
        policy (level, canSee, HIGH);
        policy (level, !canSee, LOW);
      case ReviewedBy (reviewer) =>
        policy (level, isInternal, HIGH);
        policy (level, !isInternal, LOW)
      // Can see the "Accepted" tag if is an internal user at the decision
      // stage or if all information is visible.
      case Accepted =>
        val stage = CONTEXT.stage;
        val canSee : Formula =
          (isInternal && (stage == Decision)) || (stage === Public);
        policy (level, canSee, HIGH);
        policy (level, !canSee, LOW);
    }
    mkSensitive[PaperTag](level, tag, NULL)
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
      /* policy(level, isAuthor &&
                    ((CONTEXT.stage === Rebuttal) ||
                      (CONTEXT.stage === Decision))); */
      // The public can't see the reviews if they can see the paper name or
      // authors.
      // policy(level, () => (!(nameLevel || authorLevel)) && isPublic(getTags ()));
      mkSensitive[PaperReview](level, s, dummyReview)
    }
    reviews + (reviewId -> r);
    addTag (ReviewedBy(r.reviewer))
    r
  }
}
