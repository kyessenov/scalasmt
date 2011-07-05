package cap.jeeves.jconf

/*
 * User records for jconf case study.
 * @author jeanyang
 */

import cap.scalasmt._
import scala.collection.immutable.List;
import cap.jeeves.JeevesLib._

sealed trait PaperStage extends JeevesRecord
object Submission extends PaperStage
object Review extends PaperStage
object Decision extends PaperStage
object Public extends PaperStage

sealed trait PaperTag extends JeevesRecord
object NeedsReview extends PaperTag
object Reviewed extends PaperTag
object Accepted extends PaperTag

class PaperRecord( _name : IntExpr, _authors : List[ConfUser]
                 , _papertags : List[PaperTag] ) extends JeevesRecord {
  // Some predicates...
  private val isAuthor : Formula = CONTAINS(_authors, CONTEXT/'viewer);
  private val isAccepted : Formula = CONTAINS(_papertags, Accepted);
  private val isPublicInfo : Formula = (CONTEXT/'stage === Public) && isAccepted

  // The name of the paper is always visible to the authors.
  val name : IntExpr = {
    val level : LevelVar = mkLevel();
    policy (level, isAuthor);
    policy (level, CONTEXT~'status >= UserStatus.reviewerL)
    policy (level, isPublicInfo)
    mkSensitiveInt(level, _name, -1)
  }

  val authors : List[Symbolic] = {
    val level : LevelVar = mkLevel;
    policy (level, isAuthor);
    policy ( level, (CONTEXT~'status >= UserStatus.reviewerL) &&
                    (CONTEXT/'stage === Decision));
    policy (level, isPublicInfo);
    _authors.map(a => mkSensitiveObject(level, a, NULL))
  }

  private def addTagPermission (tag : PaperTag) : Symbolic = {
    val level = mkLevel ();
    tag match {
      case NeedsReview =>
        policy( level
              , (CONTEXT~'status >= UserStatus.reviewerL) &&
                (CONTEXT/'stage === Review) )
      case Reviewed =>
        policy( level
              , (CONTEXT/'stage === Review || CONTEXT/'stage === Decision) )
      case Accepted =>
        policy ( level
               , (CONTEXT/'stage === Decision) &&
                 (CONTEXT~'status >= UserStatus.reviewerL) )
        policy ( level, CONTEXT/'stage === Public )
    }
    mkSensitiveObject(level, tag, NULL)
  }

  var tags : List[Symbolic] = _papertags.map(addTagPermission)
  def hasTag (tag : PaperTag) : Formula = CONTAINS(tags, tag)
}
