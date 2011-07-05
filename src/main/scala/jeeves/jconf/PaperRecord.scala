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
  private val isAuthor : Formula = CONTAINS(_authors, CONTEXT/'viewer);
  private val isAccepted : Formula = CONTAINS(_papertags, Accepted);

  // The name of the paper is always visible to the authors.
  val name : IntExpr = {
    val level : LevelVar = mkLevel()
    policy (level, isAuthor);
    policy (level, CONTEXT~'status >= UserStatus.reviewerL)
    policy ( level
            , (CONTEXT/'stage === Public) && isAccepted);
    mkSensitiveInt(level, _name, -1)
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

  // If a tag is not in the environment and a tag is then added to the
  // environment, how do we resolve this?
  private var _tags : List[PaperTag] = _papertags
  var tags : List[Symbolic] = _tags.map(addTagPermission)
  def hasTag (tag : PaperTag) : Formula = CONTAINS(tags, tag)
}
