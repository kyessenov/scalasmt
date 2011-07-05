package cap.jeeves.jconf

/*
 * User records for jconf case study.
 * @author jeanyang
 */

import cap.scalasmt._
import scala.collection.immutable.List;
import cap.jeeves.JeevesLib._

object PaperStage {
  val submission    : BigInt = 0;
  val review        : BigInt = 1;
  val decision      : BigInt = 2;
  val public        : BigInt = 3;
}

object PaperTag {
  private var counter = 0;
  private def getNext() : BigInt = {
    val next = counter;
    counter = counter + 1;
    next
  }

  val needsReview : BigInt = getNext();
  val reviewed : BigInt = getNext();
  val accepted : BigInt = getNext();
}

class PaperRecord( val id : BigInt
                , _name : IntExpr
                , _authors : List[IntExpr]
                , _papertags : List[BigInt] ) extends JeevesRecord {
  private val isAuthor = CONTAINS(_authors, CONTEXT~'id);
  private val isAccepted = pickBool(default = false)

  // The name of the paper is always visible to the authors.
  val name : IntExpr = {
    val level : LevelVar = mkLevel()
    policy (level, isAuthor);
    policy (level, CONTEXT~'status >= UserStatus.reviewerL)
    policy ( level
            , (CONTEXT~'stage === PaperStage.public) && isAccepted);
    mkSensitiveInt(level, _name, -1)
  }

  // The authors of the paper are visible to the authors themselves and during
  // the reveal stage.
  val authors : List[IntExpr] = {
    val level : LevelVar = mkLevel()
    policy (level, isAuthor);
    policy (level, ((CONTEXT~'status >= UserStatus.reviewerL) &&
             (CONTEXT~'stage >= PaperStage.decision)));
      policy( level
            , (CONTEXT~'stage === PaperStage.public) && isAccepted );
    _authors.map(a => mkSensitiveInt(level, a, -1))
  }

  private def addTagPermission (tag : BigInt) : IntExpr = {
    val level = mkLevel ();
    tag match {
      case PaperTag.needsReview =>
        policy( level
              , (CONTEXT~'status >= UserStatus.reviewerL) &&
                (CONTEXT~'stage === PaperStage.review) )
      case PaperTag.reviewed =>
        policy( level
              , (CONTEXT~'stage >= PaperStage.review) )
      case PaperTag.accepted =>
        policy ( level
               , (CONTEXT~'stage >= PaperStage.decision) &&
                 (CONTEXT~'status >= UserStatus.reviewerL) )
        policy ( level
               , CONTEXT~'stage === PaperStage.public )
    }
    mkSensitiveInt(level, tag, -1)
  }

  // If a tag is not in the environment and a tag is then added to the
  // environment, how do we resolve this?
  private var _tags : List[BigInt] = _papertags
  var tags : List[IntExpr] = _tags.map(addTagPermission)
  def hasTag (tag : IntExpr) : Formula = CONTAINS(tags, tag)
 
  override def toString = "jcp" + id
  override def hashCode = id.toInt
  override def equals(that: Any) =
    if (that.isInstanceOf[PaperRecord])
      (id == that.asInstanceOf[PaperRecord].id) 
    else 
      false
}
