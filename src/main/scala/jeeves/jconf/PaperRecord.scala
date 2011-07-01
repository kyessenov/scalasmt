package cap.jeeves.jconf

/*
 * User records for jconf case study.
 * @author jeanyang
 */

import cap.scalasmt._
import scala.collection.immutable.List;
import scala.collection.immutable.{Set => ISet}
import scala.collection.mutable.{Set => MSet}
import cap.jeeves.JeevesLib._

object PaperStage {
  val submission    : LevelTy = 0;
  val review        : LevelTy = 1;
  val decision      : LevelTy = 2;
  val public        : LevelTy = 3;
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
                , _papertags : MSet[BigInt] // & keywords
                , val context : ObjectVar ) extends Atom {
  private val isAuthor = CONTAINS(_authors, context~'id);
  private val isAccepted = pickBool(default = false)

  // The name of the paper is always visible to the authors.
  val name : IntExpr = {
    val level : IntVar = mkLevel()
    policy (level, isAuthor, Viewer.high);
    policy (level, context~'status >= UserStatus.reviewerL, Viewer.high)
    policy ( level
            , (context~'stage === PaperStage.public) && isAccepted
            , Viewer.high);
    mkSensitiveValue(level, _name)
  }

  // The authors of the paper are visible to the authors themselves and during
  // the reveal stage.
  val authors : List[IntExpr] = {
    val level : IntVar = mkLevel()
    policy (level, isAuthor, Viewer.high);
    policy (level, ((context~'status >= UserStatus.reviewerL) &&
             (context~'stage >= PaperStage.decision)), Viewer.high);
      policy( level
            , (context~'stage === PaperStage.public) && isAccepted
            , Viewer.high);
    _authors.map(a => mkSensitiveValue(level, a))
  }

  private def addTagPermission (tag : BigInt) : IntExpr = {
    val level = mkLevel ();
    tag match {
      case PaperTag.needsReview =>
        policy( level
              , (context~'status >= UserStatus.reviewerL) &&
                (context~'stage === PaperStage.review)
              , Viewer.high)
      case PaperTag.reviewed =>
        policy( level
              , (context~'stage >= PaperStage.review)
              , Viewer.high )
      case PaperTag.accepted =>
        policy ( level
               , (context~'stage >= PaperStage.decision) &&
                 (context~'status >= UserStatus.reviewerL)
               , Viewer.high )
        policy ( level
               , context~'stage === PaperStage.public
               , Viewer.high )
    }
    mkSensitiveValue(level, tag)
  }

  // If a tag is not in the environment and a tag is then added to the
  // environment, how do we resolve this?
  private var _tags : MSet[BigInt] = _papertags
  var tags : MSet[IntExpr] = _tags.map(addTagPermission)

  def addTag (newtag : BigInt) : Unit = {
    _tags.add(newtag);
    tags = _tags.map(addTagPermission);
    if (newtag == PaperTag.accepted) assume(isAccepted)
  }
  def hasTag (tag : IntExpr) : Formula = CONTAINS(tags, tag)
 
  override def toString = "jcp" + id
  override def hashCode = id.toInt
  override def equals(that: Any) =
    if (that.isInstanceOf[PaperRecord])
      (id == that.asInstanceOf[PaperRecord].id) 
    else 
      false
}
