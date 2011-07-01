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

  // The name of the paper is always visible to the authors.
  val name : IntExpr = {
    val level : IntVar = mkLevel()
    policy (level, context~'status >= UserStatus.reviewerL, Viewer.high)
    policy( level
            , (context~'stage === PaperStage.public) &&
              CONTAINS(tags, PaperTag.accepted)
            , Viewer.high);
    mkSensitive(level, _name)
  }

  // The authors of the paper are visible to the authors themselves and during
  // the reveal stage.
  val authors : List[IntExpr] = {
    val level : IntVar = mkLevel()
      policy(level, ((context~'status >= UserStatus.reviewerL) &&
             (context~'stage >= PaperStage.decision)), Viewer.high);
      policy( level
            , (context~'stage === PaperStage.public) &&
              CONTAINS(tags, PaperTag.accepted)
            , Viewer.high);
    _authors.map(a => mkSensitive(level, a))
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
    mkSensitive(level, tag)
  }

  // If a tag is not in the environment and a tag is then added to the
  // environment, how do we resolve this?
  private var _tags : MSet[BigInt] = _papertags
  private var tags : MSet[IntExpr] = _tags.map(addTagPermission)

  def addTag (newtag : BigInt) =
    _tags.add(newtag);
    tags = _tags.map(addTagPermission)
  def hasTag (tag : IntExpr) : Formula = CONTAINS(tags, tag)

  private def mkSensitive(levelVar : IntVar, v : IntExpr) : IntExpr = {
    val isAuthor : Formula = CONTAINS(_authors, context~'name);
    policy(levelVar, isAuthor, Viewer.high);
    mkSensitiveValue(levelVar, v)
  }
 
  override def toString = "jcp" + id
  override def hashCode = id.toInt
  override def equals(that: Any) =
    if (that.isInstanceOf[PaperRecord])
      (id == that.asInstanceOf[PaperRecord].id) 
    else 
      false
}
