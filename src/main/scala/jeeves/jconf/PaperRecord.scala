package cap.jeeves.jconf

/*
 * User records for jconf case study.
 * @author jeanyang
 */

import cap.scalasmt._
import scala.collection.immutable.List;
import UserStatus._
import cap.jeeves.JeevesLib._

object Viewer {
  val low   : LevelTy = 0
  val high  : LevelTy = 1
  val levels = List(low, high)
}

object PaperStage {
  val submission    : LevelTy = 0;
  val review        : LevelTy = 1;
  val authorReveal  : LevelTy = 2;
  val publicReveal  : LevelTy = 3;
}

/* NOTE: We will not be using this with beans for now... */
class PaperRecord( val id : BigInt
                , _name : IntExpr
                , _authors : List[IntExpr]
                , val context : AtomVar ) extends Atom {
  // The name of the paper is always visible to the authors.
  val nameLevel : IntVar = pick(default = Viewer.low)
  assume((context~'status > UserStatus.reviewerL) ==> (name === Viewer.high));
  val name : IntExpr = mkSensitive(nameLevel, _name)

  // The authors of the paper are visible to the authors themselves and during
  // the reveal stage.
  val authorsLevel : IntVar = pick(default = Viewer.low)
  assume(((context~'status === UserStatus.reviewerL) &&
    (context~'stage > PaperStage.authorReveal)) ==>
          (authorsLevel === Viewer.high))
  val authors : List[IntExpr] = _authors.map(a => mkSensitive(authorsLevel, a))

  private def mkSensitive(levelVar : IntVar, v : IntExpr) : IntExpr = {
    val isAuthor : Formula = CONTAINS(_authors, context~'name);
    assume(CONTAINS(Viewer.levels, authorsLevel));
    assume(isAuthor ==> (levelVar === Viewer.high));
    mkSensitiveValue(Viewer.levels, levelVar, v, Viewer.high)
  }
 
  override def toString = "jcp" + id
  override def hashCode = id.toInt
  override def equals(that: Any) =
    if (that.isInstanceOf[PaperRecord])
      (id == that.asInstanceOf[PaperRecord].id) 
    else 
      false
}
