package cap.jeeves.jconf

/*
 * User records for jconf case study.
 * @author jeanyang
 */

import cap.scalasmt._
import scala.collection.mutable.Map;
import PaperLevels._
import UserStatus._
import cap.jeeves.JeevesLib._

object PaperStage {

}

import PaperStage._

/* NOTE: We will not be using this with beans for now... */
class PaperRecord( val id : BigInt
                , _name : IntExpr, val namep : LevelTy
                , _authors : List[IntExpr]
                /* Pass in prviacy level and context variables. */
                , val context : AtomVar ) extends Atom {
  val level = pick(default = paperDefaultL)

  val name = mkSensitive(_name, namep);
 
  /* Set initial privacy levels. */
  assume(level === context~'status)
  assume(CONTAINS(levels, level))

  private def mkSensitive (v : IntExpr, p : LevelTy) =
    if (p > PaperLevels.paperDefaultL)
      mkSensitiveValue(PaperLevels.levels, level, v, p);
    else v

  override def toString = "jcp" + id
  override def hashCode = id.toInt
  override def equals(that: Any) =
    if (that.isInstanceOf[PaperRecord])
      (id == that.asInstanceOf[PaperRecord].id) 
    else 
      false
}
