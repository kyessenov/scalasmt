package cap.jeeves.jconf

/*
 * User records for jconf case study.
 * @author jeanyang
 */

import cap.scalasmt._
import scala.collection.immutable.List;
import cap.jeeves.JeevesLib._

class PaperRecord( val id : BigInt
                , _name : IntExpr
                , _authors : List[IntExpr]
                , val context : ObjectVar ) extends Atom {
  private val isAuthor = CONTAINS(_authors, context~'id);

  // The name of the paper is always visible to the authors.
  val name : IntExpr = {
    val nameLevel : IntVar = pick(default = Viewer.low)
    assume(isAuthor ==> (nameLevel === Viewer.high))
    assume((context~'status >= UserStatus.reviewerL) ==>
            (nameLevel === Viewer.high));
    mkSensitive(nameLevel, _name)
  }

  // The authors of the paper are visible to the authors themselves and during
  // the reveal stage.
  val authors : List[IntExpr] = {
    val authorsLevel : IntVar = pick(default = Viewer.low)
      assume(isAuthor ==> (authorsLevel === Viewer.high));
      assume(((context~'status >= UserStatus.reviewerL) &&
             (context~'stage >= PaperStage.authorReveal)) ==>
              (authorsLevel === Viewer.high))
    _authors.map(a => mkSensitive(authorsLevel, a))
  }

  private def mkSensitive(levelVar : IntVar, v : IntExpr) : IntExpr = {
    val isAuthor : Formula = CONTAINS(_authors, context~'name);
    assume(isAuthor ==> (levelVar === Viewer.high));
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
