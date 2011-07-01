package cap.jeeves.jconf

/*
 * User records for jconf case study.
 * @author jeanyang
 */

import cap.scalasmt._
import scala.collection.mutable.Map;
import cap.jeeves.JeevesLib._

object UserStatus {
  val authorL  : BigInt = 0
  val reviewerL : BigInt = 1
  val pcL      : BigInt = 2
}

/* Conference User */
class ConfUser( val id : BigInt
              , _name : IntExpr, _email : IntExpr
              , val status : BigInt, val context : ObjectVar ) extends Atom {
    private val isSelf : Formula = context~'id === id;

    val name = {
    val level = mkLevel();
    // Reviewers can see names after authors are revealed 
    policy( level, isSelf, Viewer.high);
    policy( level
          , (context~'status >= UserStatus.reviewerL) &&
            (context~'stage >= PaperStage.decision)
          , Viewer.high);
    mkSensitiveValue(level, _name);
  }
  val email = {
    val level = mkLevel()
    policy(level, isSelf, Viewer.high);
    policy(level, context~'status === UserStatus.pcL, Viewer.high);
    mkSensitiveValue(level, _email);
  }

  override def toString = "jcu" + id
  override def hashCode = id.toInt
  override def equals(that: Any) =
    if (that.isInstanceOf[ConfUser])
      (id == that.asInstanceOf[ConfUser].id) 
    else 
      false
}
