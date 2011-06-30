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
              , _name : IntExpr, _pwd : IntExpr, _email : IntExpr
              , val status : BigInt, val context : ObjectVar ) extends Atom {

  val name = {
    val level = mkLevel();
    // Reviewers can see names after authors are revealed 
    policy( level
          , (context~'status >= UserStatus.reviewerL) &&
            (context~'stage >= PaperStage.authorReveal)
          , Viewer.high);
    mkSensitive(level, _name);
  }
  // Password is always sensitive
  val pwd = {
    val level = mkLevel()
    mkSensitive(level, _pwd);
  }
  val email = {
    val level = mkLevel()
    policy(level, context~'status === UserStatus.pcL, Viewer.high);
    mkSensitive(level, _email);
  }

  private def mkSensitive(levelVar : IntVar, v : IntExpr) : IntExpr = {
    policy(levelVar, context~'id === id, Viewer.high);
    mkSensitiveValue(levelVar, v)
  }

  override def toString = "jcu" + id
  override def hashCode = id.toInt
  override def equals(that: Any) =
    if (that.isInstanceOf[ConfUser])
      (id == that.asInstanceOf[ConfUser].id) 
    else 
      false
}
