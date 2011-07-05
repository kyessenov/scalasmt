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
class ConfUser( val name : IntExpr, val status : BigInt ) extends JeevesRecord;
//    private val isSelf : Formula = CONTEXT~'id === id;

    /*
    val name = {
    val level = mkLevel();
    // Reviewers can see names after authors are revealed 
    
    policy( level, isSelf);
    policy( level
          , (CONTEXT~'status >= UserStatus.reviewerL) &&
            (CONTEXT~'stage >= PaperStage.decision) );
    policy( level
          , (CONTEXT~'stage === PaperStage.public) && isAccepted );
    mkSensitiveInt(level, _name, -1);
  }
  */
