package cap.jeeves.jconf

/*
 * User records for jconf case study.
 * @author jeanyang
 */

import cap.scalasmt._

import JConfBackend._

sealed trait UserStatus extends JeevesRecord
object PublicStatus extends UserStatus
object AuthorStatus extends UserStatus
object ReviewerStatus extends UserStatus
object PCStatus extends UserStatus

/*
trait UserID {
  var count = 0
  def genID() : String = {
    val curID = count;
    count = count + 1;
    "id" + curID.toString
  }
}
*/

/* Conference User */
case class Username (name: String) extends JeevesRecord
case class Name (name: String) extends JeevesRecord
case class Password (val pwd: String) extends JeevesRecord
case class ConfUser( val username: Username, val name: Name, _password: String
  , val role: UserStatus )
  extends JeevesRecord {
    val password: Symbolic = {
      val level = mkLevel ();
      policy (level, !(CONTEXT.viewer === this), LOW);
      mkSensitive(level, Password(_password), Password("default"))
    }
  }
