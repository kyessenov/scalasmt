package cap.jeeves.jconf

/*
 * User records for jconf case study.
 * @author jeanyang
 */

import cap.scalasmt._

import JConfBackend._

class PaperReview( id : Int
                 , _reviewer: ConfUser
                 , val body: String, val score: Int) extends JeevesRecord {
  private val Anonymous = new ConfUser(Name("Anonymous"), ReviewerStatus)

  // Restrict reviewer to only be seen by internal people.
  val reviewer = {
    val level = mkLevel();
    val isInternal : Formula = {
      val vrole = CONTEXT.viewer.role;
      (vrole === ReviewerStatus) || (vrole === PCStatus)
    }
    policy(level, isInternal, HIGH);
    policy(level, !isInternal, LOW);
    mkSensitiveObject(level, _reviewer, Anonymous);
  }
}
