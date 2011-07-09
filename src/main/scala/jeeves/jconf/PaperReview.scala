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

  private val isInternal : Formula =
    (CONTEXT.viewer.status === ReviewerStatus) || 
    (CONTEXT.viewer.status === PCStatus)

  // Restrict reviewer to only be seen by internal people.
  val reviewer = {
    val level = mkLevel();
    policy(level, isInternal, HIGH);
    policy(level, !isInternal, LOW);
    mkSensitiveObject(level, _reviewer, Anonymous);
  }
}
