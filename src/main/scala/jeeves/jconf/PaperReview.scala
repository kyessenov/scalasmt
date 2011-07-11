package cap.jeeves.jconf

/*
 * User records for jconf case study.
 * @author jeanyang
 */

import cap.scalasmt._

import JConfBackend._

class PaperReview(id: Int, reviewerV: ConfUser, var body: String, var score: Int) extends JeevesRecord {
  val reviewer = {
    val level = mkLevel();
    val vrole = CONTEXT.viewer.role;
    val isInternal = (vrole === ReviewerStatus) || (vrole === PCStatus)
    policy(level, isInternal, HIGH);
    policy(level, !isInternal, LOW);
    mkSensitive(level, reviewerV, NULL)
  }

  def updateBody (newbody: String) = body = newbody
  def updateScore (newscore: Int) = score = newscore
}
