package cap.jeeves.jconf

/*
 * User records for jconf case study.
 * @author jeanyang
 */

import cap.scalasmt._

import JConfBackend._

class PaperReview( id : Int
                 , _reviewer: ConfUser
                 , _body: String, _score: Int) extends JeevesRecord {
  // Restrict reviewer to only be seen by internal people.
  val reviewer = {
    val level = mkLevel();
    val isInternal : Formula = {
      val vrole = CONTEXT.viewer.role;
      (vrole === ReviewerStatus) || (vrole === PCStatus)
    }
    policy(level, isInternal, HIGH);
    policy(level, !isInternal, LOW);
    mkSensitive[ConfUser](level, _reviewer, NULL)
  }

  var body = _body
  def updateBody (newbody: String) = body = newbody
  var score = _score
  def updateScore (newscore: Int) = score = newscore
}
