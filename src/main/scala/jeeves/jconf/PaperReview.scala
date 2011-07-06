package cap.jeeves.jconf

/*
 * User records for jconf case study.
 * @author jeanyang
 */

import cap.scalasmt._

import JConfBackend._

case class Body(val text: String) extends JeevesRecord;
case class ReviewBody (
  val body: Body, val score: BigInt, val confidence: BigInt)
  extends JeevesRecord;

class PaperReview( id : Int
                 , _reviewer: ConfUser
                 , _rtext: String, _score: Int, _conf: Int
                 , _isAuthor : Formula ) extends JeevesRecord {
  private val Anonymous = new ConfUser(Name("Anonymous"), ReviewerStatus)
  private val DefaultBody = ReviewBody(null, -1, -1)

  private val isInternal : Formula =
    (CONTEXT/'status === ReviewerStatus) || (CONTEXT/'status === PCStatus)

  // Restrict reviewer to only be seen by internal people.
  val reviewer = {
    val level = mkLevel();
    policy(level, isInternal);
    mkSensitiveObject(level, _reviewer, Anonymous);
  }

  val review : ReviewBody = ReviewBody(Body(_rtext), _score, _conf)
}
