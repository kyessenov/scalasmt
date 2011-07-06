package cap.jeeves.jconf

/*
 * User records for jconf case study.
 * @author jeanyang
 */

import cap.scalasmt._

import JConfBackend._

/* Conference User */
case class ReviewBody (body : String, score : Int, confidence : Int)
  extends JeevesRecord;
class PaperReview( id : Int
                 , _reviewer : ConfUser
                 , _review : ReviewBody
                 , _isAuthor : Formula ) extends JeevesRecord {
  private val Anonymous = new ConfUser(Name("Anonymous"), UserStatus.reviewerL)
  private val DefaultBody = ReviewBody("", -1, -1)

  private val isInternal : Formula = CONTEXT~'STATUS >= UserStatus.reviewerL

  val reviewer = {
    val level = mkLevel();
    policy(level, isInternal);
//    policy(level, CONTEXT/'stage === Decision || CONTEXT/'stage === Rebuttal);
    mkSensitiveObject(level, _reviewer, Anonymous);
  }

  val review = {
    val level = mkLevel();
    policy(level, isInternal);
    policy(level, _isAuthor &&
                  (CONTEXT/'stage === Rebuttal || CONTEXT/'stage === Decision));
    mkSensitiveObject(level, _reviewer, DefaultBody);
  }
}
