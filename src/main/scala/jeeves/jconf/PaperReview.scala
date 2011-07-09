package cap.jeeves.jconf

/*
 * User records for jconf case study.
 * @author jeanyang
 */

import cap.scalasmt._

import JConfBackend._

case class Body(val text: String) extends JeevesRecord;
class ReviewBody ( _body: Body
                 , val score: BigInt
                 , _confidence: BigInt
                 , _isAuthor: Formula)
  extends JeevesRecord {
  private val isInternal : Formula = CONTEXT.status >= UserStatus.reviewerL

  val rlevel = mkLevel();
  policy(rlevel, isInternal);
  policy(rlevel, _isAuthor &&
                (CONTEXT.stage === Rebuttal || CONTEXT.stage === Decision));

  val body = mkSensitiveObject(rlevel, _body, NULL)
  val confidence = mkSensitiveInt(rlevel, _confidence, -1);
}

class PaperReview( id : Int
                 , _reviewer: ConfUser
                 , _rtext: String, _score: Int, _conf: Int
                 , _isAuthor : Formula ) extends JeevesRecord {
  private val Anonymous = new ConfUser(Name("Anonymous"), UserStatus.reviewerL)
  private val DefaultBody = new ReviewBody(null, -1, -1, _isAuthor)

  private val isInternal : Formula = CONTEXT.status >= UserStatus.reviewerL

  // Restrict reviewer to only be seen by internal people.
  val reviewer = {
    val level = mkLevel();
    policy(level, isInternal);
    mkSensitiveObject(level, _reviewer, Anonymous);
  }

  val review : ReviewBody =
    new ReviewBody(Body(_rtext), _score, _conf, _isAuthor)
}
