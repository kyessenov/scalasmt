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

/* Conference User */
case class Name (name : String) extends JeevesRecord
case class ConfUser( val name : Name, val status : UserStatus ) extends JeevesRecord
