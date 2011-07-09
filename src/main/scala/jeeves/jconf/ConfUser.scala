package cap.jeeves.jconf

/*
 * User records for jconf case study.
 * @author jeanyang
 */

import cap.scalasmt._

import JConfBackend._

object UserStatus {
  val publicL   : BigInt = 0
  val authorL   : BigInt = 1
  val reviewerL : BigInt = 2
  val pcL       : BigInt = 3
}

/* Conference User */
case class Name (name : String) extends JeevesRecord
case class ConfUser( val name : Name, val status : BigInt ) extends JeevesRecord
