package cap.jeeves.jconf

/*
 * User records for jconf case study.
 * @author jeanyang
 */

import cap.scalasmt._
import scala.collection.mutable.Map;
import cap.jeeves.JeevesLib._

case class ConfContext( viewer : ConfUser
                      , status : BigInt
                      , stage  : PaperStage ) extends JeevesRecord
