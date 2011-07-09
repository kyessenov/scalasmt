package cap.jeeves.jconf

/*
 * User records for jconf case study.
 * @author jeanyang
 */

import cap.scalasmt._
import scala.collection.mutable.Map;

import JConfBackend._

case class ConfContext( viewer : ConfUser
                      , stage  : PaperStage ) extends JeevesRecord
