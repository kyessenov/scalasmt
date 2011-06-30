package cap.jeeves.jconf

/*
 * User records for jconf case study.
 * @author jeanyang
 */

import cap.scalasmt._
import scala.collection.mutable.Map;
import cap.jeeves.JeevesLib._

object PaperStage {
  val submission    : LevelTy = 0;
  val review        : LevelTy = 1;
  val authorReveal  : LevelTy = 2;
  val publicReveal  : LevelTy = 3;
}

case class ConfContext(
  id : BigInt, name : BigInt, status : BigInt, stage : BigInt ) extends Atom
