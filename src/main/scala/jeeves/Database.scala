package cap.jeeves

/*
 * A library for accessing a database of records that could be symbolic.
 * @author jeanyang
 */

import cap.scalasmt._
import JeevesLib._
import scala.collection.mutable.{Map => MMap}

object KeyException extends RuntimeException("key not in db")

class Database {
  private var elements = MMap.empty[BigInt, Atom];
  
  /* Setting this up. */
  def initialize () : Unit =
    // TODO: Check for cache file of serialized database.
    throw Undefined

  def putEntry (key : BigInt, item : Atom) : Unit = elements.put(key, item)
  def getEntry (key : IntExpr) : ObjectExpr =
    key match {
      case Constant(k) =>
        elements.get(k) match {
          case Some(record) => record
          case None => throw KeyException
        }
      case _ =>
        val r : AtomVar = pickAtom();
        elements foreach {
          case (curkey, v) => assume((key === curkey) ==> (r === v))
        }
        r
    }
}
