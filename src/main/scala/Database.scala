package cap.scalasmt

import cap.scalasmt.JeevesLib._
import scala.collection.mutable.{Map => MMap}
import RelExpr._

class Database {
  object KeyException extends RuntimeException("key not in db")
  private var elements = MMap.empty[BigInt, AnyRef];
  
  /* Setting this up. */
  def initialize () : Unit =
    // TODO: Check for cache file of serialized database.
    throw Undefined

  def putEntry (key : BigInt, item : AnyRef) : Unit = elements.put(key, item)
  def getEntry (key : IntExpr) : AnyRef =
    key match {
      case Constant(k) =>
        elements.get(k) match {
          case Some(record) => record
          case None => throw KeyException
        }
      case IntVar(k) =>
        val r : AtomVar = pickAtom;
        elements foreach {
          case (curkey, v) => assume((key === curkey) ==> (r === v))
        }
        r
    }
}
