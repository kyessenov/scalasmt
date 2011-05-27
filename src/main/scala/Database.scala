package cap.scalasmt

import scala.collection.mutable.{Map => MMap}

class Database[R] {
  object KeyException extends RuntimeException("key not in db")
  private var elements = MMap.empty[BigInt, R];
  
  /* Setting this up. */
  def initialize () : Unit =
    // TODO: Check for cache file of serialized database.
    throw Undefined

  def putEntry (key : BigInt, item : R) : Unit = elements.put(key, item)
  def getEntry (key : BigInt) : R =
    elements.get(key) match {
      case Some(record) => record
      case None => throw KeyException
    }
  // TODO: Does this make sense?
  /*
  def getEntry (key : BigIntVar) : R =
    elements.find((k, v) => k === key) match {
      case Some(record) => record
      case None => throw KeyException
    }
  */
}
