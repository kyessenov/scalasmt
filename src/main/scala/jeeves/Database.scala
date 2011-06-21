package cap.jeeves

/*
 * A library for accessing a database of records that could be symbolic.
 * @author jeanyang
 */

import cap.scalasmt._
import JeevesLib._
import scala.collection.mutable.{Map => MMap}

object KeyException extends RuntimeException("key not in db")

class Database[T<:Atom] {
  private var elements = MMap.empty[BigInt, T];
  
  /* Setting this up. */
  def initialize () : Unit =
    // TODO: Check for cache file of serialized database.
    throw Undefined

  def putEntry (key : BigInt, item : T) : Unit = elements.put(key, item)
  def getEntry (key : IntExpr) : ObjectExpr =
    key match {
      case Constant(k) =>
        elements.get(k) match {
          case Some(record) => record
          case None => throw KeyException
        }
      case _ =>
        val r : ObjectVar = pickObject(default = NULL);
        elements foreach {
          case (curkey, v) => assume((key === curkey) ==> (r === v))
        }
        r
    }

  def findEntry (ffun : ObjectExpr => Formula) : List[ObjectExpr] = {
    var returnList : List[ObjectExpr] = Nil;
    elements foreach {
      case (curkey, v) => {
        val isEligible : Formula = ffun (v : T);
        isEligible match {
          case TrueF => returnList = v :: returnList
          case FalseF => ()
          case _ =>
            val r : ObjectVar = pickObject(default = NULL);
            assume (isEligible ==> (r === v))
            returnList = r :: returnList
        }
      }
    }
    returnList
  }
}
