package cap.scalasmt

class DatabaseLib[T] () {
//  private var elements = new List[T]();

  /* Setting this up. */
  def initialize () : Unit =
    // TODO: Check for cache file of serialized database.

    throw Undefined

  def putField[T] (field : String, item : T) : Unit = throw Undefined
  def getField[T] (field : String) : Option[T] = throw Undefined
}
