package cap.scalasmt


object DatabaseLib {
  /* Setting this up. */
  def connectToDatabase() = throw Undefined
  def makeTable() = throw Undefined

  def putField[T] (field : String, item : T) : Unit = throw Undefined
  def getField[T] (field : String) : Option[T] = throw Undefined
}
