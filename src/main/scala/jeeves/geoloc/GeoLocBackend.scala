package cap.jeeves.geoloc

import scala.collection.mutable.HashMap;
import scala.collection.mutable.Set;

import cap.scalasmt._
import cap.jeeves._
import JeevesLib._

object GeoLocBackend {
  private val __db = new Database[GeoUser]();
  val context : AtomVar = pickAtom();

  /*
  def getUser (uname : BigInt) : UserRecord = {
    // We know that this is going to be concrete.
    val result = (__db.getEntry(uname)).eval
    result.asInstanceOf[UserRecord];
  }

  def addFriend ( user1 : BigInt, user2 : BigInt) : Unit = {
    val record1 = getUser(user1)
    val record2 = getUser(user2)

    record1.addFriend(record2.username);
    record2.addFriend(record1.username);
  }
  */

  /*************************************************/
  /* Functions that use concretize to show things. */
  /*************************************************/
  /*
  def getBool(ctxtUser : BigInt, b : Formula) : Boolean = {
    val ctxt = getUser(ctxtUser);
    concretize(snbContext, ctxt, b)
  }

  def getConcreteRecordList (ctxtUser : BigInt, lst : List[ObjectExpr])
    : List[UserRecord] = {
    val ctxt = getUser(ctxtUser);
    concretizeList(snbContext, ctxt, lst);
  }

  def printStringList (ctxtUser : BigInt, lst : List[IntExpr]) : List[String]= {
    val ctxt = getUser(ctxtUser);
    lst.map(x => asString(concretize(snbContext, ctxt, x)));
  }
  */
}
