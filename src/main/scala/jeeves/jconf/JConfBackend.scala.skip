package cap.jeeves.jconf

import scala.collection.mutable.HashMap;
import scala.collection.mutable.Set;

import cap.scalasmt._
import cap.jeeves._
import JeevesLib._

object JConfBackend {
  private val __userDB = new Database[ConfUser]();
  private val __paperDB = new Database[PaperRecord]();

  val jcContext : ObjectVar = pickObject();

  /* Database functions. */
  def addUser ( name : String, username : String, email : String
              , status : BigInt )
            : ConfUser = {
    val iName = fromString(name);
    val iUsername = fromString(username);
    val iEmail = fromString(email);

    val user =
      new ConfUser(iUsername, iName, iEmail, status, jcContext);
    __userDB.putEntry(iUsername, user);
    return user
  }
  def getUser (uname : BigInt) : ConfUser = {
    // We know that this is going to be concrete.
    val result = (__userDB.getEntry(uname)).eval
    result.asInstanceOf[ConfUser];
  }
  def getPaper (paperName : BigInt) : PaperRecord = {
    val result = (__paperDB.getEntry(paperName)).eval
    result.asInstanceOf[PaperRecord]
  }

  /* Search by... */
  /*
  def getFriendNetworks (user : BigInt) : List[IntExpr] = {
    val friends = getFriends(user);
    val networks = friends.foldLeft (Set.empty[IntExpr]) (
        (set : Set[IntExpr], friend : IntExpr) =>
        set + (__db.getEntry(friend))~'network)
    networks.toList
  }
  */

  def getPaperByTag (tag : BigInt) : List[ObjectExpr] = {
    val f = (x : PaperRecord) => (CONTAINS(x.tags, tag));
    __paperDB.findEntry(f);
  }

  /*************************************************/
  /* Functions that use concretize to show things. */
  /*************************************************/
  def getBool(ctxtUser : BigInt, b : Formula) : Boolean = {
    val ctxt = getUser(ctxtUser);
    concretize(jcContext, ctxt, b)
  }

  def getConcreteRecordList (ctxtUser : BigInt, lst : List[ObjectExpr])
    : List[ConfUser] = {
    val ctxt = getUser(ctxtUser);
    concretizeList(jcContext, ctxt, lst);
  }

  def printStringList (ctxtUser : BigInt, lst : List[IntExpr]) : List[String]= {
    val ctxt = getUser(ctxtUser);
    lst.map(x => asString(concretize(jcContext, ctxt, x)));
  }
}
