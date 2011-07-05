package cap.jeeves.jconf

import scala.collection.mutable.HashMap;
import scala.collection.mutable.Set;

import cap.scalasmt._
import cap.jeeves._

object JConfBackend extends JeevesLib {
//  private val __userDB : Map[BigInt, ConfUser] = Map();
  private val papers : List[PaperRecord] = Nil;

  /* Database functions. */

  /*
  def getUser (uname : BigInt) : ConfUser = {
    __userDB.getEntry(uname)
  }
  def getPaper (paperName : BigInt) : PaperRecord = {
    val result = (__paperDB.getEntry(paperName)).eval
    result.asInstanceOf[PaperRecord]
  }
  */

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

  def getPaperByTag (tag : PaperTag) : List[Symbolic] = {
    filter(papers, (p : PaperRecord) => CONTAINS(p.tags, tag))
  }

  /*************************************************/
  /* Functions that use concretize to show things. */
  /*************************************************/
  /*
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
  */
}
