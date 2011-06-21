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
  def addUser ( name : String, pwd : String, username : String, email : String
              , status : BigInt )
            : ConfUser = {
    val iName = fromString(name);
    val iPwd = fromString(pwd);
    val iUsername = fromString(username);
    val iEmail = fromString(email);

    val user =
      new ConfUser(iUsername, iName, iPwd, iEmail, status, jcContext);
    __userDB.putEntry(iUsername, user);
    return user
  }
  def getUser (uname : BigInt) : ConfUser = {
    // We know that this is going to be concrete.
    val result = (__userDB.getEntry(uname)).eval
    result.asInstanceOf[ConfUser];
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
