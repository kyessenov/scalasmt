package cap.jeeves

import scala.collection.mutable.HashMap;
import scala.collection.mutable.Set;

import cap.scalasmt._
import JeevesLib._

object SocialNetBackend {
  private var __uid_count = 0;

  private val __db = new Database();

  /* HashMap for String <-> BigInt */
  private val __strs = new HashMap[BigInt, String]();
  private var __strCount : BigInt = 0;
  private def storeString (str : String) : BigInt = {
    val idx = __strCount;
    __strs += (idx -> str);
    __strCount = __strCount + 1;
    idx
  }
  private def asString (v : BigInt) : String =
    __strs.get(v) match {
      case Some(str) => str
      case None => throw Undefined
    }

  private val context : AtomVar = pickAtom;

  /* Database functions. */
  def addUser ( name      : String      , namep     : BigInt
              , pwd       : String      , pwdp      : BigInt
              , username  : String      , usernamep : BigInt
              , email     : String      , emailp    : BigInt
              , network   : String      , networkp  : BigInt
              , friends   : List[IntVar], friendsp  : BigInt )
            : Unit = {
    val uname = __uid_count;
    __uid_count = __uid_count + 1;
    
    val iName = storeString(name);
    val iPwd = storeString(pwd);
    val iUsername = storeString(username);
    val iEmail = storeString(email);
    val iNetwork = storeString(network);
    val iFriends = friends;

    val user =
      new UserRecord( uname
                    , iName, namep
                    , iPwd, pwdp
                    , iUsername, usernamep
                    , iEmail, emailp
                    , iNetwork, networkp
                    , iFriends, friendsp
                    , context );
    __db.putEntry(uname, user);
  }
  def getUser (uname : BigInt) : UserRecord = {
    __db.getEntry(uname).asInstanceOf[UserRecord];
  }

  /* What about the integrity of this data?  Is user1 allowed to become user2's
   * friend? */
  def addFriend ( user1 : BigInt, user2 : Int) : Unit = {
    val record1 = __db.getEntry(user1).asInstanceOf[UserRecord];
    val record2 = __db.getEntry(user2).asInstanceOf[UserRecord];

    record1.addFriend(record1.getName(), user1);
    record2.addFriend(record2.getName(), user2);
  }

  /******************************************/
  /* Define functions the backend supports. */
  /******************************************/
  /* This function demonstrates how we can get fields of objects without
   * worrying about permissions. */
  def getFriends (user : Int) : List[IntExpr] = {
    val curRecord = __db.getEntry(user).asInstanceOf[UserRecord];
    curRecord.getFriends();
  }

  /* This function demonstrates how we can work with symbolic objects and do
   * additional operations on them without worrying about permissions. */
  def getFriendNetworks (user : Int) : List[IntExpr] = {
    val friends = getFriends(user);
    val networks = friends.foldLeft (Set.empty[IntExpr]) (
        (set : Set[IntExpr], friend : IntExpr) =>
        set + (__db.getEntry(friend)) ~ '__network)
    networks.toList
  }

  /*************************************************/
  /* Functions that use concretize to show things. */
  /*************************************************/
  def printList (ctxt : AtomVar, lst : List[IntExpr]) = {
    // val elts = lst.map(x => concretize(context, ctxt, x));
    // elts.foreach(x => println(x));
  }
}
