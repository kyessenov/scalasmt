package cap.jeeves.socialnet

import scala.collection.mutable.HashMap;
import scala.collection.mutable.Set;

import cap.scalasmt._
import cap.jeeves._
import JeevesLib._

object SocialNetBackend {
  private val __db = new Database[UserRecord]();
  val snbContext : AtomVar = pickAtom();

  /* Database functions. */
  def addUser ( name      : String      , namep     : BigInt
              , pwd       : String      , pwdp      : BigInt
              , username  : String      , usernamep : BigInt
              , email     : String      , emailp    : BigInt
              , network   : String      , networkp  : BigInt
              , friendsp  : BigInt )
            : UserRecord = {
    val iName = fromString(name);
    val iPwd = fromString(pwd);
    val iUsername = fromString(username);
    val iEmail = fromString(email);
    val iNetwork = fromString(network);

    val user =
      new UserRecord( iName, namep
                    , iPwd, pwdp
                    , iUsername, usernamep
                    , iEmail, emailp
                    , iNetwork, networkp
                    , friendsp
                    , snbContext );
    __db.putEntry(iUsername, user);
    return user
  }
  def getUser (uname : BigInt) : UserRecord = {
    // We know that this is going to be concrete.
    val result = (__db.getEntry(uname)).eval
    result.asInstanceOf[UserRecord];
  }

  /* What about the integrity of this data?  Is user1 allowed to become user2's
   * friend? */
  def addFriend ( user1 : BigInt, user2 : BigInt) : Unit = {
    val record1 = getUser(user1)
    val record2 = getUser(user2)

    record1.addFriend(record2.username);
    record2.addFriend(record1.username);
  }

  /******************************************/
  /* Define functions the backend supports. */
  /******************************************/
  /* This function demonstrates how we can get fields of objects without
   * worrying about permissions. */
  def getFriends (user : BigInt) : List[IntExpr] = {
    val curRecord = getUser(user);
    curRecord.friends
  }
  def isFriends(user1 : BigInt, user2 : BigInt) : Formula = {
    val u1 = getUser(user1);
    return u1.isFriends(user2)
  }

  /* This function demonstrates how we can work with symbolic objects and do
   * additional operations on them without worrying about permissions. */
  def getFriendNetworks (user : BigInt) : List[IntExpr] = {
    val friends = getFriends(user);
    val networks = friends.foldLeft (Set.empty[IntExpr]) (
        (set : Set[IntExpr], friend : IntExpr) =>
        set + (__db.getEntry(friend))~'network)
    networks.toList
  }

  def getUsersByNetwork (network : String) : List[ObjectExpr] = {
    val f = (x : ObjectExpr) => (x ~ 'network === fromString(network));
    __db.findEntry(f);
  }

  /* What if we wanted to have a series of operations on the friends of
   * symbolic friends? */
  /*
  def getFriendsOfFriends (user : BigInt) : List[IntExpr] = {
    // This is still concrete.
    val friends = getFriends(user);
    val networks = friends.foldLeft (Set.empty[IntExpr]) 
    friends
  }
  */

  /*************************************************/
  /* Functions that use concretize to show things. */
  /*************************************************/
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
}
