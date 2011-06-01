package cap.scalasmt

import scala.collection.mutable.HashMap;
import scala.collection.mutable.Set;

object SocialNetBackend extends Sceeves {
  private var __uid_count = 0;

  private var __init = false;
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

  private val context : IntVar = pick(_ => true);

  /* Privacy levels--introduce delegated variable for levels and then define
     variables for each level. */
  private val level : IntVar = pick(_ => true);
  val default = JeevesLib.default;
  val friends = JeevesLib.getLevel();
  val self = JeevesLib.getLevel()
  private val levels = List(self, friends, JeevesLib.default);
  def getLevel (contextVal : BigInt)  : BigInt =
    concretize(context, contextVal, level)

  /* Define relationship between privacy levels. */
  private def setPrivacyLevels () : Unit = {
    assume((context === (1 : BigInt)) ==> (level === friends));
  }

  /* Initialization function. */
  def initialize () : Unit = {
    if (!__init) {
      setPrivacyLevels();
      __init = false;
    }
  }

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
                    , context, levels );
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

    record1.addFriend(record1.getName());
    record2.addFriend(record2.getName());
  }

  /* Define functions the backend supports. */
  def getFriends (user : Int) : List[IntExpr] = {
    val curRecord = __db.getEntry(user).asInstanceOf[UserRecord];
    curRecord.getFriends();
  }

  def getFriendNetworks (user : Int) : List[IntExpr] = {
    val friends = getFriends(user);
    /* The problem is we need to concretize the networks in the output
     * context... */
/*    val networks = friends.foldLeft (Set.empty[IntExpr]) (
        (set : Set[IntExpr], friend : IntExpr) =>
        set + __db.getEntry(friend))
    networks.toList */
    throw Undefined
  }

  /* Functions that show things. */
  def printList (ctxt : IntVar, lst : List[IntExpr]) = {
    val elts = lst.map(x => concretize(context, ctxt, x));
    elts.foreach(x => println(x));
  }
}
