package cap.scalasmt

import scala.collection.mutable.HashMap;

object SocialNetBackend extends Sceeves {
  private val __db = new Database[UserRecord]();

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

  /* HashMap for List[IntVar] <-> BigInt */
  private val __lsts = new HashMap[BigInt, List[IntVar]]();
  private var __ivarCount : BigInt = 0;
  private def storeIVarList (str : List[IntVar]) : BigInt = {
    val idx = __ivarCount;
    __lsts += (idx -> str);
    __ivarCount = __ivarCount + 1;
    idx
  }
  private def asIVarList (v : BigInt) : List[IntVar] =
    __lsts.get(v) match {
      case Some(str) => str
      case None => throw Undefined
    }

  private val context : IntVar = pick(_ => true);

  /* Privacy levels--introduce delegated variable for levels and then define
     variables for each level. */
  private val level : IntVar = pick(_ => true);
  val self = JeevesLib.getLevel()
  val friends = JeevesLib.getLevel();
  val default = JeevesLib.default;
  private val levels = List(self, friends, JeevesLib.default);

  /* Define relationship between privacy levels. */
  private def setPrivacyLevels () : Unit = {
    assume((context === (1 : BigInt)) ==> (level === friends));
  }

  /* Initialization function. */
  def initialize () : Unit = {
    setPrivacyLevels();
  }

  /* Database functions. */
  def addUser ( uname     : String
              , name      : List[String]
              , pwd       : List[String]
              , username  : List[String]
              , email     : List[String]
              , network   : List[String]
              , friends   : List[List[IntVar]] )
            : Unit = {
    val iUname = storeString(uname);
    val iName = name.map(storeString);
    val iPwd = pwd.map(storeString);
    val iUsername = username.map(storeString);
    val iEmail = email.map(storeString);
    val iNetwork = network.map(storeString);
    val iFriends = friends.map(storeIVarList);

    val user =
      new UserRecord( iUname, iName, iPwd, iUsername, iEmail, iNetwork
                    , iFriends
                    , context, levels);
    __db.putEntry(iUname, user);
  }

  /* Define functions the backend supports. */
  def getFriends (ctxt : IntVar, user : BigInt) : List[BigInt] = {
    val curRecord = __db.getEntry(user);
    val curFriends = curRecord.getFriends();
    val outputFriendsBigInt = concretize(context, ctxt, curFriends);
    val friendsList = asIVarList(outputFriendsBigInt);
    friendsList.map(x => concretize(context, ctxt, x))
  }
}
