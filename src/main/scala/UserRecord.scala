package cap.scalasmt

import scala.collection.mutable.Map;

import cap.scalasmt.JeevesLib._

/* NOTE: We will not be using this with beans for now... */
class UserRecord( uname : Int
                , name : IntExpr, namep : LevelTy
                , pwd : IntExpr, pwdp : BigInt
                , username : IntExpr, usernamep : BigInt
                , email : IntExpr, emailp : BigInt
                , network : IntExpr, networkp : BigInt
                , friends : List[IntExpr], friendsp : BigInt
                , context : IntVar
                , levels : List[BigInt] ) {
  private val __context = context;
  private val __plevels = levels;

  /* Invariant: The variables are always symbolic expressions kept up to date
     with the permission. */
  private var __realuname = uname;

  private var __name = mkSensitive(name, namep);
  private var __namep = namep;

  private var __pwd = mkSensitive(pwd, pwdp);
  private var __pwdp = pwdp;

  private var __username = mkSensitive(username, usernamep);
  private var __usernamep = usernamep;

  private var __email = mkSensitive(email, emailp);
  private var __emailp = emailp;

  private var __network = mkSensitive(network, networkp);
  private var __networkp = networkp;

  private var __friends =
    friends.map(friend => mkSensitive(friend, friendsp));
  private var __friendsp = friendsp
 
  private def mkSensitive (v : IntExpr, p : BigInt) =
    mkSensitiveValue(__plevels, __context, v, p);

  /* Define getters and setters. */
  def getUname () : Int = __realuname
  def getName () : IntExpr = __name
  def getPwd () : IntExpr = __pwd
  def getUsername () : IntExpr = __username
  def getEmail () : IntExpr = __email
  def getNetwork () : IntExpr = __network
  def getFriends () : List[IntExpr] = __friends

  def setName (n : BigInt) : Unit = __name = mkSensitive(n, namep)
  def setNameP (np : LevelTy) : Unit = {
    __namep = np;
    __name = mkSensitive(__name, __namep)
  }
  def setPwd (pwd : BigInt) : Unit = __pwd = mkSensitive(pwd, pwdp)
  // TODO: More here...
  def addFriend (user : IntExpr) {
    val newfriend = mkSensitive(user, friendsp);
    __friends = newfriend :: __friends
  }

  def getContext () : IntVar = __context
  def getLevels () : List[BigInt] = __plevels

  def equals (other : UserRecord) : Boolean = {
    (__realuname == other.getUname) && (__name == other.getName()) &&
    (__pwd == other.getPwd()) && (__username == other.getUsername()) &&
    (__email == other.getEmail()) && (__network == other.getNetwork()) &&
    (__friends == other.getFriends()) &&
    (__context == other.getContext()) && (__plevels == other.getLevels())
  }
}
