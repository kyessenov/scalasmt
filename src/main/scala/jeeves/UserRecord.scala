package cap.jeeves

/*
 * User records for jeeves social net case study.
 * @author jeanyang
 */

import cap.scalasmt._
import scala.collection.mutable.Map;
import UserLevels._
import JeevesLib._

/* NOTE: We will not be using this with beans for now... */
class UserRecord( uname : Int
                , name : IntExpr, namep : LevelTy
                , pwd : IntExpr, pwdp : BigInt
                , username : IntExpr, usernamep : BigInt
                , email : IntExpr, emailp : BigInt
                , network : IntExpr, networkp : BigInt
                , friends : List[IntExpr], friendsp : BigInt
                , context : AtomVar ) extends Atom {
  private val __level : IntVar = pick;
  private val __context : AtomVar = context;

  /* Invariant: The variables are always symbolic expressions kept up to date
     with the permission. */
  private val __realuname = uname;

  private val __name : IntVar = mkSensitive(name, namep);
  private val __namep = namep;

  private val __pwd = mkSensitive(pwd, pwdp);
  private val __pwdp = pwdp;

  private val __username = mkSensitive(username, usernamep);
  private val __usernamep = usernamep;

  private val __email = mkSensitive(email, emailp);
  private val __emailp = emailp;

  private val __network = mkSensitive(network, networkp);
  private val __networkp = networkp;

  private var __friends =
    friends.map(friend => mkSensitive(friend, friendsp));
  private val __friendsp = friendsp
 
  /* Set privacy levels. */
  assume((__username === (__context ~ '__username)) ==> (__level === selfL));
  assume((!(__level === selfL) && this.isFriends(__context ~ '__username)) ==>
          (__level === friendsL));

  private def mkSensitive (v : IntExpr, p : BigInt) =
    mkSensitiveValue(UserLevels.levels, __context, v, p);

  /* Define getters and setters. */
  def getUname () : Int = __realuname
  def getName () : IntExpr = __name
  def getPwd () : IntExpr = __pwd
  def getUsername () : IntExpr = __username
  def getEmail () : IntExpr = __email
  def getNetwork () : IntExpr = __network
  def getFriends () : List[IntExpr] = __friends
  def isFriends(u : IntExpr) : Formula = { 
    u match {
      case Constant(c) => __friends.contains(c)
      case other =>
      val isFriends = pickBool;
      __friends.foreach {
        case friend =>
          assume (__context ~ '__username === friend ==> isFriends === true);
      }
      isFriends
    }
  }

  def addFriend (user : IntExpr) {
    val newfriend = pick(x => x === user); //mkSensitive(user, friendsp);
    __friends = newfriend :: __friends
  }

  def getContext () : AtomVar = __context
  def getLevel () : IntVar = __level

  def equals (other : UserRecord) : Boolean = {
    (__realuname == other.getUname) && (__name == other.getName()) &&
    (__pwd == other.getPwd()) && (__username == other.getUsername()) &&
    (__email == other.getEmail()) && (__network == other.getNetwork()) &&
    (__friends == other.getFriends()) &&
    (__context == other.getContext())
  }
}
