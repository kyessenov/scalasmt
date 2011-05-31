package cap.scalasmt

import scala.collection.mutable.Map;

/* NOTE: We will not be using this with beans for now... */
class UserRecord( uname : BigInt, name : List[BigInt], pwd : List[BigInt]
                , username : List[BigInt], email : List[BigInt]
                , network : List[BigInt]
                , friends : List[BigInt]
                , context : IntVar, levels : List[BigInt]) {
  private var __realuname = uname;

  private var __name = name;
  private var __namep = mkSensitiveValue(__name)

  private var __pwd = pwd;
  private var __pwdp = mkSensitiveValue(__pwd);

  private var __username = username;
  private var __usernamep = mkSensitiveValue(__username);

  private var __email = email;
  private var __emailp = mkSensitiveValue(__email);

  private var __network = network;
  private var __networkp = mkSensitiveValue(__network)

  private var __friends = friends;
  private var __friendsp = throw Undefined

  private val __context = context;
  private val __plevels = levels;
 
  /* Define getters and setters. */

  /* Getters need to have a policy. */
  private def mkSensitiveValue (vals : List[BigInt]) : IntVar = {
    JeevesLib.createSensitiveValue(context, (levels.zip(vals)).toMap)
  }

  def getUname () : BigInt = __realuname
  def getName () : IntVar = __namep
  def getPwd () : IntVar = __pwdp
  def getUsername () : IntVar = __usernamep;
  def getEmail () : IntVar = __emailp;
  def getNetwork () : IntVar = __networkp
  def getFriends () : IntVar = __friendsp

  def equals (other : UserRecord) : Boolean = {
    true
  }
}
