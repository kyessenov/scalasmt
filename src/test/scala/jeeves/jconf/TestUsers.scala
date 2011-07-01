package test.cap.jeeves.jconf

import cap.jeeves._
import cap.jeeves.jconf._
import cap.scalasmt._
import org.scalatest.FunSuite
import scala.collection.immutable.Map
import scala.util.Random

import JeevesLib._

object TestUsers {
  var uids = 0;
  private def newUid () : BigInt = {
    val uid = uids;
    uids = uids + 1;
    uid
  }

  private def random = new Random()
  private def genField() : BigInt = {
    random.nextInt () % 8
  }

  def mkUser(userStatus : BigInt, context : ObjectVar)
    : Tuple3[BigInt, BigInt, ConfUser] = {
    val name = genField ();
    val email = genField ();
    val user = new ConfUser(newUid(), name, email, userStatus, context);
    (name, email, user)
  }

//  def mkPaper(
}
