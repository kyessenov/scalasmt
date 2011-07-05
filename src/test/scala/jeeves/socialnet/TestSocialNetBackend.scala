package test.cap.jeeves

import cap.scalasmt._
import cap.jeeves._
import cap.jeeves.socialnet._
import SocialNetBackend._
import org.scalatest.FunSuite
import org.scalatest.Assertions.{expect}
import scala.collection.immutable.Map

class ExampleSocialNetBackend extends FunSuite {
  val MIT = Network("MIT");
  val jean = new UserRecord(Name("Jean Yang"), Friends, MIT, Friends, Friends)
  val kuat = new UserRecord(Name("Kuat Yessenov"), Friends, MIT, Self, Friends)
  val joe = new UserRecord(Name("Joe Near"), Self, MIT, Friends, Self)

  addUser(jean);
  addUser(kuat);
  addUser(joe);
  addFriend(jean, kuat);
  addFriend(joe, kuat);

  test ("name") {
    expect(null) { concretize(kuat, joe.name) }
    expect(null) { concretize(joe, jean.name) }
    expect(Name("Kuat Yessenov")) { concretize(jean, kuat.name) }
  }

  test ("getFriends") {
    expect(kuat :: Nil) {concretize(kuat, jean.getFriends())}
    expect(Nil) {concretize(joe, jean.getFriends())}
    expect(Nil) {concretize(kuat, joe.getFriends())}
  }

  test ("isFriends") {
    expect (true) { concretize(jean, jean.isFriends(kuat)) }
    expect (true) { concretize(kuat, jean.isFriends(kuat)) }
    expect (true) { concretize(joe, joe.isFriends(kuat)) }
    expect (true) { concretize(jean, kuat.isFriends(joe)) }
    expect (false) { concretize(jean, joe.isFriends(kuat)) }
  }

  test ("networks") {
    expect (MIT) {concretize(kuat, jean.network)}
    expect (null) {concretize(jean, kuat.network)}
    expect (jean :: Nil) {concretize(jean, getUsersByNetwork(MIT))}
  }
  
  test ("state change") {
    val eunsuk = new UserRecord(Name("Eunsuk Kang"), Anyone, MIT, Anyone, Anyone);
    expect (null) { concretize(eunsuk, joe.network)}
    addFriend(joe, eunsuk)
    expect (MIT) { concretize(eunsuk, joe.network)}
    removeFriend(joe, eunsuk)
    expect (null) { concretize(eunsuk, joe.network)} 
  }
}
