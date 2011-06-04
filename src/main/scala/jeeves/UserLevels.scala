package cap.jeeves

object UserLevels {
  val defaultL = JeevesLib.default;
  val friendsL = JeevesLib.getNewLevel();
  val selfL = JeevesLib.getNewLevel()
  val levels = List(selfL, friendsL, defaultL);
}
