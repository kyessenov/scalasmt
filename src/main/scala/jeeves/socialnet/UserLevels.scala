package cap.jeeves.socialnet

import cap.jeeves.JeevesLib._

object UserLevels {
  val defaultL: LevelTy = 0;
  val friendsL: LevelTy = 1;
  val selfL: LevelTy    = 2;
  val levels = selfL :: friendsL :: defaultL :: Nil;
}
