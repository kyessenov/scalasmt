package cap.jeeves

import JeevesLib._

object UserLevels {
  val defaultL: LevelTy = default;
  assert (default == 0)
  val friendsL: LevelTy = 1;
  val selfL: LevelTy    = 2;
  val levels = selfL :: friendsL :: defaultL :: Nil;
}
