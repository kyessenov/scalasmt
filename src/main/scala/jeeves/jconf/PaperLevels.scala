package cap.jeeves.jconf

import cap.jeeves.JeevesLib._

object PaperLevels {
  val paperDefaultL : LevelTy = 0;
  val authorL       : LevelTy = 1;
  val reviewerL     : LevelTy = 2;
  val pcL           : LevelTy = 3;
  val levels = List(paperDefaultL, authorL, reviewerL, pcL)
}
