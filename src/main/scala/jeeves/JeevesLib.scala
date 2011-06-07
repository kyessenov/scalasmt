package cap.jeeves

/*
 * A library for using ScalaSMT for privacy, using symbolic varaibles to
 * represent sensitive values.
 * @author jeanyang
 */

import cap.scalasmt._
import scala.collection.immutable.Map;
import scala.collection.mutable.{Map => MMap};

object Undefined extends RuntimeException("undefined")
object JeevesLib extends Sceeves {
  type LevelTy = BigInt;
  type ValueTy = BigInt;
  type SensitiveMap = Map[BigInt, IntExpr];

  // Keep map of privacy levels.
  private var plevel_count : LevelTy = 1;
  
  val default : LevelTy = 0;
  def getNewLevel () : LevelTy = {
    val plevel_old = plevel_count
    plevel_count = plevel_count + 1;
    plevel_old
  }

  // Creates a sensitive map based on a value.
  def addCoarsePolicy (levels : List[LevelTy], v : IntExpr, minLevel : LevelTy)
    : Map[BigInt, IntExpr] = {
    levels.foldLeft (Map.empty[BigInt, IntExpr]) (
        (map : SensitiveMap, level : LevelTy) =>
          if (level < minLevel) {
            map + (level -> Constant(-1))
          }
          else {
            map + (level -> v)
          }
        )
  }

  def mkSensitiveValue (
      levels : List[LevelTy], level : IntVar, v : IntExpr
    , minLevel : BigInt)
  : IntVar = {
    val map = addCoarsePolicy(levels, v, minLevel);
    createSensitiveValue(level, map)
  }

  // Associates a constraint with a field.
  def createSensitiveValue
    (level : IntVar, vals : SensitiveMap) : IntVar = {
    var x = pick;

    // See if there is a default.
    /*
    val defaultVal =  vals.get(default);
    defaultVal match {
      case Some(v) => { x = pick; }
      case None => { } // Do nothing for now.
    }
    */

    // Go through keys and values.
    vals foreach {
      case (keyval, valConstraint) =>
        assume((level === keyval) ==> (x === valConstraint))
    }
    x
  }
}
