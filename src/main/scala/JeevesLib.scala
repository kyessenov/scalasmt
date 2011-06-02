package cap.scalasmt

/*
 * A library for using ScalaSMT for privacy, using symbolic varaibles to
 * represent sensitive values.
 * @author jeanyang
 */

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
        (map : Map[BigInt, IntExpr], level : LevelTy) =>
          if (level < minLevel)
            map + (level -> Constant(-1))
          else map + (level -> v)
        )
  }

  def mkSensitiveValue (
    levels : List[LevelTy], context : AtomVar, v : IntExpr, minLevel : BigInt)
  : IntVar = {
    val map = addCoarsePolicy(levels, v, minLevel);
    JeevesLib.createSensitiveValue(context, map)
  }
  def mkSensitiveValue (
    levels : List[LevelTy], context : IntVar, v : IntExpr, minLevel : BigInt)
  : IntVar = {
    val map = addCoarsePolicy(levels, v, minLevel);
    JeevesLib.createSensitiveValue(context, map)
  }

  // Associates a constraint with a field.
  def createSensitiveValue (context : AtomVar, vals : SensitiveMap) : IntVar = {
    var x = pick;

    // See if there is a default.
    val defaultVal =  vals.get(default);
    defaultVal match {
      case Some(v) => { x = pick(v, _ => true); }
      case None => { } // Do nothing for now.
    }

    // Go through keys and values.
    vals foreach {
      case (keyval, valConstraint) =>
        assume((context ~ '__username === keyval) ==> (x === valConstraint))
    }
    x
  }

  def createSensitiveValue (context : IntVar, vals : SensitiveMap) : IntVar = {
    var x = pick;

    // See if there is a default.
    val defaultVal =  vals.get(default);
    defaultVal match {
      case Some(v) => {
        x = pick(v, _ => true);
      }
      case None => { } // Do nothing for now.
    }
    
    // Go through keys and values.
    vals.foreach {
      case (keyval, valConstraint) =>
        println("context case: " + keyval);
        assume((context === keyval) ==> (x === valConstraint))
    }
    x
  }

}
