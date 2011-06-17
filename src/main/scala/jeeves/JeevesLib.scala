package cap.jeeves

/*
 * A library for using ScalaSMT for privacy, using symbolic varaibles to
 * represent sensitive values.
 * @author jeanyang
 */

import cap.scalasmt._
import scala.collection.immutable.Map;
import scala.collection.mutable.{Map => MMap};
import scala.collection.mutable.HashMap;

object Undefined extends RuntimeException("undefined")
object JeevesLib extends Sceeves {
  type LevelTy = BigInt;
  type ValueTy = BigInt;
  type SensitiveMap = Map[BigInt, IntExpr];

  private val __strs = new HashMap[BigInt, String]();
  def fromString (str : String) : BigInt = {
    val idx : BigInt = str.hashCode();
    __strs += (idx -> str);
    idx
  }
  def asString (v : BigInt) : String = {
    if (v == -1) {
      "[default]"
    } else {
      __strs.get(v) match {
        case Some(str) => str
        case None => println(v); throw Undefined
      }
    }
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
    createSensitiveValue(level, -1, map)
  }

  // Associates a constraint with a field.
  def createSensitiveValue
    (level : IntVar, defaultV : BigInt, vals : SensitiveMap) : IntVar = {
    var x = pick(default = defaultV);
    vals foreach {
      case (keyval, valConstraint) =>
        assume((level === keyval) ==> (x === valConstraint))
    }
    x
  }
  def createSensitiveValue
    (level : IntVar, vals : Map[BigInt, ObjectExpr])
    : AtomVar = {
    var x = pickAtom(default = NULL);
    vals foreach {
      case (keyval, valConstraint) =>
        assume((level === keyval) ==> (x === valConstraint))
    }
    x
  }

  def concretizeList[T] (
    ctxtVar : AtomVar, context : ObjectExpr, lst : List[ObjectExpr])
  : List[T] = {
    val objLst : List[T] =
      lst.map(x => concretize(ctxtVar, context, x).asInstanceOf[T]);
    objLst.filter(x => !(x == null))
  }
}
