package cap.scalasmt

import scala.collection.immutable.Map;
import scala.collection.mutable.{Map => MMap};

object Undefined extends RuntimeException("undefined")
object JeevesLib extends Sceeves {
  type LevelTy = BigInt;
  type ValueTy = BigInt;
  type SensitiveMap = Map[BigInt, BigInt];

  // Keep map of privacy levels.
  private var plevel_count : LevelTy = 1;
  
  val default : LevelTy = 0;
  def getLevel () : LevelTy = {
    val plevel_old = plevel_count
    plevel_count = plevel_count + 1;
    plevel_old
  }

  // Associates a constraint with a field.
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
    vals foreach {
      case (keyval, valConstraint) =>
        println(keyval);
        println(valConstraint);
        assume((context === keyval) ==> (x === valConstraint))
    }
    x
  }
}
