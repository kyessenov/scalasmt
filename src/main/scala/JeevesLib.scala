package cap.scalasmt

import scala.collection.mutable.Map;

object JeevesLib extends Sceeves {
  // Keep map [TODO!] of privacy levels.
  private var Privacy_levels = Map.empty[String, IntExpr]
  var context = pick(_ => true)

  /* Functions for manipulating data. */

  /* Functions for associating constraints */

  // Associates a constraint with a field.
  def createSensitiveValue (vals : Map[String, IntExpr])
    : Unit = {
    var x = pick(_ => true);

    // See if there is a default.
    val defaultVal =  vals.get("default");
    defaultVal match {
      case Some(v) => {
        x = pick(v, _ => true);
      }
      case None => {
        /* Do nothing for now. */
      }
    }

    // Go through keys and values.
    vals foreach {
      case (level, valConstraint) =>
        // Map the key to a variable.
        val keyval = 
          Privacy_levels.get(level) match {
            case Some(v) => v
            case None => throw new IllegalArgumentException("wrong field")
        };
        // Interpret the value as a constraint.
        assume(IF (context === keyval) (x === valConstraint) ELSE true)
    }
    ()
  }
}
