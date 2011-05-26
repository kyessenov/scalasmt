package cap.scalasmt

import scala.collection.mutable.Map;

object Undefined extends RuntimeException("undefined")
object JeevesLib extends Sceeves {
  // Keep map of privacy levels.
  private var Privacy_levels = Map.empty[String, IntVar]
  var context = pick(_ => true)

  /* Functions for manipulating data. */

  /* Functions for associating constraints */
  private def string2IntExpr (str : String) : IntExpr = {
    // TODO: Handle more than just integer expressions for now.
    try { str.toInt }
    catch {
      case _ : java.lang.NumberFormatException => throw Undefined
    }
  }

  private def getValueMappings (encoded : String) : Map[String, IntExpr] = {
    val asTuples : List[(String, String)] = throw Undefined;
    asTuples.foldLeft(Map.empty[String, IntExpr]) {
      case (mappings, (level, expr)) =>
        mappings + (level -> string2IntExpr (expr))
    }
  }

  // Associates a constraint with a field.
  private def createSensitiveValue (vals : Map[String, IntExpr])
    : IntExpr = {
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
        // TODO: Allow ite to be general Formulas (or Expressions) that can be
        // things other than IntExprs.
        assume(IF (context === keyval) (x === valConstraint) ELSE true)
    }
    x
  }

  /* This function returns an IntVar representing the sensitive value. */
  private def string2Expr (str : String) : IntExpr =
    createSensitiveValue(getValueMappings(str))
  private def expr2String (e : IntExpr) : String =
    e match {
/*      case Plus(l, r) => throw Undefined
      case Minus(l, r) => throw Undefined
      case Times(l, r) => throw Undefined
      case IntConditional(cond, thn, els) => throw Undefined */
      case Constant(i) => i.toString()
      case v: IntVar => throw Undefined
      case other => throw Undefined
    }

  /* Define a security level. */
  def defineSecurityLevel (levelName : String) (levelExpr : IntExpr) : Unit = {
    val x = pick(x => x === levelExpr);
    Privacy_levels.update(levelName, x)
  }

  /* DATABASE.  Access to the backend database.
  */
  def putDatabaseValue[T]
    (field : String) (value : T, permission : String) : Unit = {
    // Make a permission string.
    DatabaseLib.putField(field, value);
    DatabaseLib.putField(field, permission)
  }

  def getDatabaseValue (field : String) : IntExpr = {
    // TODO: Get permissions from database and interpret them as a
    // Map[String, IntExpr]
    DatabaseLib.getField[Int](field) match {
      case Some(intval) => Constant(intval)
      case None =>
        DatabaseLib.getField[String]("__" + field) match {
          case Some(strval) => string2Expr(strval)
          case None => throw Undefined
        }
    }
  }
}
