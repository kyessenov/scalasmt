package cap.scalasmt

/*
 * A DSL for arithmetic constraints.
 * @author kuat
 */

// sealed enables exhaustiveness checks 
sealed abstract class Formula {
  def &&(that: Formula) = And(this, that)
  def ||(that: Formula) = Or(this, that)
  // TODO: what is the precedence?
  // def |=> (that: Formula) = Or(Not(this), that)
  def ===(that: Formula) = Or(And(this, that), (And(Not(this), And(Not(that)))))
  def unary_! = Not(this)

  def vars: Set[Var] = this match {
    case c: CompositeFormula => c.left.vars ++ c.right.vars
    case Not(sub) => sub.vars
    case c: CompositeAtomic => c.left.vars ++ c.right.vars
    case TrueF => Set()
    case FalseF => Set()     
  }
  def eval: Boolean = this match {
    case And(l, r) => l.eval && r.eval;
    case Or(l, r) => l.eval || r.eval;
    case Not(s) => ! s.eval;
    case Eq(l, r) => l.eval == r.eval;
    case Leq(l, r) => l.eval <= r.eval;
    case Geq(l, r) => l.eval >= r.eval;
    case LT(l, r) => l.eval < r.eval;
    case GT(l, r) => l.eval > r.eval;
    case TrueF => true;
    case FalseF => false;
  }  
}
sealed abstract class CompositeFormula extends Formula {
  def left: Formula;
  def right: Formula;
}
case class And(left: Formula, right: Formula = TrueF) extends CompositeFormula
case class Or(left: Formula, right: Formula = FalseF) extends CompositeFormula
case class Not(sub: Formula) extends Formula
// atomic formulas
sealed abstract class Atomic extends Formula 
sealed abstract class CompositeAtomic extends Atomic {
  def left: Expr;
  def right: Expr;
}
case class Eq(left: Expr, right: Expr) extends CompositeAtomic
case class Leq(left: Expr, right: Expr) extends CompositeAtomic
case class Geq(left: Expr, right: Expr) extends CompositeAtomic
case class LT(left: Expr, right: Expr) extends CompositeAtomic
case class GT(left: Expr, right: Expr) extends CompositeAtomic
case object TrueF extends Atomic
case object FalseF extends Atomic
// expresssions
sealed abstract class Expr {
  def ===(that: Expr) = Eq(this, that)
  def <=(that: Expr) = Leq(this, that)
  def >=(that: Expr) = Geq(this, that)
  def <(that: Expr) = LT(this, that)
  def >(that: Expr) = GT(this, that)
  def unary_- = Minus(Num(0), this)
  def +(that: Expr) = Plus(this, that)
  def -(that: Expr) = Minus(this, that)
  def *(that: Expr) = Times(this, that)
  
  def vars: Set[Var] = this match {
    case c: CompositeExpr => c.left.vars ++ c.right.vars;
    case IfThenElse(cond, thn, els) => cond.vars ++ thn.vars ++ els.vars;
    case v: Var => Set(v)
    case n: Num => Set()
  }
  def eval: Int = this match { 
    case Plus(l, r) => l.eval + r.eval;
    case Minus(l, r) => l.eval - r.eval;
    case Times(l, r) => l.eval * r.eval;
    case IfThenElse(cond, thn, els) => if (cond.eval) thn.eval else els.eval;
    case Num(i) => i
    case v: Var => v.value
  }
}
sealed abstract class CompositeExpr extends Expr {
  def left: Expr;
  def right: Expr;
}
case class Plus(left: Expr, right: Expr) extends CompositeExpr
case class Minus(left: Expr, right: Expr) extends CompositeExpr
case class Times(left: Expr, right: Expr) extends CompositeExpr
case class IfThenElse(cond: Formula, thn: Expr, els: Expr) extends Expr
case class Num(i: Int) extends Expr
// invariant: only one Var for every id
case class Var private(id: Int) extends Expr {
  override def toString = "var" + id
  def copy: Var = throw new RuntimeException;

  private var rep: Option[Int] = None
  def assigned = rep.isDefined
  def value_=(i: Int) {
    assert (! assigned);
    rep = Some(i);
  }
  def value = rep match {
    case Some(i) => i;
    case None => throw new RuntimeException("no value assigned to " + this);
  }
}
object Var {
  private var COUNTER = 0;
  def make = {
    COUNTER = COUNTER + 1;
    Var(COUNTER);
  }
}

// compiler looks up implicit conversions here 
object Expr {
  implicit def fromInt(i: Int) = Num(i)  
}
object Formula {
  implicit def fromBool(i: Boolean) = if (i) TrueF else FalseF
}
object `package` {
  def IF(cond: Formula)(thn: Expr) = new {def ELSE(els: Expr) = IfThenElse(cond, thn, els)}
}

// translator to SMT expressions
// TODO: use dynamic linking library (as soon as x64 JNI issue is resolved)
object SMT {
  var Z3_PATH = "/home/kuat/opt/z3/bin/z3"
  var Z3_COMMANDS ="-smtc" :: "-m" :: "-t:10" :: "-nw" :: "-in" :: Nil
  var DEFAULT = 0
   
  private def smtlib(f: Formula): String = f match {
    case And(a,b) => "(and " + smtlib(a) + " " + smtlib(b) + ")"
    case Or(a,b) => "(or " + smtlib(a) + " " + smtlib(b) + ")"
    case Not(a) => "(not " + smtlib(a) + ")"
    case TrueF => "true"
    case FalseF => "false"
    case Eq(a,b) => "(= " + smtlib(a) + " " + smtlib(b) + ")"
    case Leq(a,b) => "(<= " + smtlib(a) + " " + smtlib(b) + ")"
    case Geq(a,b) => "(>= " + smtlib(a) + " " + smtlib(b) + ")" 
    case LT(a,b) => "(< " + smtlib(a) + " " + smtlib(b) + ")"
    case GT(a,b) => "(> " + smtlib(a) + " " + smtlib(b) + ")"   
  }

  private def smtlib(e: Expr): String = e match {
    case Plus(a,b) => "(+ " + smtlib(a) + " " + smtlib(b) + ")"
    case Minus(a,b) => "(- " + smtlib(a) + " " + smtlib(b) + ")"
    case Times(a,b) => "(* " + smtlib(a) + " " + smtlib(b) + ")"
    case IfThenElse(c,a,b) => "(if " + smtlib(c) + " " + smtlib(a) + " " + smtlib(b) + ")"
    case Num(i) => i.toString
    case v: Var =>
      if (v.assigned)
        v.value.toString
      else 
        v.toString
  }

  private def smt(f: Formula): List[String] = f match {
    case And(a,b) => 
      smt(a) ::: smt(b);
    case _ => 
      "(assert " + smtlib(f) + ")" :: Nil
  }

  private def smt(vs: List[Var], f: Formula): List[String] = {
    "(declare-funs (" ::
    (for (v <- vs) yield "(" + v + " Int) ") :::
    "))" ::
    smt(f) ::: 
    "(check-sat)" ::
    "(model)" ::
    "(next-sat)" ::
    Nil
  }

  /**
   * Solves for an assignment to satisfy the formula.
   * Some variables might be left without assignment.
   * TODO: is it worth keeping SMT session alive?
   */
  def solve(f: Formula) {
    val vs = f.vars.filter(! _.assigned).toList
    val input = smt(vs, f);
    
    // call Z3
    import java.io._
    val pb = new ProcessBuilder((Z3_PATH :: Z3_COMMANDS).toArray: _*);
    pb.redirectErrorStream(true);
    val p = pb.start;
    val os = new BufferedWriter(new OutputStreamWriter(p.getOutputStream));
    for (l <- input) os.write(l);
    os.close;
    
    val is = new BufferedReader(new InputStreamReader(p.getInputStream));
    var line: String = is.readLine;
    var output:List[String] = Nil;
    while (line != null) {
      output = line :: output;
      line = is.readLine;
      //println(line);
    }
    is.close;

    p.destroy;

    if (output.size < 2) {
      println("unexpected output: " + output.reverse);
      println("on input: " + input);
      throw new RuntimeException("SMT failed");
    }
    var sat :: rest = output.reverse;
    var nextsat :: middle = rest.reverse;
    var model = middle.reverse.mkString("","","");
    if (sat != "sat") {
      throw new RuntimeException("formula is not satisfiable")
    }
    if (nextsat == "sat") 
      println("Warning: there are more than one possible assignments")    
    
    // parse model
    var m: Map[Int, Int] = Map();
    val PREFIX = "(\"model\" \"";
    val SUFFIX = "\")";
    val defines = model.substring(PREFIX.size, model.size - SUFFIX.size);
    //println(defines);
    val defs = defines.split("\\(define |\\)\\s*");
    for (d <- defs; if d.size > 0) {
      val List(vr,vl) = d.split("var|\\s").toList.drop(1).map(_.toInt)
      for (v @ Var(i) <- vs; if i == vr) 
        v.value = vl;
    }
  }

  def assignDefault(v: Var): Int = {
    if (! v.assigned)
      v.value = 0;
    v.value;
  }
}

object ChoiceStmt {
  import SMT._

  def choose(spec: Var => Formula): Int = {
    val x = Var.make;
    solve(spec(x))
    assignDefault(x)
  }

  def choose(spec: (Var, Var) => Formula): (Int, Int) = {
    val x = Var.make;
    val y = Var.make;
    solve(spec(x, y))
    (assignDefault(x), assignDefault(y))
  }

}





