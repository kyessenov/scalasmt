package cap.scalasmt

// translator to SMT expressions
// TODO: use dynamic linking library (as soon as x64 JNI issue is resolved)
// TODO: avoid the cost of the process creation

object SMT {
  object UnsatException extends RuntimeException("model is not satisfiable")

  var TIMEOUT = 10
  var Z3_PATH = "/home/kuat/opt/z3/bin/z3"
  var Z3_COMMANDS ="-smtc" :: "-m" :: "-t:" + TIMEOUT :: "-in" :: Nil
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
    (for (v <- vs) yield "  (" + v + " Int) ") :::
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
      throw UnsatException
    }
    if (nextsat == "sat") 
      println("Warning: there are more than one possible assignments")    
    
    // parse model
    var m: Map[Int, Int] = Map();
    val PREFIX = "(\"model\" \"";
    val SUFFIX = "\")";
    val defines = model.substring(PREFIX.size, model.size - SUFFIX.size);
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

