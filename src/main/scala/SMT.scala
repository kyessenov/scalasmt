package cap.scalasmt

// translator to SMT expressions
// TODO: use dynamic linking library (as soon as x64 JNI issue is resolved)
// TODO: avoid the cost of the process creation

object SMT {
  object UnsatException extends RuntimeException("model is not satisfiable")

  var TIMEOUT = 10
  var Z3_PATH = System.getProperty("user.home") + "/opt/z3/bin/z3"
  var Z3_COMMANDS ="-smt2" :: "-m" :: "-t:" + TIMEOUT :: "-in" :: Nil
  
  var PRINT_INPUT = false;
  var PRINT_OUTPUT = false;  

  private def smtlib(f: Formula)(implicit env: Environment): String = f match {
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
    case BoolConditional(c,a,b) => "(if " + smtlib(c) + " " + smtlib(a) + " " + smtlib(b) + ")" 
  }

  private def smtlib(e: IntExpr)(implicit env: Environment): String = e match {
    case Plus(a,b) => "(+ " + smtlib(a) + " " + smtlib(b) + ")"
    case Minus(a,b) => "(- " + smtlib(a) + " " + smtlib(b) + ")"
    case Times(a,b) => "(* " + smtlib(a) + " " + smtlib(b) + ")"
    case IntConditional(c,a,b) => "(if " + smtlib(c) + " " + smtlib(a) + " " + smtlib(b) + ")"
    case Constant(i) => i.toString
    case v: IntVar => 
      if (env.has(v))
        env(v).toString
      else 
        v.toString
  }

  private def smt(f: Formula)(implicit env: Environment): List[String] = f match {
    case And(a,b) => 
      smt(a) ::: smt(b);
    case _ => 
      "(assert " + smtlib(f) + ")" :: Nil
  }
  
  /** Follow SMT-LIB 2 format */
  private def translate(f: Formula)(implicit env: Environment): List[String] = {
    "(set-logic QF_NIA)" ::
    "(declare-funs (" ::
    (for (v <- f.vars.toList;
          if ! env.has(v)) 
      yield "  (" + v + " Int) ") :::
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
  def solve(f: Formula, env: Environment) = {
    val input = translate(f)(env);
    
    // call Z3
    import java.io._
    import scala.Console.err
    val pb = new ProcessBuilder((Z3_PATH :: Z3_COMMANDS).toArray: _*);
    pb.redirectErrorStream(true);
    val p = pb.start;

    val os = new BufferedWriter(new OutputStreamWriter(p.getOutputStream));
    for (l <- input) {
      os.write(l);
      if (PRINT_INPUT) 
        println(l);
    }
    os.close;
    
    val is = new BufferedReader(new InputStreamReader(p.getInputStream));
    var line: String = is.readLine;
    var output:List[String] = Nil;
    while (line != null) {
      if (PRINT_OUTPUT) println(line);
      output = line :: output;
      line = is.readLine;
    }
    is.close;

    p.destroy;

    if (output.size < 2) {
      err.println("unexpected output: " + output.reverse);
      err.println("on input: " + input);
      throw new RuntimeException("unexpected output from SMT");
    }
    val sat :: rest = output.reverse;
    val nextsat :: middle = rest.reverse;
    val model = middle.reverse.mkString("","","");

    if (sat != "sat") 
      throw UnsatException
    
    if (nextsat == "sat") 
      err.println("Warning: there are more than one possible assignments")    
    
    // parse model
    var result = env;
    val PREFIX = "(\"model\" \"";
    val SUFFIX = "\")";
    val defines = model.substring(PREFIX.size, model.size - SUFFIX.size);
    val defs = defines.split("\\(define |\\)\\s*");
    for (d <- defs; if d.size > 0) {
      val List(vr,vl) = d.split("var|\\s").toList.drop(1).map(BigInt(_));
      for (v @ IntVar(i) <- f.vars; if i == vr)  
        result = result + (v -> vl);
    }
    result;
  }
}

