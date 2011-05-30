package cap.scalasmt

/* 
 * Translator to SMT-LIB2.
 * TODO: use dynamic linking library (as soon as x64 JNI issue is resolved)
 * TODO: avoid the cost of the process creation
 */
object SMT {
  object UnsatException extends RuntimeException("model is not satisfiable")

  var TIMEOUT = 10
  var PRINT_INPUT = false;
  var PRINT_OUTPUT = false;  

  private var Z3_PATH = Option(System.getProperty("smt.home")) match {
    case Some(path) => path
    case None => System.getProperty("user.home") + "/opt/z3/bin/z3"
  }
  def Z3_COMMANDS ="-smt2" :: "-m" :: "-t:" + TIMEOUT :: "-in" :: Nil
  
  private def variable(v: Var[_])(implicit env: Environment) =
    if (env.has(v))
      env(v).toString
    else
      v.toString

  private def formula(f: Formula)(implicit env: Environment): String = f match {
    case And(a,b) => "(and " + formula(a) + " " + formula(b) + ")"
    case Or(a,b) => "(or " + formula(a) + " " + formula(b) + ")"
    case Not(a) => "(not " + formula(a) + ")"
    case TrueF => "true"
    case FalseF => "false"
    case Eq(a,b) => "(= " + integer(a) + " " + integer(b) + ")"
    case Leq(a,b) => "(<= " + integer(a) + " " + integer(b) + ")"
    case Geq(a,b) => "(>= " + integer(a) + " " + integer(b) + ")" 
    case LT(a,b) => "(< " + integer(a) + " " + integer(b) + ")"
    case GT(a,b) => "(> " + integer(a) + " " + integer(b) + ")"  
    case BoolConditional(c,a,b) => "(if " + formula(c) + " " + formula(a) + " " + formula(b) + ")" 
    case v: BoolVar => variable(v)
  }

  private def integer(e: IntExpr)(implicit env: Environment): String = e match {
    case Plus(a,b) => "(+ " + integer(a) + " " + integer(b) + ")"
    case Minus(a,b) => "(- " + integer(a) + " " + integer(b) + ")"
    case Times(a,b) => "(* " + integer(a) + " " + integer(b) + ")"
    case IntConditional(c,a,b) => "(if " + formula(c) + " " + integer(a) + " " + integer(b) + ")"
    case Constant(i) => i.toString
    case v: IntVar => variable(v) 
  }

  /** Follow SMT-LIB 2 format */
  private def translate(f: Formula, env: Environment): List[String] = {
    "(set-logic QF_NIA)" ::
    "(declare-funs (" ::
    {for (v <- f.vars.toList;
          if ! env.has(v))
      yield v match {
        case _: IntVar => "  (" + v + " Int) "
        case _: BoolVar => "  (" + v + " Bool)"
      }
    } :::
    "))" ::
    {for (clause <- f.clauses) yield "(assert " + formula(clause)(env) + ")"} ::: 
    "(check-sat)" ::
    "(model)" ::
    "(next-sat)" ::
    Nil
  }

  /**
   * Solves for an assignment to satisfy the formula.
   * Some variables might be left without assignment.
   */
  def solve(f: Formula, env: Environment) = {
    val input = translate(f, env);
    
    // call Z3
    import java.io._
    import scala.Console.err
    val pb = new ProcessBuilder((Z3_PATH :: Z3_COMMANDS).toArray: _*);
    pb.redirectErrorStream(true);
    val p = pb.start;

    val os = new BufferedWriter(new OutputStreamWriter(p.getOutputStream));
    for (l <- input) {
      os.write(l);
      if (PRINT_INPUT) println(l);
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
      val List(name, value) = d.split("\\s").toList;
      for (v <- f.vars; if name == v.toString)
        v match {
          case v: IntVar => result = result + (v -> BigInt(value))
          case v: BoolVar => result = result + (v -> value.toBoolean)
        }
    }
    result;
  }
}

