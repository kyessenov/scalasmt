package cap.scalasmt

import scala.collection.immutable.Set.Set1

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
  
  /**
   * Expression translators.
   */

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
    case IntEq(a,b) => "(= " + integer(a) + " " + integer(b) + ")"
    case Leq(a,b) => "(<= " + integer(a) + " " + integer(b) + ")"
    case Geq(a,b) => "(>= " + integer(a) + " " + integer(b) + ")" 
    case LT(a,b) => "(< " + integer(a) + " " + integer(b) + ")"
    case GT(a,b) => "(> " + integer(a) + " " + integer(b) + ")"  
    case BoolConditional(c,a,b) => "(if " + formula(c) + " " + 
      formula(a) + " " + formula(b) + ")" 
    case RelEq(a,b) => "(forall (x Object) (= " + 
      atom(a)("x", env) + " " + atom(b)("x", env) + "))"
    case RelSub(a,b) => "(forall (x Object) (=> " + 
      atom(a)("x", env) + " " + atom(b)("x", env) + "))"
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

  private def atom(e: RelExpr)(implicit q: String, env: Environment): String = e match {
    case Union(a,b) => "(or " + atom(a) + " " + atom(b) + ")"
    case Diff(a,b) => "(and " + atom(a) + " (not " + atom(b) + "))"
    case Intersect(a,b) => "(and " + atom(a) + " " + atom(b) + ")"
    case Singleton(Object(o)) => "(= " + q + " " + uniq(o) + ")"
    case ObjectSet(os) => "(or " + os.map("(= " + q + " " + uniq(_) + ")") + ")"
    case Join(root, f) => 
      val r = q + "0";
      "(exists (" + r + " Object) (and (= " + q + 
        " (" + f.name + " " + r + ")) " + atom(root)(r, env) + "))"
    case Singleton(v : AtomVar) => "(= " + q + " " + 
      (if (env.has(v)) 
        uniq(env(v))
      else
        v.toString) + " )"
    case v: AtomSetVar => 
      if (env.has(v))
        atom(ObjectSet(env(v)))
      else
        "(" + v + " " + q + ")"

  }

  /** 
   * Atom foot print of a formula.
   * TODO: field decls make formulas depend on the heap implicitly.
   * TODO: universe bounding might make certain disequalities unsatisfiable
   */

  case class FootPrint(
    objects: Set[AnyRef] = Set(), 
     fields: Set[FieldDesc] = Set()) {
    def ++ (that: FootPrint) = 
      FootPrint(this.objects ++ that.objects, this.fields ++ that.fields)
  }

  private def univ(f: Formula)(implicit env: Environment): FootPrint = f match {
    case f: BinaryFormula => univ(f.left) ++ univ(f.right)
    case f: IntFormula => FootPrint()
    case f: RelFormula => univ(f.left) ++ univ(f.right)
    case BoolConditional(cond, thn, els) => univ(cond) ++ univ(thn) ++ univ(els)
    case FalseF => FootPrint()
    case TrueF => FootPrint()
    case Not(f) => univ(f)
    case _: BoolVar => FootPrint()
  }

  private def univ(e: RelExpr)(implicit env: Environment): FootPrint = e match {
    case f: BinaryRelExpr => univ(f.left) ++ univ(f.right)
    case Join(root, f) => univ(root) ++ FootPrint(fields = Set(f))
    case Singleton(o : Object[_]) => FootPrint(objects = Set(o.eval))
    case Singleton(v : AtomVar) => FootPrint(objects = if (env.has(v)) Set(env(v)) else Set())
    case os: ObjectSet => FootPrint(objects = os.eval)
    case v: AtomSetVar => FootPrint(objects = if (env.has(v)) env(v) else Set())
  }

  @annotation.tailrec 
  private def closure(fp: FootPrint): FootPrint = fp match {
    case FootPrint(objects, fields) =>
      val grow = objects ++ 
        (for (o <- objects; f <- fields) yield 
          Join(Object(o), f).eval).flatten ++
        Set(null) 

      if (grow.size > objects.size)
        closure(FootPrint(grow, fields))
      else 
        fp
  }

  private def uniq(o: AnyRef) = "o" + (if (o == null) "0" else o.toString.hashCode)

  /**
   * SMT-LIB 2 translation.
   */

  private def translate(f: Formula)(implicit env: Environment, fp: FootPrint): List[String] = {
    "(set-logic QF_NIA)" ::
    """(set-option set-param "ELIM_QUANTIFIERS" "true")""" :: 
    {
      val FootPrint(objects, fields) = fp;
      "(declare-datatypes ((Object " + 
         objects.map("(" + uniq(_) + ")").mkString(" ") + ")))" :: 
      {for (f <- fields) yield 
        "(declare-fun " + f.name + " (Object) Object)"}.toList :::
      {for (o <- objects; 
        f <- fields;
        v = Join(Object(o), f).eval) yield 
        "(assert (= (" + f.name + " " + uniq(o) + ") " + (if (v.size == 0) uniq(null) else uniq(v.head)) + "))"}.toList
    } :::
    "(declare-funs (" ::
    {for (v <- f.vars.toList;
          if ! env.has(v))
      yield "  " + (v match {
        case _: IntVar => "(" + v + " Int) "
        case _: BoolVar => "(" + v + " Bool)"
        case _: AtomVar => "(" + v + " Object)"
        case _: AtomSetVar => "(" + v + " Object Bool)"
      })
    } :::
    "))" ::
    {for (clause <- f.clauses) yield "(assert " + formula(clause) + ")"} ::: 
    "(check-sat)" ::
    "(get-info model)" ::
    "(next-sat)" ::
    Nil
  }

  /**
   * Solves for an assignment to satisfy the formula.
   * Some variables might be left without assignment.
   */
  def solve(f: Formula)(implicit env: Environment = EmptyEnv) = {
    val fp @ FootPrint(objects, _) = closure(univ(f))
    val input = translate(f)(env, fp);
    
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
      err.println("unexpected output: " + output.reverse.mkString("\n"));
      err.println("on input: " + input.mkString("\n"));
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
    val PREFIX = "((\"model\" \"";
    val SUFFIX = "\"))";
    val defines = model.substring(PREFIX.size, model.size - SUFFIX.size);
    
    val defs = defines.split("\\(define ");
    for (d <- defs; if d.size > 0 && ! d.startsWith("(")) {
      val List(name, value) = d.split("\\)|\\s").toList;
      for (v <- f.vars; if name == v.toString)
        v match {
          case v: IntVar => result = result + (v -> BigInt(value))
          case v: BoolVar => result = result + (v -> value.toBoolean)
          case v: AtomVar => result = result + 
            (v -> objects.find(o => uniq(o) == value).get)
          case v: AtomSetVar => 
            // TODO: better S-expression parsing
            throw new RuntimeException("not implemented")
        }
    }
    result;
  }
}

