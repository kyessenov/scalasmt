package cap.scalasmt

/* 
 * Translator to SMT-LIB2.
 * TODO: use dynamic linking library (as soon as x64 JNI issue is resolved)
 * TODO: avoid the cost of the process creation
 */
object SMT {
  object UnsatException extends RuntimeException("model is not satisfiable")

  var TIMEOUT = 10
  var PRINT_DEBUG = false;
  var PRINT_INPUT = true;
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
    case ObjectEq(a,b) => "(= " +  atom(a) + " " + atom(b) + ")"
    case RelEq(a,b) => "(forall (x Object) (iff " + 
      set(a)("x", env) + " " + set(b)("x", env) + "))"
    case RelSub(a,b) => "(forall (x Object) (=> " + 
      set(a)("x", env) + " " + set(b)("x", env) + "))"
    case v: BoolVar => variable(v)
  }

  private def integer(e: IntExpr)(implicit env: Environment): String = e match {
    case Plus(a,b) => "(+ " + integer(a) + " " + integer(b) + ")"
    case Minus(a,b) => "(- " + integer(a) + " " + integer(b) + ")"
    case Times(a,b) => "(* " + integer(a) + " " + integer(b) + ")"
    case IntConditional(c,a,b) => "(if " + formula(c) + " " + integer(a) + " " + integer(b) + ")"
    case Constant(i) => i.toString
    case ObjectIntField(root, f) => "(" + f.name + " " + atom(root) + ")"
    case v: IntVar => variable(v) 
  }

  private def atom(e: ObjectExpr)(implicit env: Environment): String = e match {
    case AtomConditional(cond, thn, els) => "(if " + formula(cond) + " " + atom(thn) + " " + atom(els) + ")"
    case Object(o) => uniq(o)
    case v: AtomVar =>  
      if (env.has(v)) 
        uniq(env(v))
      else
        v.toString
  }

  private def set(e: RelExpr)(implicit q: String, env: Environment): String = e match {
    case Union(a,b) => "(or " + set(a) + " " + set(b) + ")"
    case Diff(a,b) => "(and " + set(a) + " (not " + set(b) + "))"
    case Intersect(a,b) => "(and " + set(a) + " " + set(b) + ")"
    case Singleton(o) => "(= " + q + " " + atom(o) + ")"
    case ObjectSet(os) => if (os.size == 0) "false" else "(or " + os.map("(= " + q + " " + uniq(_) + ")").mkString(" ") + ")"
    case RelJoin(root, f) => 
      val r = q + "0";
      "(exists (" + r + " Object) (and (= " + q + 
        " (" + f.name + " " + r + ")) " + set(root)(r, env) + "))"
    case v: AtomSetVar => 
      if (env.has(v))
        set(ObjectSet(env(v)))
      else
        "(" + v + " " + q + ")"
  }

  /** 
   * Bounded universe of atoms (and their fields)   
   */

  private def uniq(o: Atom) = "o" + (if (o == null) "0" else o.uniq)
  
  private case class Scope(objects: Set[Atom] = Set(), 
                       fields: Set[FieldDesc[_]] = Set(), 
                       vars: Set[Var[_]] = Set()) {
    def ++ (that: Scope) = 
      Scope(this.objects ++ that.objects, this.fields ++ that.fields, this.vars ++ that.vars)
    def ++ (f: FieldDesc[_]) = copy(fields = fields + f)
    def ++ (v: Var[_]) = copy(vars = vars + v)
    def ++ (o: Atom) = copy(objects = objects + o)
    def size = objects.size + fields.size + vars.size
    override def toString = "objects: " + objects.size + "; fields: " + fields.size + "; vars: " + vars.size
  }

  private def univ(f: Expr[_])(implicit env: Environment): Scope = f match {
    case f: BinaryExpr[_] => univ(f.left) ++ univ(f.right)
    case f: Ite[_] => univ(f.cond) ++ univ(f.thn) ++ univ(f.els)
    case f: UnaryExpr[_] => univ(f.sub)
    case v: BoolVar => Scope() ++ v
    case v: IntVar => Scope() ++ v
    case v: AtomVar => Scope(objects = if (env.has(v)) Set(env(v)) else Set()) ++ v
    case v: AtomSetVar => Scope(objects = if (env.has(v)) env(v) else Set()) ++ v
    case Object(o) => Scope() ++ o
    case os: ObjectSet => Scope(objects = os.eval)
    case RelJoin(root, f) => univ(root) ++ f
    case ObjectIntField(root, f) => univ(root) ++ f
    case _  => Scope()
  }

  @annotation.tailrec 
  private def closure(cur: Scope)(implicit env: Environment): Scope = {
    val exprs = {for (o <- cur.objects; f <- cur.fields) yield f match {
      case f: AtomFieldDesc => f(o)
      case f: IntFieldDesc => f(o)
    }}.flatten

    val more = exprs.foldLeft(cur){(scope, expr) => scope ++ univ(expr)} ++ (null: Atom)

    if (more.size > cur.size)
      closure(more)
    else 
      cur
  }


  /**
   * SMT-LIB 2 translation.
   */

  private def translate(f: Formula)(implicit env: Environment, fp: Scope): List[String] = {
    val Scope(objects, fields, vars) = fp;
    assert (f.vars subsetOf vars)
    if (PRINT_DEBUG) 
      println(fp)

    // declare all objects
    "(declare-datatypes ((Object " + objects.map("(" + uniq(_) + ")").mkString(" ") + ")))" :: 
    // declare all fields
    {for (f <- fields) yield "(declare-fun " + f.name + {f match {
      case _: AtomFieldDesc => " (Object) Object)"
      case _: IntFieldDesc => " (Object) Int)"
    }}}.toList :::
    // declare all variables
    "(declare-funs (" ::
    {for (v <- vars.toList; if ! env.has(v))
      yield "  " + (v match {
        case _: IntVar => "(" + v + " Int) "
        case _: BoolVar => "(" + v + " Bool)"
        case _: AtomVar => "(" + v + " Object)"
        case _: AtomSetVar => "(" + v + " Object Bool)"
      })
    } ::: "))" ::
    // assert field values
    {for (o <- objects; f <- fields) yield "(assert (= (" + f.name + " " + uniq(o) + ") " + {f match {
      case f: AtomFieldDesc => f(o) match {
        case Some(a) => atom(a)
        case None => /* function is total */ uniq(null)
      }
      case f: IntFieldDesc => f(o) match {
        case Some(i) => integer(i)
        case None => /* function is total */ 0
      }
    }} + "))"}.toList :::
    // assert formula
    {for (clause <- f.clauses) yield "(assert " + formula(clause) + ")"} ::: 
    """ 
    |(set-logic QF_NIA)
    |(set-option set-param "ELIM_QUANTIFIERS" "true") 
    |(check-sat)
    |(get-info model)
    |(next-sat)
    """.stripMargin ::
    Nil
  }

  /**
   * Solves for an assignment to satisfy the formula.
   * Some variables might be left without assignment.
   */
  def solve(f: Formula)(implicit env: Environment = DefaultEnv) = {
    val scope = closure(univ(f))
    val input = translate(f)(env, scope);
    
    // call Z3
    import java.io._
    import scala.Console.err
    val pb = new ProcessBuilder((Z3_PATH :: Z3_COMMANDS).toArray: _*);
    pb.redirectErrorStream(true);
    val p = pb.start;
  
    // print input
    val os = new BufferedWriter(new OutputStreamWriter(p.getOutputStream));
    for (l <- input) {
      os.write(l);
      if (PRINT_INPUT) println(l);
    }
    os.close;
    
    // read output
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

    // parse output
    if (output.size < 2) {
      err.println("unexpected output: " + output.reverse.mkString("\n"));
      err.println("on input: " + input.mkString("\n"));
      throw new RuntimeException("unexpected output from SMT");
    }
    val sat :: rest = output.reverse;
    val nextsat :: middle = rest.reverse;
    val model = middle.reverse.mkString("","","");

    if (sat != "sat") { 
      if (PRINT_DEBUG)
        for (l <- input) println(l)
      throw UnsatException
    }
    
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
      for (v <- scope.vars; if name == v.toString)
        v match {
          case v: IntVar => result = result + (v -> BigInt(value))
          case v: BoolVar => result = result + (v -> value.toBoolean)
          case v: AtomVar => result = result + 
            (v -> scope.objects.find(o => uniq(o) == value).get)
          case v: AtomSetVar => 
            // TODO: better S-expression parsing
            throw new RuntimeException("not implemented")
        }
    }
    result;
  }
}

