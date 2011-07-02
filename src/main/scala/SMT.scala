package cap.scalasmt

/* 
 * Translator to SMT-LIB2.
 */
object UnsatException extends RuntimeException("inconsistent model")

trait Solver {
  /** Issue a command and expect success. */
  def command(s: String)
 /** Check satisfiability. */
  def check(): Boolean
  /** Check for another model. */
  def next(): Boolean
  /** Retrieve the model. */
  def model(): String
  /** Terminate the solver. */
  def close()
  /** Assert a boolean condition. */
  def assert(s: String) = command("(assert " + s + ")")
  /** Push a context */
  def push() = command("(push)")
  /** Pop a context */
  def pop() = command("(pop)")
 
  protected def >(s: String)
  protected def <(): String
}

trait Logging extends Solver {
  abstract override def >(s: String) {println(s); super.>(s)}
}

class Z3 extends Solver {
  import java.io._
  import scala.collection.mutable

  var TIMEOUT = 10;
  
  def PATH = Option(System.getProperty("smt.home")) match {
    case Some(path) => path
    case None => System.getProperty("user.home") + "/opt/z3/bin/z3"
  }

  def COMMANDS ="-smt2" :: "-m" :: "-t:" + TIMEOUT :: "-in" :: Nil
  
  private var process = {
    val pb = new ProcessBuilder((PATH :: COMMANDS).toArray: _*);
    pb.redirectErrorStream(true);
    pb.start;
  }
  
  private var input = new BufferedWriter(new OutputStreamWriter(process.getOutputStream));
  private var output = new BufferedReader(new InputStreamReader(process.getInputStream));
  
  protected def >(s: String) = {input.write(s); input.newLine; input.flush}
  protected def <() = output.readLine 

  protected def <<() = {
    val out = new mutable.ListBuffer[String]
    var line = <;
    while (line != null) {
      out += line;
      if (output.ready) line = < else line = null;
    }
    out.toList;
  }

  command("""(set-logic QF_NIA)""")
  command("""(set-option set-param "ELIM_QUANTIFIERS" "true")""")

  override def command(s: String) = {>(s); Predef.assert (< == "success")} 
  override def check = {>("(check-sat)"); < == "sat"}
  override def next  = {>("(next-sat)"); < == "sat"}
  override def model = {>("(get-info model)"); <<.mkString}
  override def close {input.close; output.close; process.destroy;}
}
 
/**
 * Expression translators.
 */ 
object SMT {
  private def variable(v: Var[_])(implicit env: Environment) =
    if (env.has(v))
      env(v).toString
    else
      v.toString

  private def formula(f: Expr[Boolean])(implicit env: Environment): String = f match {
    case And(a,b) => "(and " + formula(a) + " " + formula(b) + ")"
    case Or(a,b) => "(or " + formula(a) + " " + formula(b) + ")"
    case Not(a) => "(not " + formula(a) + ")"
    case BoolEq(a,b) => "(= " + formula(a) + " " + formula(b) + ")"
    case BoolVal(v) => v.toString
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

  private def integer(e: Expr[BigInt])(implicit env: Environment): String = e match {
    case Plus(a,b) => "(+ " + integer(a) + " " + integer(b) + ")"
    case Minus(a,b) => "(- " + integer(a) + " " + integer(b) + ")"
    case Times(a,b) => "(* " + integer(a) + " " + integer(b) + ")"
    case IntConditional(c,a,b) => "(if " + formula(c) + " " + integer(a) + " " + integer(b) + ")"
    case IntVal(i) => i.toString
    case ObjectIntField(root, f) => "(" + f.name + " " + atom(root) + ")"
    case v: IntVar => variable(v) 
  }

  private def atom(e: Expr[Atom])(implicit env: Environment): String = e match {
    case ObjectConditional(cond, thn, els) => "(if " + formula(cond) + " " + atom(thn) + " " + atom(els) + ")"
    case Object(o) => uniq(o)
    case ObjectField(root, f) => "(" + f.name + " " + atom(root) + ")"
    case v: ObjectVar =>  
      if (env.has(v)) 
        uniq(env(v))
      else
        v.toString
  }

  private def set(e: Expr[Set[Atom]])(implicit q: String, env: Environment): String = e match {
    case Union(a,b) => "(or " + set(a) + " " + set(b) + ")"
    case Diff(a,b) => "(and " + set(a) + " (not " + set(b) + "))"
    case Intersect(a,b) => "(and " + set(a) + " " + set(b) + ")"
    case Singleton(o) => "(= " + q + " " + atom(o) + ")"
    case ObjectSet(os) => if (os.size == 0) "false" else "(or " + os.map("(= " + q + " " + uniq(_) + ")").mkString(" ") + ")"
    case RelJoin(root, f) => 
      val r = q + "0";
      "(exists (" + r + " Object) (and (= " + q + 
        " (" + f.name + " " + r + ")) " + set(root)(r, env) + "))"
    case v: ObjectSetVar => 
      if (env.has(v))
        set(ObjectSet(env(v)))
      else
        "(" + v + " " + q + ")"
  }

  /** 
   * Bounded universe of atoms (and their fields)   
   */

  private def uniq(o: Atom) = "o" + (if (o == null) "0" else o.uniq)
  
  private object Scope {
    implicit def fromField(f: FieldDesc[_]) = Scope(fields = Set(f))
    implicit def fromVar(v: Var[_]) = Scope(vars = Set(v))
    implicit def fromAtom(o: Atom) = Scope(objects = Set(o))
    implicit def fromAtomSet(o: Set[Atom]) = Scope(objects = o)
  }

  private case class Scope(objects: Set[Atom] = Set(), 
                       fields: Set[FieldDesc[_]] = Set(), 
                       vars: Set[Var[_]] = Set()) {
    def ++ (that: Scope) = Scope(
      this.objects ++ that.objects, 
      this.fields ++ that.fields, 
      this.vars ++ that.vars
    )
    def size = objects.size + fields.size + vars.size
  }
 
  private def univ(f: Expr[_])(implicit env: Environment): Scope = (f: @unchecked) match {
    case f: BinaryExpr[_] => univ(f.left) ++ univ(f.right)
    case f: Ite[_] => univ(f.cond) ++ univ(f.thn) ++ univ(f.els)
    case f: UnaryExpr[_] => univ(f.sub)
    case v: Var[_] => v
    case os: ObjectSet => os.eval
    case o: Object => o.eval
    case RelJoin(root, f) => univ(root) ++ f
    case ObjectIntField(root, f) => univ(root) ++ f
    case ObjectField(root, f) => univ(root) ++ f
    case _: Constant[_] => Scope()
  }

  @annotation.tailrec 
  private def closure(cur: Scope)(implicit env: Environment): Scope = {
    val fields = {for (o <- cur.objects; f <- cur.fields) yield f match {
      case f: ObjectFieldDesc => f(o)
      case f: IntFieldDesc => f(o)
    }}.flatten

    val variables = {for (v <- cur.vars; if env.has(v)) yield v match {
      case v: ObjectVar => Set(env(v))
      case v: ObjectSetVar => env(v)
      case _ => Set[Atom]()
    }}.flatten

    val more = cur ++ 
      (null: Atom) ++ 
      variables ++ 
      fields.map(univ(_)).foldLeft(Scope())(_ ++ _)

    if (more.size > cur.size)
      closure(more)
    else 
      cur
  }


  /**
   * SMT-LIB 2 translation.
   */

  private def prelude(implicit env: Environment, fp: Scope): List[String] = {
    val Scope(objects, fields, vars) = fp;

    // declare all objects
    "(declare-datatypes ((Object " + objects.map("(" + uniq(_) + ")").mkString(" ") + ")))" :: 
    // declare all fields
    {for (f <- fields) yield "(declare-fun " + f.name + {f match {
      case _: ObjectFieldDesc => " (Object) Object)"
      case _: IntFieldDesc => " (Object) Int)"
    }}}.toList :::
    // declare all variables
    {for (v <- vars.toList; if ! env.has(v))
      yield "(declare-fun " + v + {v match {
        case _: IntVar =>  " () Int) "
        case _: BoolVar => " () Bool)"
        case _: ObjectVar => " () Object)"
        case _: ObjectSetVar => " (Object) Bool)"
    }}}.toList :::
    // declare field values
    {for (o <- objects; f <- fields) yield "(assert (= (" + f.name + " " + uniq(o) + ") " + {f match {
      case f: ObjectFieldDesc => f(o) match {
        case Some(a) => atom(a)
        case None => /* function is total */ uniq(f.default)
      }
      case f: IntFieldDesc => f(o) match {
        case Some(i) => integer(i)
        case None => /* function is total */ f.default
      }
    }} + "))"}.toList :::
    Nil
  }

  /**
   * Solves for an assignment to satisfy the formula.
   * Some variables might be left without assignment.
   * 
   * Super-normal default logic is decided to saturating context in the order
   * the judgements are supplied.
   */
  def solve(f: Formula, defaults: List[Formula] = Nil, checkNext: Boolean = true)
    (implicit env: Environment = DefaultEnv) = {
    implicit val scope = closure(univ(f :: defaults))

    val solver = new Z3// with Logging
  
    for (s <- prelude) solver.command(s)  
    for (clause <- f.clauses) 
      solver.assert(formula(clause))

    // invariant: the model is consistent 
    if (! solver.check) throw UnsatException
    for (d <- defaults) {
      solver.push;
      solver.assert(formula(d));
      if (! solver.check) solver.pop;
    }
    assert (solver.check)
    val result = model(solver.model, scope)
    
    if (checkNext && solver.next) 
      println("Warning: multiple assignments") 

    solver.close;   
 
    result;
  }

  private def model(model: String, scope: Scope)
    (implicit env: Environment) = {
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
          case v: ObjectVar => result = result + 
            (v -> scope.objects.find(o => uniq(o) == value).get)
          case v: ObjectSetVar => 
            // TODO: better S-expression parsing
            throw new RuntimeException("not implemented")
        }
    }
    result
  }
}

