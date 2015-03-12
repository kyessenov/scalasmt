
This is a playground project for testing ideas on integrating declarative constraints into a general-purpose programming language. SAT, SMT, CSP, and Logic Programming are all welcome as back-ends.

We have the following dependencies:
  * [http://code.google.com/p/simple-build-tool/][SBT] build tool
  * [http://research.microsoft.com/en-us/um/redmond/projects/z3/][Z3] version 3.2 or later. In theory, any SMT-LIB2 compliant solver should work.

To run the tests:
  * set `smt.home` to wherever you placed Z3 binary ("~/opt/z3" by default)
  * `./sbt update`
  * `./sbt test`

----

`AST.scala` defines the language of constraints. We support the linear arithmetic, boolean logic, equality theory on objects and fields of objects interpreted as functions.

----

Sceeves is a constraint environment that provides three primitive operations: 
  * `pick`: creates a symbolic variable (think of it as an angelic choice)
  * `assume`: adds a constraint (this is in some way opposite to assert)
  * `concretize`: solves for symbolic variables using the constraints  

`pick` also allows specifying default values which are applied whenever that does not cause contradiction with the rest of the constraints. This is a way to control non-determinism.

`concretize` also allows specifying context constraint that is assumed temporarily to evaluate an expression.

----

Jeeves is an extension of Sceeves for privacy constraints.

----
The examples are in `test` directory. 

Example (Sudoku solver):
```scala
    val M = 3;
    val N = M * M;
    
    // create symbolic variables 
    val s = Array.ofDim[IntVar](N,N);
    for (i <- 0 until N; j <- 0 until N) 
      s(i)(j) = pick (x => 0 < x && x <= N)
    
    // all rows are distinct
    for (i <- 0 until N) 
      assume(DISTINCT(s(i)))

    // all columns are distinct
    for (j <- 0 until N) 
      assume(DISTINCT(for (i <- 0 until N) yield s(i)(j)))

    // all M blocks are distinct
    for (mi <- 0 until M;
         mj <- 0 until M)
      assume(DISTINCT(for (i <- 0 until M; j <- 0 until M) 
                yield s(M*mi + i)(M*mj + j)))
    
    // fill in known cells
    assert (input.length == N * N);
    for (i <- 0 until N; 
         j <- 0 until N;
         c = input(i*N + j);
         if c != '0')
      assume(s(i)(j) === c.toString.toInt);
 
    // solve for constraints
    for (i <- 0 until N; j <- 0 until N) 
      concretize(s(i)(j));
```

Example (Jeeves social network):
```scala
case class Name(s: String) extends JeevesRecord
case class Email(s: String) extends JeevesRecord
case class Network(s: String) extends JeevesRecord
sealed trait UserLevel 
object Anyone extends UserLevel
object Self extends UserLevel
object Friends extends UserLevel

class UserRecord(
  nameV: Name, 
  nameL: UserLevel,
  networkV: Network, 
  networkL: UserLevel, 
  friendsL: UserLevel) extends JeevesRecord {
  private var friends : Set[UserRecord] = Set()

  /** Mutators */
  def add(u: UserRecord) {friends = friends + u}
  def remove(u: UserRecord) {friends = friends - u}

  /** Observers */
  val name = mkSensitive[Name](level(nameL), nameV)
  val network = mkSensitive[Network](level(networkL), networkV);
  def getFriends() = {
    val l = level(friendsL);
    friends.map(mkSensitive[UserRecord](l, _))
  }
  def isFriends(u: UserRecord) = getFriends.has(u)

  /** Helpers */
  private def level(l: UserLevel): LevelVar = {
    val level = mkLevel();
    val me = CONTEXT === this;
    l match {
      case Anyone => 
      case Self => 
        policy(level, ! me, LOW)
      case Friends => 
        policy(level, ! friends.has(CONTEXT) && ! me,  LOW);
    }
    level
  }
}
```
