package test.cap.scalasmt

import cap.scalasmt._
import org.scalatest.FunSuite
import org.scalatest.Assertions.{expect}

class ExampleSceeves extends FunSuite with Sceeves {
  test ("pick") {
    val x = pick (_ === 1);
    expect(1) {concretize(x)};
    expect(1) {concretize(x)};
  }

  test ("unsatisfiable") {
    new Sceeves {
      val x = pick (_ => false);
      intercept[SMT.UnsatException.type] {
        println(concretize(x));
      }
    }
  }

  test ("pick 2") {
    val y = pick (_ > 0);
    val z = pick (z => y === z && z === 1);
    expect(1) {concretize(z)};
    expect(1) {concretize(y)};
  }

  test ("pick many") {
    val x = pick (_ > 0);
    val y = pick (_ > 0);
    val z = pick (_ > 0);
    assume(x + y + z === 3);
    expect(1) {concretize(x)};
    expect(1) {concretize(y)};
    expect(1) {concretize(z)}; 
  }

  test ("conditional") {
    val x = pick (_ > 1337);
    val y: Expr = IF (x > 1337) {1} ELSE {0};
    expect(1) {concretize(y)};
  }

  val M = 3;
  val N = M * M;
 
  def sudoku(input: String) = {
    val s = Array.ofDim[IntVar](N,N);
    for (i <- 0 until N; j <- 0 until N) s(i)(j) = pick (x => x > 0 && x <= N)
    
    def distinct(vs: Traversable[IntVar]) =
      for (vs1 <- vs; vs2 <- vs; if (vs1 != vs2)) assume ( ! (vs1 === vs2))

    // all rows are distinct
    for (i <- 0 until N) distinct(s(i))

    // all columns are distinct
    for (j <- 0 until N) distinct(for (i <- 0 until N) yield s(i)(j))

    // all M blocks are distinct
    for (mi <- 0 until M;
         mj <- 0 until M)
      distinct(for (i <- 0 until M; j <- 0 until M) yield s(M*mi + i)(M*mj + j))
    
    assert (input.length == N * N);
    for (i <- 0 until N; 
         j <- 0 until N;
         c = input(i*N + j);
         if c != '0')
      s(i)(j).value = c.toString.toInt;
 
    for (i <- 0 until N; j <- 0 until N) concretize(s(i)(j));

    s;
  }

  test ("sudoku") {
    // input: see relations.examples.sudoku for more
    val input = "600200050018060020003000400000607800402050000000908000504090300020000014300005007"
    val s = sudoku(input); 
    for (i <- 0 until N)
      println(s(i).map(concretize(_)).toList.mkString("["," ","]"))   
  }

  ignore ("sudoku performance") {
    val PROBLEMS = 10;

    // measure average time
    import scala.io.Source
    val source = Source.fromURL(getClass.getResource("/sudoku17.txt"));

    val result = (for (l <- source.getLines.take(PROBLEMS);         
         start = System.currentTimeMillis;
         s = sudoku(l);
         end = System.currentTimeMillis)
       yield (end - start)).toList;

    println (result.size + " problems");
    println (result.mkString("[", " ", "]") + " time");
  }

  test ("graph coloring") {
    case class Node(name: String);
    case class Edge(from: Node, to: Node)
    case class Graph(edges: Traversable[Edge], nodes: Traversable[Node])

    def color(k: Int, g: Graph) = {
      val c = Map() ++ g.nodes.map(n => (n, pick {c => c >= 0 && c < k}));
      for (e <- g.edges) 
        assume (c(e.from) !== c(e.to));  
      g.nodes.map(n => concretize(c(n)));
    }

    val a = Node("a");
    val b = Node("b");
    val c = Node("c");
    val ab = Edge(a, b);
    val bc = Edge(b, c);
    val ca = Edge(c, a);
    val g = Graph(ab :: bc :: ca :: Nil, a :: b :: c :: Nil)

    expect(Set(0,1,2)) {
      color(3,g).toSet;
    }
  }
}
