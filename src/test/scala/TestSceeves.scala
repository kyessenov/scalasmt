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
      intercept[Inconsistent.type] {
        println(concretize(x));
      }
    }
  }

  test ("overflow") {
    new Sceeves {
      val x = pick (_ === Int.MaxValue);
      val y = pick (_ === x + 1);
      intercept[java.lang.NumberFormatException] {
        println(concretize(y));
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
    val y = (x > 1337) ? 1 ! 0;
    expect(1) {concretize(y)};
  }

  val M = 3;
  val N = M * M;
 
  def sudoku(input: String) = {
    val s = Array.ofDim[IntVar](N,N);
    for (i <- 0 until N; j <- 0 until N) 
      s(i)(j) = pick (x => x > 0 && x <= N)
    
    // all rows are distinct
    for (i <- 0 until N) assume(DISTINCT(s(i)))

    // all columns are distinct
    for (j <- 0 until N) assume(DISTINCT(for (i <- 0 until N) yield s(i)(j)))

    // all M blocks are distinct
    for (mi <- 0 until M;
         mj <- 0 until M)
      assume(DISTINCT(for (i <- 0 until M; j <- 0 until M) 
                yield s(M*mi + i)(M*mj + j)))
    
    assert (input.length == N * N);
    for (i <- 0 until N; 
         j <- 0 until N;
         c = input(i*N + j);
         if c != '0')
      assign(s(i)(j), c.toString.toInt);
 
    for (i <- 0 until N; j <- 0 until N) concretize(s(i)(j));

    for (i <- 1 to N) 
      expect(N) {s.map(s => s.count(v => concretize(v) == i)).sum}
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

  test ("sendmoremoney") {
    def digit = pick (x => x >= 0 && x <= 9);
    val vars = (1 to 8).toList.map(_ => digit);
    val List(s,e,n,d,m,o,r,y) = vars;
     
    assume (s !== 0)
    assume (m !== 0)
    assume (DISTINCT(vars))

    val send = s*1000 + e*100 + n*10 + d;
    val more = m*1000 + o*100 + r*10 + e;
    val money = m*10000 + o*1000 + n*100 + e*10 + y;
    assume (send + more === money);

    expect (List(9, 5, 6, 7, 1, 0, 8, 2)) {
      vars.map(concretize(_))
    }
  }

  test ("defaults") {
    val x = pick(1, _ > 0);
    expect(1) {concretize(x)}

    val y = pick(1, _ > 0);
    assume(y === 2);
    expect(2) {concretize(y)}

    val z = pick(y, _ > 0);
    expect(2) {concretize(z)}
  }

  test ("euler") {
    // problem 9
    val a = pick(_ > 0);
    val b = pick(_ > a);
    val c = pick(_ > b);
    assume(a + b + c === 1000);
    assume(a*a + b*b === c*c);
    assume(a === 200);
    assume(b === 375);
    assume(c === 425);

    expect(31875000) {concretize(a*b*c)}
  }

  test ("context") {
    val key = pick(_ > 0);
    val hidden = (key === 1) ? 1 ! 0;
    expect(1) {let(key, 1).concretize(hidden)}
    expect(0) {let(key, 2).concretize(hidden)}
    intercept[Inconsistent.type] {
      let(key, 0).concretize(hidden)
    }
  }

  test ("queens") {
    val N = 16;

    val cells = (1 to N).map{_ => 
      (pick(x => x >= 0 && x < N), pick(y => y >= 0 && y < N))
    }.toList;

    assume (DISTINCT(cells.map (_._1)));
    assume (DISTINCT(cells.map (_._2)));
    assume (DISTINCT(cells.map (c => c._1 + c._2)));
    assume (DISTINCT(cells.map (c => c._1 - c._2)));

    // break symmetries
    for (i <- 0 until N)
      assign(cells(i)._1, i);

    def spaces(sp: Int) = "".padTo(sp, '.');
    
    for (j <- 0 until N) 
      cells.find(c => concretize(c._1) == j) match {
        case Some(c) => 
          val i = concretize(c._2);
          print(spaces(i));
          print("*");
          print(spaces(N-i-1));
          println;
        case None => 
          fail("Constraints are wrong")
      }
  }
}
