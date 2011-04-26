package test.cap.scalasmt

import cap.scalasmt._
import org.scalatest.FunSuite
import org.scalatest.Assertions.{expect}

class ExampleSceeves extends FunSuite with Sceeves {
  test ("defer") {
    val x = defer (x => x === 1);
    expect(1) {concretize(x)};
    expect(1) {concretize(x)};
  }

  test ("defer 2") {
    val y = defer (y => y > 0);
    val z = defer (z => y === z && z === 1);
    expect(1) {concretize(z)};
    expect(1) {concretize(y)};
  }

  test ("defer many") {
    val x = defer (x => x > 0);
    val y = defer (y => y > 0);
    val z = defer (z => z > 0);
    assert(x + y + z === 3);
    expect(1) {concretize(x)};
    expect(1) {concretize(y)};
    expect(1) {concretize(z)}; 
  }

  test ("conditional") {
    val x = defer (x => x > 1337);
    val y: Expr = IF (x > 1337) {1} ELSE {0};
    expect(1) {concretize(y)};
  }

  val M = 3;
  val N = M * M;
 
  def sudoku(input: String) = {
    val s = Array.ofDim[Var](N,N);
    for (i <- 0 until N; j <- 0 until N) s(i)(j) = defer (x => x > 0 && x <= N)
    
    def distinct(vs: Traversable[Var]) =
      for (vs1 <- vs; vs2 <- vs; if (vs1 != vs2)) assert ( ! (vs1 === vs2))

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
}
