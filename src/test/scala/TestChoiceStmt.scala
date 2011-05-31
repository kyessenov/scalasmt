package test.cap.scalasmt

import cap.scalasmt._
import ChoiceStmt._
import org.scalatest.FunSuite
import org.scalatest.Assertions

class ExampleChoice extends FunSuite {
  test ("choice statement") {
    expect(-2) { 
      choose(x => (x + 2*3 - 4 === 0 && 1 > 0))   
    } 
    expect((1,2)) {
      choose((x,y) => x === 1 && y === 2)
    }
    expect((3,4)) {
      choose((x,y) => x + y === 7 && x < y && y < x*2)
    }
    expect(0) {
      choose(x => true)
    }
  }

  test ("multiplication") {
    expect(1) {
      choose (x => x*2 === 2)
    }
    expect((1,2)) {
      choose ((x,y) => x*y === 2 && x + y === 3 && x < y)
    }
  }
}
