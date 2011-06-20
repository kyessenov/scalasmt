package test.cap.jeeves.geoloc

import cap.jeeves._
import cap.jeeves.geoloc._
import cap.scalasmt._
import org.scalatest.FunSuite
import org.scalatest.Assertions.{expect}

import JeevesLib._

class ExampleGeoLocation extends FunSuite {
  val context = pickAtom();

  /*
  val author0 =
    new PaperRecord( 0, Constant(0), Constant(123), Constant(0)
                , LocationStatus.authorL, context);
  val reviewer0 =
    new PaperRecord( 1, Constant(1), Constant(456), Constant(0)
                , LocationStatus.reviewerL, context);
  val pc0 =
    new PaperRecord( 1, Constant(1), Constant(789), Constant(0)
                , LocationStatus.pcL, context);

  test ("name visibility") {
    expect (-1) { concretize(context, reviewer0, author0.pwd) };
    expect (123) { concretize(context, author0, author0.pwd) };
  }
  */
}
