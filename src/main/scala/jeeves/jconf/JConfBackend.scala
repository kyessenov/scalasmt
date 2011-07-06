package cap.jeeves.jconf

import scala.collection.mutable.Map

import cap.scalasmt._
import cap.jeeves._

object JConfBackend extends JeevesLib {
  private var papers : Map[Int, PaperRecord] = Map();

  /* Making papers. */
  private var _papercount = 0;
  private def getPaperUid () : Int = {
    val count = _papercount;
    _papercount = _papercount + 1;
    count
  }
  def mkPaper(name : Title, authors : List[ConfUser], tags : List[PaperTag])
      : PaperRecord = {
    val paper = new PaperRecord(getPaperUid(), name, authors, tags);
    papers += (paper.id -> paper);
    paper
  }
 
  /* Updating papers. */
  def addTag (uid : Int, tag : PaperTag) : Unit = {
    papers.get(uid) match {
      case Some(p) => p.addTag(tag)
      case None => ()
    }
  }
  
  def addReview (uid : Int, reviewer : ConfUser, review : ReviewBody) {
    papers.get(uid) match {
      case Some(p) => p.addReview(reviewer, review)
      case None => ()
    }
  }

  /* Searching. */
  def getPaperByTag (tag : PaperTag) : List[Symbolic] = {
    val paperList = (papers.toList).map(x => x._2);
    filter(paperList, (p : PaperRecord) => CONTAINS(p.getTags (), tag))
  }
}
