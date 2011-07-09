package cap.jeeves.jconf

import cap.scalasmt._
import cap.jeeves._

object JConfBackend extends JeevesLib {
  private var papers : List[PaperRecord] = Nil

  /* Making papers. */
  private var _papercount = 0;
  private def getPaperUid () : Int = {
    val count = _papercount;
    _papercount = _papercount + 1;
    count
  }
  def addPaper(name : Title, authors : List[ConfUser], tags : List[PaperTag])
      : PaperRecord = {
    val paper = new PaperRecord(getPaperUid(), name, authors, tags);
    papers = paper::papers;
    paper
  }
 
  /* Updating papers. */
  def addTag (p: PaperRecord, tag: PaperTag) : Unit = p.addTag(tag)
  def addReview
    (p: PaperRecord, reviewer: ConfUser, rtext: String, score: Int)
    : Unit = {
      p.addReview(reviewer, rtext, score)
  }

  /* Searching. */
  def getById (id: Int): Option[PaperRecord] = {
    papers.find ((p: PaperRecord) => p.id == id)
  }
  def searchByName (name: String): List[Symbolic] = {
    filter(papers, (p: PaperRecord) => p.name === Title(name))
  }
  def searchByAuthor (author: ConfUser) : List[Symbolic] = {
    filter(papers, (p: PaperRecord) => CONTAINS(p.authors, author))
  }
  def searchByTag (tag: PaperTag): List[Symbolic] = {
    filter(papers, (p: PaperRecord) => CONTAINS(p.getTags (), tag))
  }
}
