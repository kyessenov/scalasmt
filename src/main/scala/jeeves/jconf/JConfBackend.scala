package cap.jeeves.jconf

import cap.scalasmt._
import cap.jeeves._

import scala.collection.mutable.Map
import scala.collection.mutable.Set

object JConfBackend extends JeevesLib {
  // We do not delegate integrity checking to Jeeves.
  var assignments : Map[Int, Set[ConfUser]] = Map[Int, Set[ConfUser]]()
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
 
  /* Reviews. */
  def assignReview (p: PaperRecord, reviewer: ConfUser): Unit = {
    if (!((reviewer.role == ReviewerStatus) || (reviewer.role == PCStatus)))
      return;
    assignments.get(p.id) match {
      case Some(reviewers) => reviewers += reviewer
      case None =>
        val reviewers = Set[ConfUser]();
        reviewers += reviewer;
        assignments += (p.id -> reviewers)
    };
  }
  def isAssigned (p: PaperRecord, reviewer: ConfUser): Boolean = {
    assignments.get(p.id) match {
      case Some(reviewers) => reviewers.contains(reviewer)
      case None => false
    }
  }
  def addReview
    (p: PaperRecord, reviewer: ConfUser, rtext: String, score: Int)
    : Unit = {
      if (isAssigned (p, reviewer))
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
    filter(papers, (p: PaperRecord) => p.authors.has(author))
  }
  def searchByTag (tag: PaperTag): List[Symbolic] = {
    filter(papers, (p: PaperRecord) => p.getTags().has(tag))
  }
}
