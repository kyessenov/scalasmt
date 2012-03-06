package cap.jeeves.jconf

import cap.scalasmt._
import cap.jeeves._

import scala.collection.mutable.Map
import scala.collection.mutable.Set

import Expr._

object JConfBackend extends JeevesLib {
  private val usercache = ".jconfusers.cache"
  private val assignmentcache = ".jconfassignments.cache"
  private val papercache = ".jconfpapers.cache"

  private var users : Map[Username, ConfUser] = Map[Username, ConfUser]()
  private var assignments : Map[Int, Set[ConfUser]] = Map[Int, Set[ConfUser]]()
  private var papers : List[PaperRecord] = Nil

  def JConfBackend() {
    try {
      Persistence.readFromFile[Map[Username, ConfUser]](usercache);
      Persistence.readFromFile[Map[Int, Set[ConfUser]]](assignmentcache);
      Persistence.readFromFile[List[PaperRecord]](papercache)
    } catch {
      // There was no file, so we don't have to do anything.
      case e: Exception =>
        users = Map[Username, ConfUser]()
        assignments = Map[Int, Set[ConfUser]]()
        papers = Nil
    }

    Runtime.getRuntime().addShutdownHook(new Thread(new Runnable() {

      def run() {
        /* TODO: Save users. */
      }
    }));
  }

  /* Making papers. */
  private var _papercount = 0;
  private def getPaperUid () : Int = {
    val count = _papercount;
    _papercount = _papercount + 1;
    count
  }

  def addUser(newUser: ConfUser) = {
    users += (newUser.username -> newUser)
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
  def getById(id: Int) = 
    papers.find ((p: PaperRecord) => p.id == id)
  
  def searchByName(name: String) = 
    papers.filter(_.name === Title(name))
  
  def searchByAuthor(author: ConfUser) = 
    papers.filter(_.authors.has(author))
  
  def searchByTag(tag: PaperTag) = papers.filter(_.getTags().has(tag))
  
  /* More mundane logistical things. */
  def loginUser(id: String, password: String): Option[ConfUser] = {
    users.get(Username(id)) match {
      case Some(user) =>
        // Stage should not matter...
        val userCtxt = new ConfContext(user, Submission);
        /* TODO: Can we do this without concretizing here? */
        val pwd : Password =
          concretize(userCtxt, user.password).asInstanceOf[Password];
        println(pwd.pwd);
        if (pwd.pwd.equals(password)) Some(user) else None
      case None =>
        println("user not found");
        None
    }
  }
}
