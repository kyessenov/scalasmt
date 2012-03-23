package test.cap.jeeves

import cap.scalasmt._
import cap.jeeves._
import org.scalatest.FunSuite
import org.scalatest.Assertions.{expect}
import scala.collection.immutable.Map

class JeevesTutorial extends FunSuite with JeevesLib {
  case class StringVal (v: String) extends JeevesRecord

  /**
   * Jeeves equivalent of "Hello world."
   */
  case class User (id: Int) extends JeevesRecord

  private var _id = 0
  private def getNextUid (): Int = {
    val id = _id
    _id = _id + 1
    id
  }

  // Some users.
  val alice = User (getNextUid ());
  val bob = User (getNextUid ())
  val claire = User (getNextUid ())

  test ("Jeeves hello world") {
    val aliceName = StringVal("Alice")
    val anonymousName = StringVal("Anonymous")

    val name: Symbolic = {
      val a = mkLevel ();
      policy(a, !(CONTEXT === alice), LOW)
      mkSensitive(a // Level variable
        , aliceName     // High-confidentiality value
        , anonymousName // Low-confidentiality value
      )
    }

    expect(aliceName) { concretize(alice, name) }
    expect(anonymousName) { concretize(bob, name) }
    expect(anonymousName) { concretize(claire, name) }
  }
  

  /**
   * Simple conference management example.
   */
  sealed trait UserRole extends JeevesRecord
  case object AuthorRole extends UserRole
  case object ReviewerRole extends UserRole
  case object PCRole extends UserRole

  case class Paper(
      private val title: String
    , private val author: User
    , private var isAccepted: Boolean ) extends JeevesRecord {
    // Level variables.
    private val _titleL = mkLevel()
    private val _acceptedL = mkLevel()

    private val _isInternalF: Formula = {
      ((CONTEXT.viewer.role === ReviewerRole)
      || (CONTEXT.viewer.role === PCRole))
    }

    policy(_titleL
      , !((CONTEXT.viewer === author) || _isInternalF
          || ((CONTEXT.stage === Public) && (getIsAccepted ())))
      , LOW)
    def getTitle() = {
      mkSensitive(_titleL, StringVal(title), StringVal(""))
    }
    def showTitle(ctxt: ConfContext): String = {
      concretize(ctxt, getTitle()).asInstanceOf[StringVal].v
    }

    // TODO: Make this sensitive.
    def setIsAccepted(accepted: Boolean): Unit = {
      isAccepted = accepted
    }
    def getIsAccepted(): Formula = {
      isAccepted
    }
    def showIsAccepted(ctxt: ConfContext): Boolean = {
      concretize(ctxt, getIsAccepted()).asInstanceOf[Boolean]
    }
  }

  sealed trait ConfStage extends JeevesRecord
  case object Submission extends ConfStage
  case object Review extends ConfStage
  case object Decision extends ConfStage
  case object Public extends ConfStage

  case class ConfContext(viewer: User, stage: ConfStage) extends JeevesRecord

//  val paper0 = new Paper("Paper", alice)

  test ("title policy") {
  
  }
}
