import sbt._

class ScalaSMT(info: ProjectInfo) extends DefaultProject(info) {
  override def compileOptions = super.compileOptions ++ Seq(Unchecked, Deprecation)
  val scalatest = "org.scalatest" % "scalatest" % "1.3" % "test"
}
