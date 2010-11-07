import sbt._

class Project(info:ProjectInfo) extends DefaultProject(info) {

  def extraResources = "LICENSE"
  
  override def mainResources = super.mainResources +++ extraResources

  val servlet = "javax.servlet" % "servlet-api" % "2.5" % "provided"

  val scalatest = "org.scalatest" % "scalatest" % "1.2" % "test"
  
}
