import sbt._

class Project(info:ProjectInfo) extends DefaultProject(info) {

  def extraResources = "LICENSE"
  
  override def mainResources = super.mainResources +++ extraResources

  val servlet = "javax.servlet" % "servlet-api" % "2.5" % "provided"

  val scalatest = "org.scalatest" % "scalatest" % "1.2" % "test"

  val scalatra = "org.scalatra" %% "scalatra" % "2.0.0.M2" % "compile"

  val sonatypeNexusSnapshots = "Sonatype Nexus Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"

  val sonatypeNexusReleases = "Sonatype Nexus Releases" at "https://oss.sonatype.org/content/repositories/releases"


}
