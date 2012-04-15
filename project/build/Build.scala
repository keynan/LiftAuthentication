import sbt._

object Dependencies {

	val lift_webkit = "net.liftweb" %% "lift-webkit" % liftVersion.value.toString % "compile"
	val lift_mapper = "net.liftweb" %% "lift-mapper" % liftVersion.value.toString % "compile"
	//val jetty = "org.mortbay.jetty" % "jetty" % "6.1.26" % "test"
	val junit = "junit" % "junit" % "4.7" % "test"
	val testing_tools = "org.scala-tools.testing" %% "specs" % "1.6.8" % "test"
  
	val logbackVer = "0.9.26"
  val logbackclassic = "ch.qos.logback" % "logback-classic"  % logbackVer
}

object LiftAuthBuild extends Build {
	
	import Dependencies._;
  //val liftVersion = property[Version]

  // uncomment the following if you want to use the snapshot repo
  //  val scalatoolsSnapshot = ScalaToolsSnapshots

  // If you're using JRebel for Lift development, uncomment
  // this line
  // override def scanDirectories = Nil

  lazy val JavaNet = "Java.net Maven2 Repository" at "http://download.java.net/maven/2/"

  override def libraryDependencies = Set(
	  lift_webkit,
		lift_mapper,
		junit,
		testn_tools,
		logbackclassic
  ) ++ super.libraryDependencies
}
