import sbt._
import Keys._

import PlayProject._

object ApplicationBuild extends Build {

  val appName         = "zentask"
  val appVersion      = "0.1"

  /* Cloudbees plublic snapshot REPO */
  //val cloudbeesRepo = "Cloudbees public snapshot" at "https://repository-andy-petrella.forge.cloudbees.com/snapshot"
//  val fakodSnapRepo = "fakod-snapshots" at "https://raw.github.com/ehudkaldor/fakod-mvn-repo/master/snapshots/"	
//  val fakodRelRepo = "fakod-release" at "https://raw.github.com/ehudkaldor/fakod-mvn-repo/master/releases"
  									   //https://github.com/FaKod/fakod-mvn-repo/tree/master/releases/org/neo4j/neo4j-scala/0.2.0-RC1
  val neo4jRepo = "neo4jRepo" at "http://m2.neo4j.org/content/groups/everything" 

  /* LOCAL MAVEN REPO */
  val localMavenRepo = "Local Maven Repository" at "file:///home/ehud/.m2/repository"

  val appDependencies = Seq(
//    "be.nextlab" %% "neo4j-rest-play-plugin" % "0.0.1-SNAPSHOT",
//
//
//    "org.specs2" %% "specs2" % "1.8.2" % "test" withSources,
//    "be.nextlab" %% "gatling-play2-plugin" % "0.0.1-SNAPSHOT" % "test"
      "org.neo4j" % "neo4j-kernel" % "1.7",
      "org.neo4j" % "neo4j-shell" % "1.7",
      "org.neo4j" % "neo4j-rest-graphdb" % "1.8-SNAPSHOT",
      "org.neo4j" % "neo4j-scala" % "0.2.0-M1"
  )
  
//  // Only compile the bootstrap bootstrap.less file and any other *.less file in the stylesheets directory
//  def customLessEntryPoints(base: File): PathFinder = (
//      (base / "app" / "assets" / "stylesheets" / "bootstrap" * "bootstrap.less") +++
//      (base / "app" / "assets" / "stylesheets" * "*.less")
//    )

  val main = PlayProject(appName, appVersion, appDependencies, mainLang = SCALA).settings(
    resolvers ++= Seq(localMavenRepo,neo4jRepo)
//    lessEntryPoints <<= baseDirectory(customLessEntryPoints)
  )
//
}
            
	
