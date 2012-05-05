import sbt._
import Keys._

import PlayProject._

object ApplicationBuild extends Build {

  val appName         = "zentask"
  val appVersion      = "0.1"

  /* Cloudbees plublic snapshot REPO */
  val cloudbeesRepo = "Cloudbees public snapshot" at "https://repository-andy-petrella.forge.cloudbees.com/snapshot"

  val appDependencies = Seq(
    "be.nextlab" %% "neo4j-rest-play-plugin" % "0.0.1-SNAPSHOT"
  )

//  val main = PlayProject(appName, appVersion, mainLang = SCALA)
  val main = PlayProject(appName, appVersion, appDependencies, mainLang = SCALA).settings(
    resolvers ++= Seq(cloudbeesRepo)
  )

}
            
