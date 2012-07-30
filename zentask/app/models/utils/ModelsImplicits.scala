package models.utils
import models.users.User
import org.neo4j.graphdb.Node
import org.neo4j.scala.Neo4jWrapper
import models.projects.Task
import models.projects.Project


object ModelsImplicits extends MyRestGraphDatabaseServiceProvider with Neo4jWrapper{


  implicit def user2node(user: User): Node = {
    withTx {
      implicit neo =>
      	getNodeIndex("USER").get.query("email", user.email).getSingle
    }      
  }

  implicit def task2node(task: Task): Node = {
    withTx {
      implicit neo =>
      	getNodeIndex("TASK").get.query("folder+project", task.folder + "+" + task.project).getSingle
    }      
  }

  implicit def project2node(project: Project): Node = {
    withTx {
      implicit neo =>
      	getNodeIndex("PROJECT").get.query("folder+name", project.folder + "+" + project.name).getSingle
    }      
  }

}