package models.projects

import java.util.{Date}
import play.api.db._
import play.api.Play.current
import org.neo4j.scala.{RestTypedTraverser, RestGraphDatabaseServiceProvider, Neo4jWrapper}
import models.users.User


case class Task(id: Long, folder: String, project: Long, title: String, done: Boolean, description: Option[String] = None, dueDate: Option[Date] = None, assignedTo: Option[String] = None)

object Task extends Neo4jWrapper with MyRestGraphDatabaseServiceProvider with RestTypedTraverser{
  

  // -- Queries
  
  /**
   * Retrieve a Task from the id.
   */
  def findById(id: Long): Option[Task] = withTx {
    implicit neo => 
    	Neo4jWrapper.toCC[Task](getNodeById(id))
  }
  
  /**
   * Retrieve tasks for the user.
   */
  def findInvolving(email: String): Seq[(Task,Project)] = withTx {
    implicit neo => {
      getNodeById(100)
      User.findByEmail(email)
      Seq()
    }
  }
  
  /**
   * Retrieves the tasks that were added as preceding to
   * the parameter. Note that these are only the tasks directly
   * preceding to this one, and not recursive 
   * @param task the task to query for preceding tasks
   * @return a Seq of preceding tasks
   */
  def getPreceding(task: Task): Seq[Task] = withTx {
    implicit neo => 
    	Seq[Task]()
  }
  
  def isPreceding(dependent: Task, preceding: Task): Boolean = withTx {
    implicit neo => 
    	false
  }
  
  /**
   * Adds a preceding task to the dependent one
   * @param dependent the preceded task
   * @param preceding the preceding task
   */
  def addPreceding(dependent: Task, preceding: Task) = withTx {
    implicit neo =>
  }
  
  /**
   * Find tasks related to a project
   */
  def findByProject(project: Long): Seq[Task] = withTx {
    implicit neo => 
    	Seq()
  }

  /**
   * Delete a task
   */
  def delete(id: Long) = withTx {
    implicit neo => 
  }
  
  /**
   * Delete all task in a folder.
   */
  def deleteInFolder(projectId: Long, folder: String) = withTx {
    implicit neo => 
  }
  
  /**
   * Mark a task as done or not
   */
  def markAsDone(taskId: Long, done: Boolean) = withTx {
    implicit neo => 
  }
  
  /**
   * Rename a folder.
   */
  def renameFolder(projectId: Long, folder: String, newName: String) = withTx {
    implicit neo => 
  }
  
  /**
   * Check if a user is the owner of this task
   */
  def isOwner(task: Long, user: String): Boolean = withTx {
    implicit neo => 
    	false
  }

  /**
   * Create a Task.
   */
  def create(task: Task): Task = withTx {
    implicit neo => 
    	Task(1, "", 1, "", false, None, None, None)
  }
}