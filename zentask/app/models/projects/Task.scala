package models.projects

import org.joda.time.DateTime
import org.neo4j.graphdb.index.Index
import org.neo4j.graphdb.Node
import org.neo4j.graphdb.Relationship
import org.neo4j.scala.Neo4jWrapper
import org.neo4j.scala.RestGraphDatabaseServiceProvider
import org.neo4j.scala.RestTypedTraverser
import org.neo4j.scala.TypedTraverser
import models.users.User
import models.utils.ModelsImplicits.task2node
import models.utils.ModelsImplicits.user2node
import models.utils.MyRestGraphDatabaseServiceProvider
import play.api.Play.current
import scala.collection.JavaConversions.iterableAsScalaIterable
import play.api.Logger

case class Task(name: String, folder: String, project: String, done: Boolean, description: Option[String] = None, dueDate: Option[DateTime] = None, owner: Option[String] = None){
  val creationTime: DateTime = DateTime.now()
}

object Task extends Neo4jWrapper with MyRestGraphDatabaseServiceProvider with RestTypedTraverser with TypedTraverser{

  lazy val nodeIndex: Index[Node] = getNodeIndex("task").getOrElse{addNodeIndex("task").get}
  lazy val relationshipIndex: Index[Relationship] = getRelationIndex("task").getOrElse{addRelationshipIndex("task").get}


  // -- Queries
  
  /**
   * Retrieve all tasks.
   */
  def findAll: Seq[Task] = {
    withTx {
      implicit neo => {
        getReferenceNode.doTraverse[Task](follow ->- "TASK") {
          END_OF_GRAPH
        } {
          case (x: Task, _) => true 
          case _ => false
        }.toList
      }
    }
  }
  
  /**
   * Retrieve tasks for the user.
   */
  def findInvolving(email: String): Option[Seq[Task]] = withTx {
    implicit neo => {
      User.findByEmail(email).map { user =>
        val userIndex = getNodeIndex("user").getOrElse{addNodeIndex("user").get}
        (for (node <- relationshipIndex.get("owner of", user); t = node.getStartNode().toCC[Task].get) yield t) toSeq
      } orElse {
        None
      }
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
    implicit neo => {
      (for (node <- relationshipIndex.get("preceding to", task); t = node.getStartNode().toCC[Task].get) yield t) toSeq
    }    
  }
  
  def isPreceding(dependent: Task, preceding: Task): Boolean = withTx {
    implicit neo => {
      getPreceding(dependent) contains(preceding)
    }
  }
  
  /**
   * Adds a preceding task to the dependent one
   * @param dependent the preceded task
   * @param preceding the preceding task
   */
  def addPreceding(dependent: Task, preceding: Task) = withTx {
    implicit neo => {
      val precedingToRel = preceding.createRelationshipTo(dependent, "preceding to")
      Logger.debug("created new precedeing-to relationship, preceding: " + preceding.getId() + " , dependent: " + dependent.getId())
      relationshipIndex.add(precedingToRel, "preceding to", dependent)
      Logger.debug("new precedeing-to relationship added to index")

      val precededByRel = preceding.createRelationshipTo(dependent, "preceded by")
      Logger.debug("created new preceded-by relationship, preceding: " + preceding.getId() + " , dependent: " + dependent.getId())
      relationshipIndex.add(precededByRel, "preceded by", preceding)
      Logger.debug("new preceded-by relationship added to index")
    }
  }
  
  /**
   * Find tasks related to a project
   */
  def findByNameAndProjectAndFolder(taskName: String, folderName: String, projectName: String): Option[Task] = withTx {    
    implicit neo => {
      nodeIndex.get("name+folder+project", taskName + "+" + folderName + "+" + projectName).getSingle().toCC[Task]
    }
  }

  /**
   * Find tasks related to a project
   */
  def findByNodeId(id: Long): Option[Task] = withTx {
    implicit neo => 
    	nodeIndex.get("nodeId", id).getSingle().toCC[Task]
  }

  /**
   * Delete a task
   */
  def delete(task: Task) = withTx {
    implicit neo => {
      val taskId = task.getId 
      task.delete()
      Logger.debug("deleted task id: " + taskId)
    } 
  }
  
//  /**
//   * Delete all task in a folder.
//   */
//  def deleteAllInFolder(projectName: String, folder: String) = withTx {
//    implicit neo => {
//      
//    } 
//  }
  
  /**
   * Mark a task as done or not
   */
  def markAsDone(task: Task, done: Boolean) = withTx {
    implicit neo => {
      task.setProperty("done", done)
      Logger.debug("task id: " + task.getId() + " done set to " + done)
    } 
  }
  
  def setOwner (task: Task, email:String) = withTx{
    implicit neo => {
      User.findByEmail(email) map { user =>
        Logger.debug("Setting task owner to " + user.email)
        task.setProperty("owner", email)
        val userIndex = getNodeIndex("user").getOrElse{addNodeIndex("user").get}
        val userRel: Relationship = relationshipIndex.get("owner of", task).getSingle()      
        Logger.debug("owner relationship acquired, id: " + userRel.getId())
        relationshipIndex.get("owned by", userRel.getStartNode()).find(_.getStartNode() == task).map { rel =>
          Logger.debug("owned relationship acquired, id: " + rel.getId())
          relationshipIndex.remove(rel)
          Logger.debug("owned relationship removed from index")
          rel.delete 
          Logger.debug("owned relationship deleted")
        } orElse {
          Logger.debug("owner relationship not found")
          Some(None)
        }
        relationshipIndex.remove(userRel)
        Logger.debug("owner relationship removed from index")
        userRel.delete
        Logger.debug("owner relationship deleted")
        
        val newTaskRel: Relationship = task.createRelationshipTo(user, "owned by")
        Logger.debug("new owned-by relatioship created for task id: " + task.getId() + " and user " + user.email + ", relationship id: " + newTaskRel.getId())
        relationshipIndex.add(newTaskRel, "owned by", task) 
        Logger.debug("new owned-by relatioship added to index")

        val newUserRel: Relationship = user.createRelationshipTo(task, "owner of")
        Logger.debug("new owner-of relatioship created for user " + user.email + " and task id: " + task.getId() + " , relationship id: " + newTaskRel.getId())
        relationshipIndex.add(newUserRel, "owner of", user) 
        Logger.debug("new owner-of relatioship added to index")
      }
      
    }
  }
  
  /**
   * Create a Task.
   */
  def create(task: Task): Option[Task] = {
    findByNameAndProjectAndFolder(task.name, task.folder, task.project) match {
      case _ => {
        Logger.debug("task not found")
        None 
      }
    }
    withTx {
    implicit neo => 
      val node = createNode(task)
      Logger.debug("database node create for task id " + node.getId)
      User.findByEmail(task.owner.get).map { user =>
        val userIndex = getNodeIndex("user").getOrElse{addNodeIndex("user").get}
        val nodeRel: Relationship = node.createRelationshipTo(user, "owned by")
        Logger.debug("new owned-by relatioship created for task id: " + node.getId() + " and user " + user.email + ", relationship id: " + nodeRel.getId())
        relationshipIndex.add(nodeRel, "owned by", node) 
        Logger.debug("new owned-by relatioship added to index")
        val userRel: Relationship = user.createRelationshipTo(node, "owner of")
        Logger.debug("new owner-of relatioship created for user " + user.email + " and task id: " + task.getId() + " , relationship id: " + userRel.getId())
        relationshipIndex.add(userRel, "owner of", user)
        Logger.debug("new owner-of relatioship added to index")
      }
	  nodeIndex.add(node, "name+folder+project", task.name + "+" + task.folder + "+" + task.project)
      Logger.debug("new taks node added to name+folder+project index")
	  nodeIndex.add(node, "nodeId", node.getId())
      Logger.debug("new taks node added to nodeId index")
      Neo4jWrapper.toCC[Task] (node)
    }      
  }
}