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
import org.neo4j.scala.DatabaseService
import org.neo4j.scala.DatabaseServiceImpl

case class Task(id: Long, name: String, folder: String, projectId: Long, done: Boolean, description: Option[String] = None, dueDate: Option[DateTime] = None, ownerId: Option[Long] = None){
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
  
  def findByNodeId(id: Long): Option[Task] = withTx {
    implicit neo => {
      Neo4jWrapper.toCC[Task](getNodeById(id))
    }
  }

  
  /**
   * Retrieve all tasks for a project.
   */
  def findByProject(projectId: Long): Seq[Task] = {
    withTx {
      implicit neo => {
        getReferenceNode.doTraverse[Task](follow ->- "TASK") {
          END_OF_GRAPH
        } {
          case (x: Task, _) => x.projectId.equals(projectId) 
          case _ => false
        }.toList
      }
    }
  }
  
  def isOwner(taskId: Long, userId: Long): Boolean = withTx {
    implicit neo => {
      User.findByNodeId(userId).map { user =>
        findByNodeId(taskId) map { task =>
          task.ownerId == user.id
        }
      }
    }
    false
  }
  
  /**
   * Retrieve tasks for the user.
   */
  def findInvolving(userEmail: String): Option[Seq[Task]] = withTx {
    implicit neo => {
      User.findByEmail(userEmail).map { user =>
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
  def findByNameAndProjectAndFolder(taskName: String, folderName: String, projectId: Long): Option[Task] = withTx {    
    implicit neo => {
      nodeIndex.get("name+folder+project", taskName + "+" + folderName + "+" + projectId).getSingle().toCC[Task]
    }
  }

  /**
   * Delete a task
   */
  def delete(taskId: Long) = withTx {
    implicit neo => {
      findByNodeId(taskId) map { task: Task =>
        nodeIndex.remove(task)
        Logger.debug("removed task id: " + taskId + " from node index")
        /***
         * TODO: remove all relationships (dependencies, owner) and indices
         ***/
        task.delete()
        Logger.debug("deleted task id: " + taskId)        
      }
    } 
  }
    
  /**
   * Delete all task in a folder.
   */
  def deleteAllInFolder(projectId: Long, folder: String) = withTx {
    implicit neo => {
      for (task <- findAll if task.projectId == projectId && task.folder.equals(folder))
        delete(task.id)
    } 
  }
  
  def renameFolder(projectId: Long, folder: String, newFolderName: String): Seq[Task] = withTx {
    implicit neo => {
      val taskIdSeq = for (task <- findAll if task.projectId == projectId && task.folder.equals(folder)) yield task.id
      taskIdSeq.foreach { taskId =>
        moveFolder(taskId, newFolderName)
      }
      for (taskId <- taskIdSeq) yield findByNodeId(taskId) match {
        case task: Task => task
      }
    } 
  }
  
  def moveFolder(taskId: Long, newFolderName: String): Option[Task] = withTx {
    implicit neo => {
      findByNodeId(taskId).map { task =>
        task.asInstanceOf[Node].setProperty("folder", newFolderName)
        task
      }
    }
  }
  
  /**
   * Mark a task as done or not
   */
  def markAsDone(task: Task, done: Boolean) = withTx {
    implicit neo => {
      task.setProperty("done", done)
      Logger.debug("task id: " + task.getId() + " done set to " + done)
    } 
  }
  
  def setOwner (task: Task, ownerId: Option[Long]) = withTx{
    Logger.debug("setOwner")
    implicit neo => {
      val userIndex = getNodeIndex("user").getOrElse{addNodeIndex("user").get}
      if (relationshipIndex.get("task->owner", task).size() == 0){
          Logger.debug("task->owner relationship not found")  
      } else {
        for (taskRel <- relationshipIndex.get("task->owner", task)){
          Logger.debug("task->owner relationship acquired, id: " + taskRel.getId())
          relationshipIndex.get("owner->task", taskRel.getStartNode()).find(_.getStartNode() == task).map { ownerRel =>
            Logger.debug("owner->task relationship acquired, id: " + ownerRel.getId())
            relationshipIndex.remove(ownerRel)
            Logger.debug("owner->task relationship removed from index")
            ownerRel.delete 
            Logger.debug("owner->task relationship deleted")
          } 
          relationshipIndex.remove(taskRel)
          Logger.debug("task->owner relationship removed from index")
          taskRel.delete
          Logger.debug("task->owner relationship deleted")  
        }
      }
      ownerId map { newOwnerId =>
        User.findByNodeId(newOwnerId) map { user =>
          Logger.debug("Setting task ownerId to " + newOwnerId)
          task.setProperty("ownerId", newOwnerId)
        
          val newTaskRel: Relationship = task.createRelationshipTo(user, "task->owner")
          Logger.debug("new task->owner relatioship created for task id: " + task.id + " and user id " + newOwnerId + ", relationship id: " + newTaskRel.getId())
          relationshipIndex.add(newTaskRel, "task->owner", task) 
          Logger.debug("new task->owner relatioship added to index")

          val newUserRel: Relationship = user.createRelationshipTo(task, "owner->task")
          Logger.debug("new owner->task relatioship created for user id " + newOwnerId + " and task id: " + task.id + " , relationship id: " + newTaskRel.getId())
          relationshipIndex.add(newUserRel, "owner->task", user) 
          Logger.debug("new owner->task relatioship added to index")
        } getOrElse {
          Logger.debug("ownerId not found in user database -> task owner set to none")                
        }        
      } getOrElse {
        Logger.debug("ownerId is empty -> task owner set to none")                
      }
    }
  }
  
  def update(taskId: Long, name: String, folder: String, done: Boolean, description: Option[String] = None, dueDate: Option[DateTime] = None, ownerId: Option[Long] = None): Option[Task] = {
    findByNodeId(taskId) map { taskNode =>
      Logger.debug("task found in DB. updating")
      createOrUpdate(Some(taskNode), name, folder, None, done, description, dueDate, ownerId)
    } getOrElse {
      Logger.debug("task was not found in DB. aborting")
      None
    }
  }

  def create(name: String, folder: String, projectId: Long, done: Boolean, description: Option[String] = None, dueDate: Option[DateTime] = None, ownerId: Option[Long] = None): Option[Task] = {
    createOrUpdate(None, name, folder, Some(projectId), done, description, dueDate, ownerId)
  }

  /**
   * Create a Task or update and existing one.
   */
  private def createOrUpdate(myTask: Option[Task], name: String, folder: String, projectId: Option[Long], done: Boolean, description: Option[String] = None, dueDate: Option[DateTime] = None, ownerId: Option[Long] = None): Option[Task] = {
    withTx {
      implicit neo => {
        val node: Node = myTask map { task =>
          Logger.debug("task with id " + task.id +" passed as parameter. updating")
          task.asInstanceOf[Node]
        } getOrElse{
          projectId map { myProjId =>
            Logger.debug("task not passed as parameter. creating")          
            val myNode = createNode("name", name)
            Logger.debug("database node created with id " + myNode.getId)          
	        myNode.setProperty("id", node.getId)
	        Logger.debug("node property set: id=" + node.getId)
    	    node.setProperty("projectId", myProjId)
	        Logger.debug("node property set: projectId=" + myProjId)
	        myNode            
          } getOrElse {
	        Logger.debug("no task or project id passed as parameters. returning None")
            return None
          }
        }
	    node.setProperty("folder", folder)
	    Logger.debug("node property set: folder=" + folder)
	    node.setProperty("done", done)
	    Logger.debug("node property set: done=" + done)
	    description map { desc =>
	      node.setProperty("description", desc)	      
	      Logger.debug("node property set: description=" + desc)
	    }
	    dueDate map { dueD =>
	      node.setProperty("dueDate", dueD)
	      Logger.debug("node property set: dueDate=" + dueD)
	    }
	    //creating the task if none existent or getting existing task.
	    //no need to check on projectId, because if both task and projectId were None, would have returned None above 
	    val task: Task = myTask.getOrElse{
	      val myTask = Task(node.getId, name, folder, projectId.get, done, description, dueDate, ownerId)
  		  nodeIndex.add(node, "name+folder+project", task.name + "+" + task.folder + "+" + task.projectId)
	      Logger.debug("new task node added to name+folder+project index")
		  nodeIndex.add(node, "nodeId", node.getId())
	      Logger.debug("new task node added to nodeId index")
	      myTask
        }
	    setOwner(task, ownerId)
	    Neo4jWrapper.toCC[Task] (node)
	  }
    }      
  }
}