package models.projects

import org.neo4j.graphdb.index.Index
import org.neo4j.graphdb.Node
import org.neo4j.graphdb.Relationship
import org.neo4j.scala.Neo4jWrapper
import org.neo4j.scala.RestGraphDatabaseServiceProvider
import org.neo4j.scala.RestTypedTraverser
import org.neo4j.scala.TypedTraverser
import models.users.User
import models.utils.MyRestGraphDatabaseServiceProvider
import models.utils.ModelsImplicits.{project2node, user2node}
import play.api.Play.current
import scala.collection.JavaConversions.iterableAsScalaIterable
import play.api.Logger
import org.neo4j.graphdb.Direction

case class Project(id: Long, name: String, ownerId: Long, description: Option[String] = None)

object Project extends Neo4jWrapper with MyRestGraphDatabaseServiceProvider with RestTypedTraverser with TypedTraverser{
  
  // -- Indices
  lazy val nodeIndex: Index[Node] = getNodeIndex("project").getOrElse{addNodeIndex("project").get}
  lazy val relationshipIndex: Index[Relationship] = getRelationIndex("project").getOrElse{addRelationshipIndex("project").get}

  
  // -- Queries
    
  /**
   * Retrieve a Project from id.
   */
  def findByName(projectName: String): Option[Project] = withTx{
    implicit neo => {
      nodeIndex.get("name", projectName).getSingle().toCC[Project]
    }
  }
  
  def findByNodeId(id: Long): Option[Project] = withTx {
    implicit neo => {
      Neo4jWrapper.toCC[Project](getNodeById(id))
    }
  }
  
  def isOwner(projectId: Long, userId: Long): Boolean = withTx{
    implicit neo => {
      findByNodeId(projectId) map { proj =>
        //return weather the project owner id is the user id parameter
        proj.ownerId == userId
      } getOrElse {
        //if we got here - project was not found, so return false
        false
      }
    }
  }

  def isMember(projectId: Long, userId: Long): Boolean = withTx{
    implicit neo => {
      findByNodeId(projectId).map { proj =>
	    ((for (rel <- relationshipIndex.get("project->member", proj) if rel.getEndNode.getId == userId) yield rel.getEndNode) toSeq).size > 0
        //return weather the project owner id is the user id parameter
      } getOrElse {
        //if we got here - project was not found, so return false
        false
      }
    }
  }

  /**
   * Retrieve all projects owned by user
   */
  def findInvolving(userEmail: String): Option[Seq[Project]] = withTx{
    implicit neo => {
      User.findByEmail(userEmail).map { user =>
        val userIndex = getNodeIndex("user").getOrElse{addNodeIndex("user").get}
        (for (node <- relationshipIndex.get("owner of", user); t = node.getStartNode().toCC[Project].get) yield t) toSeq
      } orElse {
        None
      }
    }    
  }
  
  /**
   * Rename a project.
   */
  def rename(projectId: Long, newName: String) = withTx {
    implicit neo => {
      findByNodeId(projectId) map { project =>
        val oldName = project.name
        project.setProperty("name", newName)
        val msg = "project id: " + projectId + " renamed from " + oldName + " to " + project.name
        Logger.debug(msg)
        (true, msg)
      } getOrElse{
        val msg = "project id:" + projectId + " was not found in database. aborting rename"
        Logger.debug(msg)
        (false, msg)
      }
    }
  }
  
  /**
   * Delete a project.
   */
  def delete(projectId: Long) = withTx {
    implicit neo => {
      val project = findByNodeId(projectId)
      project map { proj =>
        proj.delete()
        val msg = "deleted project id: " + projectId
        Logger.debug(msg)
        (true, msg)
      } getOrElse {
        val msg = "project id " + projectId + " was not found. aborting delete"
        Logger.debug(msg)
        (false, msg)
      }
    }
  }
  
  /**
   * Retrieve project member
   */
  def membersOf(projectId: Long): Option[Seq[User]] = withTx{
    implicit neo => {
      findByNodeId(projectId) map { project:Project =>
        (for (node <- project.getRelationships("member", "-->"); t = node.getStartNode().toCC[User].get) yield t) toSeq
      }
    }
  }
  
  
  def setOwner (project: Project, newOwnerId: Long) = withTx{
    Logger.debug("setOwner")
    implicit neo => {
      User.findByNodeId(newOwnerId) map { user =>
        Logger.debug("Setting project owner to " + user.email)
        project.setProperty("ownerId", newOwnerId)
        val userIndex = getNodeIndex("user").getOrElse{addNodeIndex("user").get}
        if (relationshipIndex.get("project->owner", project).size() == 0){
            Logger.debug("project->owner relationship not found")  
        } else {
          for (projRel <- relationshipIndex.get("project->owner", project)){
            Logger.debug("project->owner relationship acquired, id: " + projRel.getId())
            relationshipIndex.get("owner->project", projRel.getStartNode()).find(_.getStartNode() == project).map { ownerRel =>
              Logger.debug("owner->project relationship acquired, id: " + ownerRel.getId())
              relationshipIndex.remove(ownerRel)
              Logger.debug("owner->project relationship removed from index")
              ownerRel.delete 
              Logger.debug("owner->project relationship deleted")
            } 
            relationshipIndex.remove(projRel)
            Logger.debug("project->owner relationship removed from index")
            projRel.delete
            Logger.debug("project->owner relationship deleted")  
          } 
        }
        
        val newProjRel: Relationship = project.createRelationshipTo(user, "project->owner")
        Logger.debug("new project->owner relatioship created for project id: " + project.getId() + " and user " + user.email + ", relationship id: " + newProjRel.getId())
        relationshipIndex.add(newProjRel, "project->owner", project) 
        Logger.debug("new project->owner relatioship added to index")

        val newOwnerRel: Relationship = user.createRelationshipTo(project, "owner->project")
        Logger.debug("new owner->project relatioship created for user " + user.email + " and project id: " + project.getId() + " , relationship id: " + newOwnerRel.getId())
        relationshipIndex.add(newOwnerRel, "owner->project", user) 
        Logger.debug("new owner->project relatioship added to index")
      }
      
    }
  }

  /**
   * Add a member to the project team.
   */
  def addMember(projectId: Long, userId: Long) = withTx {
	Logger.debug("addMember")
    implicit neo => {
      findByNodeId(projectId) map { project =>
        User.findByNodeId(userId).map { member =>
	      Logger.debug("user " + member.email + " found")
          val nodeRel: Relationship = project.createRelationshipTo(member, "project->member")
	      Logger.debug("new project->member relatioship created for project id: " + project.getId() + " and user " + member.email + ", relationship id: " + nodeRel.getId())
  	      relationshipIndex.add(nodeRel, "project->member", project) 
	      Logger.debug("new project->member relatioship added to index")
	      val memberRel: Relationship = member.createRelationshipTo(project, "member->project")
	      Logger.debug("new member->project relatioship created for project id: " + project.getId() + " and user " + member.email + ", relationship id: " + memberRel.getId())
	      relationshipIndex.add(memberRel, "member->project", project) 
          Logger.debug("new member->project relatioship added to index")
          val msg = "user id:" + userId + " was was added as member to project id:" + projectId
          (true, msg)
        } getOrElse {
          val msg = "user id:" + userId + " was not found in database. aborting add member"
          (false, msg)
        }        
      } getOrElse {
        val msg = "project id:" + projectId + " was not found in database. aborting add member"
        (false, msg)
      }
    }
  }
  
  /**
   * Remove a member from the project team.
   */
  def removeMember(project: Project, userEmail: String) = withTx {
	Logger.debug("removeMember")
    implicit neo => {
      User.findByEmail(userEmail).map { user =>
	    Logger.debug("user " + userEmail + " found")
        for (projRel <- project.getRelationships(Direction.OUTGOING, "project->member") if projRel.getEndNode().eq(user)) {
          relationshipIndex.remove(projRel)
          Logger.debug("project->member relatioship removed from index. project id: " + project.getId() + " and user " + user.email + ", relationship id: " + projRel.getId())
          projRel.delete
      	  Logger.debug("project->member relatioship id: " + projRel.getId() + " deleted")
        }
        for (memberRel <- user.getRelationships(Direction.OUTGOING, "member->project") if memberRel.getEndNode().eq(project)) {
          relationshipIndex.remove(memberRel)
          memberRel.delete
          Logger.debug("member->project relatioship removed from index. project id: " + project.getId() + " and user " + user.email + ", relationship id: " + memberRel.getId())
          Logger.debug("member->project relatioship id: " + memberRel.getId() + " deleted")
        }
      }
    }
  }
  
  /**
   * Check if a user is a member of this project
   */
//  def isMember(project: Project, userEmail: String): Boolean  = withTx {
//    implicit neo => {
//      User.findByEmail(userEmail).map { user =>
//        membersOf(project).contains(user)
//      }
//      false
//    }
//  }
  
  def update(projectId: Long, name: String, ownerId: Long, description: Option[String]): Option[Project] = {
    None
  }
   
  /**
   * Create a Project.
   */
  def create(name: String, ownerId: Long, description: Option[String]): Option[Project] = {
    Logger.debug("create")
    findByName(name) match {
      case _ => {
        Logger.debug("project name found in DB: " + name)
        None 
      }
    }
    withTx {
      Logger.debug("project name not found in DB. creating")
      implicit neo => {
	    val node = createNode("name", name)
	    node.setProperty("id", node.getId)
	    description.map {desc =>
	      node.setProperty("description", desc)
	    }
	    val project = Project(node.getId, name, ownerId, description)
	    Logger.debug("database node create for project id " + node.getId)
	    setOwner(project, project.ownerId)
		nodeIndex.add(node, "name", project.name)
	    Logger.debug("new project node added to name+folder index")
		nodeIndex.add(node, "nodeId", node.getId())
	    Logger.debug("new project node added to nodeId index")
	    	    
	    Neo4jWrapper.toCC[Project] (node)
      } 
    }
  }
      }      