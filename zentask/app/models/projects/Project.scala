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

case class Project(name: String, ownerEmail: String, folder: String = "", description: String = "")

object Project extends Neo4jWrapper with MyRestGraphDatabaseServiceProvider with RestTypedTraverser with TypedTraverser{
  
  // -- Parsers
  lazy val nodeIndex: Index[Node] = getNodeIndex("project").getOrElse{addNodeIndex("project").get}
  lazy val relationshipIndex: Index[Relationship] = getRelationIndex("project").getOrElse{addRelationshipIndex("project").get}

  
  // -- Queries
    
  /**
   * Retrieve a Project from id.
   */
  def findByNameAndFolder(projectName: String, folder: String): Option[Project] = withTx{
    implicit neo => {
      nodeIndex.get("name+folder", projectName + "+" + folder).getSingle().toCC[Project]
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
  def rename(project: Project, newName: String) = withTx {
    implicit neo => {
      val oldName = project.name
      project.setProperty("name", newName)
      Logger.debug("project id: " + project.getId() + " renamed from " + oldName + " to " + project.name)
    }
  }
  
  /**
   * Delete a project.
   */
  def delete(project: Project) = withTx {
    implicit neo => {
      val projectId = project.getId 
      project.delete()
      Logger.debug("deleted project id: " + projectId)
    }
  }
  
  /**
   * Retrieve project member
   */
  def membersOf(project: Project): Seq[User] = withTx{
    implicit neo => {
      (for (node <- project.getRelationships("member", "-->"); t = node.getStartNode().toCC[User].get) yield t) toSeq
    }
  }
  
  /**
   * Add a member to the project team.
   */
  def addMember(project: Project, userEmail: String) = withTx {
    implicit neo => {
      User.findByEmail(userEmail).map { member =>
        val nodeRel: Relationship = project.createRelationshipTo(member, "member")
	    Logger.debug("new member relatioship created for project id: " + project.getId() + " and user " + member.email + ", relationship id: " + nodeRel.getId())
	    relationshipIndex.add(nodeRel, "member", project) 
	    Logger.debug("new member relatioship added to index")
	    val memberRel: Relationship = member.createRelationshipTo(project, "member of")
	    Logger.debug("new member-of relatioship created for project id: " + project.getId() + " and user " + member.email + ", relationship id: " + memberRel.getId())
	    relationshipIndex.add(memberRel, "member of", project) 
        Logger.debug("new member-of relatioship added to index")
      }
    }
  }
  
  /**
   * Remove a member from the project team.
   */
  def removeMember(project: Project, userEmail: String) = withTx {
    implicit neo => {
      
    }
  }
  
  /**
   * Check if a user is a member of this project
   */
  def isMember(project: Project, userEmail: String): Boolean  = withTx {
    implicit neo => {
      User.findByEmail(userEmail).map { user =>
        membersOf(project).contains(user)
      }
      false
    }
  }
   
  /**
   * Create a Project.
   */
  def create(project: Project, members: Seq[User]): Option[Project] = {
    findByNameAndFolder(project.name, project.folder) match {
      case _ => {
        Logger.debug("project found in DB")
        None 
      }
    }
    withTx {
      implicit neo => {
	    val node = createNode(project)
	    Logger.debug("database node create for task id " + node.getId)
	    User.findByEmail(project.ownerEmail).map { user =>
	      val userIndex = getNodeIndex("user").getOrElse{addNodeIndex("user").get}
	      val nodeRel: Relationship = node.createRelationshipTo(user, "owned by")
	      Logger.debug("new owned-by relatioship created for project id: " + node.getId() + " and user " + user.email + ", relationship id: " + nodeRel.getId())
	      relationshipIndex.add(nodeRel, "owned by", node) 
	      Logger.debug("new owned-by relatioship added to index")
	      val userRel: Relationship = user.createRelationshipTo(node, "owner of")
	      Logger.debug("new owner-of relatioship created for user " + user.email + " and roject id: " + project.getId() + " , relationship id: " + userRel.getId())
	      relationshipIndex.add(userRel, "owner of", user)
	      Logger.debug("new owner-of relatioship added to index")
	    }
		nodeIndex.add(node, "name+folder", project.name + "+" + project.folder)
	    Logger.debug("new project node added to name+folder index")
		nodeIndex.add(node, "nodeId", node.getId())
	    Logger.debug("new project node added to nodeId index")
	    
	    for (member <- members) {
	      val nodeRel: Relationship = node.createRelationshipTo(member, "member")
	      Logger.debug("new member relatioship created for project id: " + node.getId() + " and user " + member.email + ", relationship id: " + nodeRel.getId())
	      relationshipIndex.add(nodeRel, "member", node) 
	      Logger.debug("new member relatioship added to index")
	      val memberRel: Relationship = member.createRelationshipTo(node, "member of")
	      Logger.debug("new member-of relatioship created for project id: " + node.getId() + " and user " + member.email + ", relationship id: " + memberRel.getId())
	      relationshipIndex.add(memberRel, "member of", node) 
	      Logger.debug("new member-of relatioship added to index")
	    }
	    
	    Neo4jWrapper.toCC[Project] (node)
      }      
    } 
  }
}