package models.users

import org.neo4j.graphdb.Node
import org.neo4j.scala.Neo4jWrapper
import org.neo4j.scala.RestTypedTraverser
import org.neo4j.scala.TypedTraverser
import models.utils.MyRestGraphDatabaseServiceProvider
import models.utils.ModelsImplicits.user2node
import org.neo4j.graphdb.index.Index
import org.neo4j.graphdb.Relationship

import play.api.Logger

case class User(id: Long, email: String, password: String, firstName: String = "", lastName: String = "", role: Role, isActivated: Boolean = false) {    
}

object User extends Neo4jWrapper with MyRestGraphDatabaseServiceProvider with RestTypedTraverser with TypedTraverser{

  lazy val nodeIndex: Index[Node] = getNodeIndex("user").getOrElse{addNodeIndex("user").get}
  lazy val relationshipIndex: Index[Relationship] = getRelationIndex("user").getOrElse{addRelationshipIndex("user").get}

  // -- Parsers	
  
  // -- Queries
  
  /**
   * Retrieve a User by email.
   */
  def findByEmail(email: String): Option[User] = {
    Some[User](findAll.filter(_.email == email)(0))
  }
  
  def findByNodeId(id: Long): Option[User] = withTx {
    implicit neo => {
      Neo4jWrapper.toCC[User](getNodeById(id))
    }
  }
  
  /**
   * Retrieve all users.
   */
  def findAll: Seq[User] = {
    withTx {
      implicit neo => {
        getReferenceNode.doTraverse[User](follow ->- "USER") {
          END_OF_GRAPH
        } {
          case (x: User, _) => true 
          case _ => false
        }.toList.sortWith(_.email < _.email)
      }
    }
  }
  
  /**
   * Authenticate a User.
   */
  def authenticate(email: String, password: String): Boolean = {
    findByEmail(email).map { user =>
      user.password == passwordHash(password)    
    } 
    false
  }
  
  def exists(email: String): Boolean = {
    findByEmail(email).getOrElse {
      return true
    }
    false
  }
   
  /**
   * Create a User.
   */
  def create(email: String, clearPassword: String, firstName: String = "", lastName: String = "", role: Role = Role.MinimalRole.get, isActivated: Boolean = false): Either[Option[User], String] = {
    withTx {
      implicit neo => {
	    exists(email) match {
	      case true => {
            Logger.debug("user email " + email + " already exists")
            Right("user email " + email + " already exists")
	      }
	      case false => {
  	        val node: Node = createNode("email", email)
	        node.setProperty("id", node.getId)
	        node.setProperty("password", passwordHash(clearPassword))
	        node.setProperty("firstName", firstName)
    	    node.setProperty("lastName", lastName)
	        node.setProperty("roleName", role.name)
	        node.setProperty("isActived", isActivated)
	    
    	    User(node.getId, email, passwordHash(clearPassword), firstName, lastName, role, isActivated)
            Logger.debug("new node for user email: " + email + " created")
	        nodeIndex.add(node, "nodeId", node.getId())
    	    Logger.debug("new user node added to nodeId index")
	        nodeIndex.add(node, "email", email)
            Logger.debug("new user node added to email index")
        
            Left(findByNodeId(node.getId))
	      }
	    }	
      }
    }
  }
  
  def passwordHash(clearPass: String): String = {
    clearPass
  }
}