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

case class User(email: String, password: String, firstName: String = "", lastName: String = "", roleName: String, isActivated: Boolean = false) {    
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
  def create(user: User): Option[User] = {
    withTx {
      implicit neo => {
	    exists(user.email) match {
	      case true => {
            Logger.debug("user email " + user.email + " already exists")
            None
	      }
	    }	
	    val node: Node = createNode(
	        User(user.email, passwordHash(user.password), user.firstName, user.lastName, user.roleName, user.isActivated)
	    )
        Logger.debug("new node for user email: " + user.email + " created")
	    nodeIndex.add(node, "email", user.email)
        Logger.debug("new user node added to email index")
        nodeIndex.add(node, "nodeId", node.getId())
        Logger.debug("new user node added to nodeId index")
	    Neo4jWrapper.toCC[User](node)
      }
    }
  }
  
  def passwordHash(clearPass: String): String = {
    clearPass
  }
}