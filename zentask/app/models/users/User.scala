package models.users

import java.net.URI

import org.neo4j.graphdb.Node
import org.neo4j.scala.{Neo4jWrapper, RestGraphDatabaseServiceProvider, RestTypedTraverser, TypedTraverser}

import play.api.Play.current

case class User(email: String, password: String, firstName: String = "", lastName: String = "", isActivated: Boolean = false, roleName: String)

object User extends AnyRef with Neo4jWrapper with RestGraphDatabaseServiceProvider with RestTypedTraverser with TypedTraverser{
  
  lazy val userList = {
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
  
  override def uri: URI = {
    new URI("localhost")
  }
  
  override def userPw: Option[(String, String)] ={
    Some("","")
  }
  
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
    val user: User = findByEmail(email).getOrElse {
      return false
    }
    user.password == passwordHash(password)
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
	      case true => None
	    }
	    var node: Node = createNode(
	        User(user.email, passwordHash(user.password), user.firstName, user.lastName, user.isActivated, user.roleName)
	    )
	    Neo4jWrapper.toCC[User](node)
      }
    }
  }
  
  def passwordHash(clearPass: String): String = {
    clearPass
  }
}