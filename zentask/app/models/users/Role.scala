package models.users

import play.api.db._
import play.api.Play.current
import play.Logger
import org.neo4j.scala.Neo4jWrapper
import models.utils.MyRestGraphDatabaseServiceProvider
import org.neo4j.scala.RestTypedTraverser
import org.neo4j.scala.TypedTraverser
import org.neo4j.graphdb.Node
import org.neo4j.graphdb.index.Index

case class Role (name: String)

object Role extends Neo4jWrapper with MyRestGraphDatabaseServiceProvider with RestTypedTraverser with TypedTraverser{

  lazy val index: Index[Node] = getNodeIndex("role").getOrElse{addNodeIndex("role").get}
  
  lazy val MinimalRole = create("minimal")

  
  // -- Queries

  def findByName(name: String): Option[Role] = {
        Some[Role](findAll.filter(_.name == name)(0))
  }
  
  def findByNodeId(id: Long): Option[Role] = withTx {
    implicit neo => {
      Neo4jWrapper.toCC[Role](getNodeById(id))
    }
  }

  
  /**
   * Retrieve all roles.
   */
  def findAll: Seq[Role] = {
    withTx {
      implicit neo => {
        getReferenceNode.doTraverse[Role](follow ->- "ROLE") {
          END_OF_GRAPH
        } {
          case (x: Role, _) => true 
          case _ => false
        }.toList.sortWith(_.name < _.name)
      }
    }
  }
  
    /**
   * Create a Role.
   */
  def create(roleName: String): Option[Role] = {
    withTx {
      implicit neo => {
	    findByName(roleName) match {
	      case _ => None
	    }
	    var node: Node = createNode(
	        Role(roleName)
	    )
	    index.add(node, "name", roleName)
	    Neo4jWrapper.toCC[Role](node)
      }
    }

  }
}