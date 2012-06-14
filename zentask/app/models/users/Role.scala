package models.users

import play.api.db._
import play.api.Play.current
import anorm._
import anorm.SqlParser._
import play.Logger
import org.neo4j.scala.Neo4jWrapper
import models.utils.MyRestGraphDatabaseServiceProvider
import org.neo4j.scala.RestTypedTraverser
import org.neo4j.scala.TypedTraverser

case class Role (name: String)

object Role extends Neo4jWrapper with MyRestGraphDatabaseServiceProvider with RestTypedTraverser with TypedTraverser{
    // -- Queries
  
  
  /**
   * Retrieve a Role from name.
   */
  def findByName(name: String): Option[Role] = {
    None
  }
  
  /**
   * Retrieve all roles.
   */
  def findAll: Seq[Role] = {
    Seq()
  }
  
    /**
   * Create a Role.
   */
  def create(role: Role): Option[Role] = {
    None
  }
}