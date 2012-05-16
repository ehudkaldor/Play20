package models

import play.api.db._
import play.api.Play.current

import anorm._
import anorm.SqlParser._

case class Project(id: Pk[Long], folder: String, name: String)

object Project {
  
  // -- Parsers
  
  /**
   * Parse a Project from a ResultSet
   */
  val simple = {
    get[Pk[Long]]("project.id") ~
    get[String]("project.folder") ~
    get[String]("project.name") map {
      case id~folder~name => Project(id, folder, name)
    }
  }
  
  // -- Queries
    
  /**
   * Retrieve a Project from id.
   */
  def findById(id: Long): Option[Project] = {
    DB.withConnection { implicit connection =>
      SQL("select * from project where id = {id}").on(
        'id -> id
      ).as(Project.simple.singleOpt)
    }
  }
  
  /**
   * Retrieve project for user
   */
  def findInvolving(user: User): Seq[Project] = {
    DB.withConnection { implicit connection =>
      SQL(
        """
          select * from project 
          join project_member on project.id = project_member.project_id 
          where project_member.user_email = {email}
        """
      ).on(
        'email -> user.email
      ).as(Project.simple *)
    }
  }
  
  /**
   * Update a project.
   */
  def rename(project: Project, newName: String) {
    DB.withConnection { implicit connection =>
      SQL("update project set name = {name} where id = {id}").on(
        'id -> project.id, 
        'name -> newName
      ).executeUpdate()
    }
  }
  
  /**
   * Delete a project.
   */
  def delete(project: Project) {
    DB.withConnection { implicit connection => 
      SQL("delete from project where id = {id}").on(
        'id -> project.id
      ).executeUpdate()
    }
  }
  
  /**
   * Delete all project in a folder
   */
  def deleteInFolder(folder: String) {
    DB.withConnection { implicit connection => 
      SQL("delete from project where folder = {folder}").on(
        'folder -> folder
      ).executeUpdate()
    }
  }
  
  /**
   * Rename a folder
   */
  def renameFolder(folder: String, newName: String) {
    DB.withConnection { implicit connection =>
      SQL("update project set folder = {newName} where folder = {name}").on(
        'name -> folder, 'newName -> newName
      ).executeUpdate()
    }
  }
  
  /**
   * Retrieve project member
   */
  def membersOf(project: Project): Seq[User] = {
    DB.withConnection { implicit connection =>
      SQL(
        """
          select user.* from user 
          join project_member on project_member.user_email = user.email 
          where project_member.project_id = {project}
        """
      ).on(
        'project -> project.id
      ).as(User.simple *)
    }
  }
  
  /**
   * Add a member to the project team.
   */
  def addMember(project: Project, user: User) {
    DB.withConnection { implicit connection =>
      SQL("insert into project_member values({project}, {user})").on(
        'project -> project.id,
        'user -> user.email
      ).executeUpdate()
    }
  }
  
  /**
   * Remove a member from the project team.
   */
  def removeMember(project: Project, user: User) {
    DB.withConnection { implicit connection =>
      SQL("delete from project_member where project_id = {project} and user_email = {user}").on(
        'project -> project.id,
        'user -> user.email
      ).executeUpdate()
    }
  }
  
  /**
   * Check if a user is a member of this project
   */
  def isMember(project: Project, user: User): Boolean = {
    DB.withConnection { implicit connection =>
      SQL(
        """
          select count(user.email) = 1 from user 
          join project_member on project_member.user_email = user.email 
          where project_member.project_id = {project} and user.email = {email}
        """
      ).on(
        'project -> project.id,
        'email -> user.email
      ).as(scalar[Boolean].single)
    }
  }
   
  /**
   * Create a Project.
   */
  def create(project: Project, owner: User, members: Seq[User]): Project = {
     DB.withTransaction { implicit connection =>
       
       // Get the project id
       val id: Long = project.id.getOrElse {
         SQL("select next value for project_seq").as(scalar[Long].single)
       }
       
       // Insert the project
       SQL(
         """
           insert into project values (
             {id}, {name}, {folder}
           )
         """
       ).on(
         'id -> id,
         'name -> project.name,
         'folder -> project.folder
       ).executeUpdate()
       
       // Insert project owner
       SQL("insert into project_owner values ({id}, {email})").on('id -> id, 'email -> owner.email).executeUpdate()
       
       // Add members
       members.foreach { user =>
         SQL("insert into project_member values ({id}, {email})").on('id -> id, 'email -> user.email).executeUpdate()
       }
       
       project.copy(id = Id(id))       
     }
  }  
}