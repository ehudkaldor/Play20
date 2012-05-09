package models
import play.api.db._
import play.api.Play.current
import anorm._
import anorm.SqlParser._
import play.Logger

case class Role (name: String)

object Role {
    // -- Parsers
  
  /**
   * Parse a User from a ResultSet
   */
  val simple = {
    get[String]("Role.name") map {
      case name => Role(name)
    }
  }
  
    // -- Queries
  
  
  /**
   * Retrieve a Role from name.
   */
  def findByName(name: String): Option[Role] = {
    DB.withConnection { implicit connection =>
      SQL("select * from role where name = {name}").on(
        'name -> name
      ).as(Role.simple.singleOpt)
    }
  }
  
  /**
   * Retrieve all roles.
   */
  def findAll: Seq[Role] = {
    DB.withConnection { implicit connection =>
      SQL("select * from role").as(Role.simple *)
    }
  }
  
    /**
   * Create a Role.
   */
  def create(role: Role): Role = {
    DB.withTransaction { implicit connection =>
       
      // Query DB for role name
      SQL("select * from role where name = {name}").on(
        'name -> role.name   
      ).as(Role.simple.singleOpt) match {
        // role name found == role is already in DB
        case None => {
	      // Insert the role
	      SQL(
	        """
	          insert into role values (
	            {name}
	          )
	        """
	      ).on(
	        'name -> role.name
	      ).executeUpdate()
	       
	      Logger.debug("role name: " + role.name + " inserted to db")
	       
          role
        }
        case r => {
          Logger.debug("role name: " + role.name + " found in db")
          role
        }
      }       
    }
  }
}