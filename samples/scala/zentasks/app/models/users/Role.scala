package models
import play.api.db._
import play.api.Play.current
import anorm._
import anorm.SqlParser._

case class Role (id: Pk[Long], name: String)

object Role {
    // -- Parsers
  
  /**
   * Parse a User from a ResultSet
   */
  val simple = {
    get[Pk[Long]]("Role.id") ~
    get[String]("Role.name") map {
      case id~name => Role(id, name)
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
       
       // Get the role id
       val id: Long = role.id.getOrElse {
         SQL("select next value for role_seq").as(scalar[Long].single)
       }
       
       // Insert the role
       SQL(
         """
           insert into role values (
             {id}, {name}
           )
         """
       ).on(
         'id -> id,
         'name -> role.name
       ).executeUpdate()
       
       role.copy(id = Id(id))
     }
  }
}