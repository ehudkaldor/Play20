package models.users

import play.api.db._
import play.api.Play.current
import anorm._
import anorm.SqlParser._
//import be.nextlab.play.neo4j.rest.{Relation, CypherResult, Neo4JEndPoint, Node}
import play.api.libs.json.{Format, JsValue,JsObject, JsString, JsBoolean}

case class User(email: String, password: String, firstName: String = "", lastName: String = "", isActivated: Boolean = false, roleName: String)

object User {
  
  implicit object UserJsonFormat extends Format[User]{
    def reads(json: JsValue) = User(
      (json \ "email").as[String],
      (json \ "password").as[String],
      (json \ "firstName").as[String],
      (json \ "lastName").as[String],
      (json \ "isActivated").as[Boolean],
      (json \ "roleName").as[String]
      )
      
    def writes(user: User) = JsObject(Seq(
      "email" -> JsString(user.email),
      "password" -> JsString(user.password),
      "firstName" -> JsString(user.firstName),
      "lastName" -> JsString(user.lastName),
      "isActivated" -> JsBoolean(user.isActivated),
      "roleName" -> JsString(user.roleName)
    ))
  }
  
  // -- Parsers
  
  /**
   * Parse a User from a ResultSet
   */
  val simple = {
    get[String]("user.email") ~
    get[String]("user.password") ~
    get[String]("user.firstName") ~
    get[String]("user.lastName") ~
    get[Boolean]("user.isActivated") ~
    get[String]("user.roleName") map {
      case email~password~firstName~lastName~isActivated~roleName => User(email, password, firstName, lastName, isActivated, roleName)
    }
  }
  
  // -- Queries
  
  /**
   * Retrieve a User from email.
   */
  def findByEmail(email: String): Option[User] = {
    DB.withConnection { implicit connection =>
      SQL("select * from user where email = {email}").on(
        'email -> email
      ).as(User.simple.singleOpt)
    }
  }
  
  /**
   * Retrieve all users.
   */
  def findAll: Seq[User] = {
    DB.withConnection { implicit connection =>
      SQL("select * from user").as(User.simple *)
    }
  }
  
  /**
   * Authenticate a User.
   */
  def authenticate(email: String, password: String): Option[User] = {
    DB.withConnection { implicit connection =>
      SQL(
        """
         select * from user where 
         email = {email} and password = {password}
        """
      ).on(
        'email -> email,
        'password -> password
      ).as(User.simple.singleOpt)
    }
  }
  
  def exists(email: String): Boolean = {
    DB.withConnection { implicit connection =>
      SQL(
        """
         select * from user where 
         email = {email}
        """
      ).on(
        'email -> email
      ).as(User.simple.singleOpt).isDefined
    }    
  }
   
  /**
   * Create a User.
   */
  def create(user: User): Option[User] = {
    DB.withConnection { implicit connection =>
      findByEmail(user.email) match {
        case None => {
	      SQL(
	        """
	          insert into user values (
	            {email}, {password}, {firstName}, {lastName}, {isActivated}
	          )
	        """
	      ).on(
	        'email -> user.email,
	        'password -> user.password,
	        'firstName -> user.firstName,
	        'lastName -> user.lastName,
	        'isActivated -> user.isActivated	
	      ).executeUpdate()
        }
      }
      Some(user)               
    }
  }
}