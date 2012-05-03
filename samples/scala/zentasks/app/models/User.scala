package models

import play.api.db._
import play.api.Play.current

import anorm._
import anorm.SqlParser._

case class User(email: String, password: String, firstName: String = "", lastName: String = "", isActivated: Boolean = false)

object User {
  
  // -- Parsers
  
  /**
   * Parse a User from a ResultSet
   */
  val simple = {
    get[String]("user.email") ~
    get[String]("user.password") ~
    get[String]("user.firstName") ~
    get[String]("user.lastName") ~
    get[Boolean]("user.isActivated") map {
      case email~password~firstName~lastName~isActivated => User(email, password, firstName, lastName, isActivated)
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
  def create(user: User): User = {
    DB.withConnection { implicit connection =>
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
      
      user
      
    }
  }
  
}
