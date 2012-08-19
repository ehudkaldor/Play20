package models.users

import org.specs2.mutable._

import play.api.test._
import play.api.test.Helpers._

class UserSpec extends Specification {
  "models.User" should {
    "User created" in {
      val user = User.create("ehud@gmail.com", "eee", "eee", "eee", Role.MinimalRole.get, false)
      user match {
        case Left(newUserOption) => newUserOption match {
            case newUser => newUser.get.email.equals("ehud@gmail.com")
        }    
      }
    }
  }
}