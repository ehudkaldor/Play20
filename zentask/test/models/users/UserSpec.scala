package models.users

import org.specs2.mutable._

import play.api.test._
import play.api.test.Helpers._

class UserSpec extends Specification {
  "models.User" should {
    "User created" in {
      User.create(User("ehud@gmail.com", "eee", "eee", "eee", false, "fff")).get.email.equals("ehud@gmail.com")
    }
  }
}