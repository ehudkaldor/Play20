import play.api._
import models._
import anorm._
import models.users.Role
import models.users.User

object Global extends GlobalSettings {
  
  override def onStart(app: Application) {
    InitialData.insert()
  }
  
}

/**
 * Initial set of data to be imported 
 * in the sample application.
 */
object InitialData {
  
  def date(str: String) = new java.text.SimpleDateFormat("yyyy-MM-dd").parse(str)
  
  def insert() = {
    
    val superUserRole = Role.create("superuser")
    Role.create("monitor")
    
    if(User.findAll.isEmpty) {
      superUserRole map { suRole =>
        User.create("admin@white-bears.org", "secret", "admin", "admin", suRole, true) 
      }
    }
    
  }
  
}