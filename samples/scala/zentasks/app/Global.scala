import play.api._

import models._
import anorm._

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
    
    Role.create(Role("superuser"))
    Role.create(Role("monitor"))
    
    if(User.findAll.isEmpty) {
      User.create(User("admin@white-bears.org", "secret", isActivated=true))     
    }
    
  }
  
}