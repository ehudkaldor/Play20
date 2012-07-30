package controllers

import anorm._
import play.api.data.Forms._
import play.api.data._
import play.api.mvc._
import play.api._
import views._
import models.projects.{Project, Task}
import models.users.User
import views.html.defaultpages.notFound

/**
 * Manage projects related operations.
 */
object Projects extends Controller with Secured {

  /**
   * Display the dashboard.
   */
  def index = IsAuthenticated { email => _ =>
    User.findByEmail(email).map { user =>
      Ok(
        html.dashboard(
          Project.findInvolving(email), 
          Task.findInvolving(email), 
          user
        )
      )
    }.getOrElse(Forbidden)
  }

  // -- Projects

  /**
   * Add a project.
   */
  def add = IsAuthenticated { username => implicit request =>
    Form("group" -> nonEmptyText).bindFromRequest.fold(
      errors => BadRequest,
      folder => Ok(
        views.html.projects.item(
          User.findByEmail(username) map { user =>
            Project.create(
              Project(folder, "New project", user.email), 
              Seq(user)
            )
          }
        )          
      )
    )
  }

  /**
   * Delete a project.
   */
  def delete(projectName: String) = IsProjectOwner(projectName) { username => _ =>
    Project.findByName(projectName).map { p =>
      Project.delete(p)      
    }
    Ok
  }

  /**
   * Rename a project.
   */
  def rename(projectName: String) = IsProjectOwner(projectName) { _ => implicit request =>
    Form("name" -> nonEmptyText).bindFromRequest.fold(
      errors => BadRequest,
      newName => { 
        Project.findByName(projectName).map { p =>
          Project.rename(p, newName) 
          Ok(newName) 
        }
        BadRequest(projectName)
      }
    )
  }

  // -- Project groups

  /**
   * Add a new project group.
   */
  def addGroup = IsAuthenticated { _ => _ =>
    Ok(html.projects.group("New group"))
  }

  // -- Members

  /**
   * Add a project member.
   */
  def addUser(projectName: String) = IsProjectOwner(projectName) { _ => implicit request =>
    Form("user" -> nonEmptyText).bindFromRequest.fold(
      errors => BadRequest,
      user => { 
        Project.findByName(projectName).map { p =>
          Project.addMember(p, user) 
          }          
          Ok(user) 
      }
    )
  }

  /**
   * Remove a project member.
   */
  def removeUser(projectName: String) = IsProjectOwner(projectName) { _ => implicit request =>
    Form("user" -> nonEmptyText).bindFromRequest.fold(
      errors => BadRequest,
      user => { 
        Project.findByName(projectName).map { p =>
          Project.removeMember(p, user)
        }          
        Ok(user) 
      }
    )
  }
}

