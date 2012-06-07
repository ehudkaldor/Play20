package controllers

import anorm._
import play.api.data.Forms._
import play.api.data._
import play.api.mvc._
import play.api._
import views._
import models.projects.{Project, Task}
import models.users.User

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
          Task.findTodoInvolving(email), 
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
              Project(NotAssigned, folder, "New project", user.email), 
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
  def delete(project: Project) = IsMemberOf(project.id) { username => _ =>
    Project.delete(project)
    Ok
  }

  /**
   * Rename a project.
   */
  def rename(project: Long) = IsMemberOf(project) { _ => implicit request =>
    Form("name" -> nonEmptyText).bindFromRequest.fold(
      errors => BadRequest,
      newName => { 
        Project.rename(project, newName) 
        Ok(newName) 
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

  /**
   * Delete a project group.
   */
  def deleteGroup(folder: String) = IsAuthenticated { _ => _ =>
    Project.deleteInFolder(folder)
    Ok
  }

  /**
   * Rename a project group.
   */
  def renameGroup(folder: String) = IsAuthenticated { _ => implicit request =>
    Form("name" -> nonEmptyText).bindFromRequest.fold(
      errors => BadRequest,
      newName => { Project.renameFolder(folder, newName); Ok(newName) }
    )
  }

  // -- Members

  /**
   * Add a project member.
   */
  def addUser(project: Long) = IsMemberOf(project) { _ => implicit request =>
    Form("user" -> nonEmptyText).bindFromRequest.fold(
      errors => BadRequest,
      user => { Project.addMember(project, user); Ok }
    )
  }

  /**
   * Remove a project member.
   */
  def removeUser(project: Long) = IsMemberOf(project) { _ => implicit request =>
    Form("user" -> nonEmptyText).bindFromRequest.fold(
      errors => BadRequest,
      user => { Project.removeMember(project, user); Ok }
    )
  }
}

