package controllers

import play.api.data.Forms._
import play.api.data._
import play.api.mvc._
import play.api._
import views._
import models.projects.{Project, Task}
import models.users.User
import views.html.defaultpages.notFound
import play.api.data.format.Formats._
import views.html.defaultpages.badRequest


/**
 * Manage projects related operations.
 */
object Projects extends Controller with Secured {
    
  val projectForm = Form(
    tuple(
      "projectId" -> optional(of[Long]),
      "name" -> nonEmptyText,
      "description" -> optional(text),
      "ownerId" -> of[Long]
    )
  )


  /**
   * Display the projects dashboard.
   */
  def projectsDashboard = IsAuthenticated { email => _ =>
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
  def addProject = IsAuthenticated { _ => implicit request =>
    projectForm.bindFromRequest.fold(
      errors => BadRequest,
      {
        case (projectId, name, description, ownerId) => {
          val project = projectId map { 
            Project.update(_, name, ownerId, description)
          } getOrElse {
            Project.create(name, ownerId, description)
          }
          Ok(html.projects.item(project))
        }
      }
    )
  }

  /**
   * Delete a project.
   */
  def deleteProject(projectId: Long) = IsProjectOwner(projectId) { username => _ =>
    Project.delete(projectId)      
    Ok
  }

  /**
   * Rename a project.
   */
  def renameProject(projectId: Long) = IsProjectOwner(projectId) { _ => implicit request =>
    Form("name" -> nonEmptyText).bindFromRequest.fold(
      errors => BadRequest,
      newName => { 
        Project.rename(projectId, newName)
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

  // -- Members

  /**
   * Add a project member.
   */
  def addMemberToProject(projectId: Long) = IsProjectOwner(projectId) { _ => implicit request =>
    Form("user" -> of[Long]).bindFromRequest.fold(
      errors => BadRequest,
      user => {  
        Project.addMember(projectId, user) 
        Redirect(html.projects.group(Project.findByNodeId(projectId))) 
      }
    )
  }

  /**
   * Remove a project member.
   */
  def removeMemberFromProject(projectId: Long) = IsProjectOwner(projectId) { _ => implicit request =>
    Form("user" -> nonEmptyText).bindFromRequest.fold(
      errors => BadRequest,
      user => { 
        Project.findByNodeId(projectId) map { p =>
          Project.removeMember(p, user)
        }          
        Redirect(html.projects.group(Project.findByNodeId(projectId))) 
      }
    )
  }
}

