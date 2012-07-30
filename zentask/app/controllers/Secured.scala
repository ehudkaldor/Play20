package controllers

import play.api.mvc.RequestHeader
import play.api.mvc.Request
import play.api.mvc.Results
import play.api.mvc.Result
import models.projects._
import play.api.mvc.AnyContent
import play.api.mvc.Security
import play.api.mvc.Action

trait Secured {
  
  /**
   * Retrieve the connected user email.
   */
  private def username(request: RequestHeader) = request.session.get("email")

  /**
   * Redirect to login if the user in not authorized.
   */
  private def onUnauthorized(request: RequestHeader) = Results.Redirect(routes.Application.login)
  
  // --
  
  /** 
   * Action for authenticated users.
   */
  def IsAuthenticated(f: => String => Request[AnyContent] => Result) = Security.Authenticated(username, onUnauthorized) { user =>
    Action(request => f(user)(request))
  }

  /**
   * Check if the connected user is a member of this project.
   */
  def IsProjectOwner(project: Project)(f: => String => Request[AnyContent] => Result) = IsAuthenticated { user => request =>
    if(Project.isOwner(project, user)) {
      f(user)(request)
    } else {
      Results.Forbidden
    }
  }

  def IsProjectMember(project: Project)(f: => String => Request[AnyContent] => Result) = IsAuthenticated { user => request =>
    if(Project.isMember(projectName, user)) {
      f(user)(request)
    } else {
      Results.Forbidden
    }
  }

  /**
   * Check if the connected user is a owner of this task.
   */
  def IsTaskOwner(taskId: Long)(f: => String => Request[AnyContent] => Result) = IsAuthenticated { user => request =>
    if(Task.isOwner(taskId, user)) {
      f(user)(request)
    } else {
      Results.Forbidden
    }
  }
}