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
  def IsAuthenticated(f: => String => Request[AnyContent] => Result) = Security.Authenticated(username, onUnauthorized) { userIdAsString =>
    Action(request => f(userIdAsString)(request))
  }

  /**
   * Check if the connected user is a member of this project.
   */
  def IsProjectOwner(projectId: Long)(f: => String => Request[AnyContent] => Result) = IsAuthenticated { userIdAsString => request =>
    if(Project.isOwner(projectId, userIdAsString.toLong)) {
      f(userIdAsString)(request)
    } else {
      Results.Forbidden
    }
  }

  def IsProjectMember(projectId: Long)(f: => String => Request[AnyContent] => Result) = IsAuthenticated { userIdAsString => request =>
    if(Project.isMember(projectId, userIdAsString.toLong)) {
      f(userIdAsString)(request)
    } else {
      Results.Forbidden
    }
  }

  /**
   * Check if the connected user is a owner of this task.
   */
  def IsTaskOwner(taskId: Long)(f: => String => Request[AnyContent] => Result) = IsAuthenticated { userIdAsString => request =>
    if(Task.isOwner(taskId, userIdAsString.toLong)) {
      f(userIdAsString)(request)
    } else {
      Results.Forbidden
    }
  }
}