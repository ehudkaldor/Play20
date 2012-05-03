package controllers

import play.api._
import play.api.mvc._
import play.api.data._
import play.api.data.Forms._

import models._
import views._

object Application extends Controller {

  // -- Authentication

  val loginForm = Form(
    tuple(
      "email" -> text,
      "password" -> text
    ) verifying ("Invalid email or password", result => result match {
      case (email, password) => User.authenticate(email, password).isDefined
    })
  )

    // - Registration Form

  val registerForm = Form(
    tuple(
      "email" -> nonEmptyText,
      "verifyEmail" -> nonEmptyText,
      "password" -> nonEmptyText,
      "verifyPassword" -> nonEmptyText
    ) verifying (result => result match {
      case (email, verifyEmail, password, verifyPassword) => {
        //email and verifyEmail are different
        "email and email verification are not the same"; email == verifyEmail
        //password and verifyPassword are different
        "password and password verification are not the same"; password == verifyPassword
        //email exists already
        "email address already exists"; User.findByEmail(email).isDefined
      }
    })
  )

  /**
   * Login page.
   */
  def login = Action { implicit request =>
    Ok(html.users.login(loginForm))
  }

  /**
   * Handle login form submission.
   */
  def authenticate = Action { implicit request =>
    loginForm.bindFromRequest.fold(
      formWithErrors => BadRequest(html.users.login(formWithErrors)),
      user => Redirect(routes.Projects.index).withSession("email" -> user._1)
    )
  }
  
    /**
   * Register page.
   */
  def register = Action { implicit request =>
    Ok(html.users.register(registerForm))
  }
  
    /**
   * Handle register form submission
   */
  def createUser = Action { implicit request =>
    registerForm.bindFromRequest.fold(
      formWithErrors => BadRequest(html.users.register(formWithErrors)),
      user => {
//        val email = user._1
//        val password = user._3
//        User.create(User(email, password))
        User.create(User(user._1, user._3))
        Redirect(routes.Projects.index).withSession("email" -> user._1) 
      }
    )
  }

  /**
   * Logout and clean the session.
   */
  def logout = Action {
    Redirect(routes.Application.login).withNewSession.flashing(
      "success" -> "You've been logged out"
    )
  }

  // -- Javascript routing

  def javascriptRoutes = Action { implicit request =>
    import routes.javascript._
    Ok(
      Routes.javascriptRouter("jsRoutes")(
        Projects.add, Projects.delete, Projects.rename,
        Projects.addGroup, Projects.deleteGroup, Projects.renameGroup,
        Projects.addUser, Projects.removeUser, Tasks.addFolder, 
        Tasks.renameFolder, Tasks.deleteFolder, Tasks.index,
        Tasks.add, Tasks.update, Tasks.delete
      )
    ).as("text/javascript") 
  }

}

/**
 * Provide security features
 */
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
  def IsMemberOf(project: Long)(f: => String => Request[AnyContent] => Result) = IsAuthenticated { user => request =>
    if(Project.isMember(project, user)) {
      f(user)(request)
    } else {
      Results.Forbidden
    }
  }

  /**
   * Check if the connected user is a owner of this task.
   */
  def IsOwnerOf(task: Long)(f: => String => Request[AnyContent] => Result) = IsAuthenticated { user => request =>
    if(Task.isOwner(task, user)) {
      f(user)(request)
    } else {
      Results.Forbidden
    }
  }

}

