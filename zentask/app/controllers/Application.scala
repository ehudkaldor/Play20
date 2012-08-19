package controllers

import play.api._
import play.api.mvc._
import play.api.data._
import play.api.data.Forms._
import models._
import views._
import models.users.User
import models.projects._
import play.api.data.format.Formats._
import org.joda.time.DateTime


object Application extends Controller with Secured {

  /********
   * 
   * Forms
   * 
   ********/
  
  
  val memberLoginForm = Form(
    tuple(
      "email" -> text,
      "password" -> text
    ) verifying ("Invalid email or password", result => result match {
      case (email, password) => User.authenticate(email, password)
    })
  )

  val memberRegisterForm = Form(
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
  
  val projectForm = Form(
    tuple(
      "projectId" -> optional(of[Long]),
      "name" -> nonEmptyText,
      "description" -> optional(text),
      "ownerId" -> of[Long]
    )
  )

  val taskForm = Form(
    tuple(
      "taskId" -> optional(of[Long]),
      "name" -> nonEmptyText,
      "folder" -> nonEmptyText,
      "description" -> optional(text),
      "dueDate" -> optional(date("MM/dd/yy")),
      "assignedToUserId" -> optional(of[Long]),
      "projectId" -> of[Long]
    )
  )

  /**
   * Login page.
   */
  def login = Action { implicit request =>
    Ok(html.users.login(memberLoginForm))
  }

  /**
   * Handle login form submission.
   */
  def authenticate = Action { implicit request =>
    memberLoginForm.bindFromRequest.fold(
      formWithErrors => BadRequest(html.users.login(formWithErrors)),
      user => Redirect(routes.Projects.index).withSession("email" -> user._1)
    )
  }
  
    /**
   * Register page.
   */
  def register = Action { implicit request =>
    Ok(html.users.register(memberRegisterForm))
  }
  
    /**
   * Handle register form submission
   */
  def createUser = Action { implicit request =>
    memberRegisterForm.bindFromRequest.fold(
      formWithErrors => BadRequest(html.users.register(formWithErrors)),
      user => {
        User.create(user._1, user._3) match{
          case Left(newUserOption) => newUserOption match {
            //option contains the newly created User
            case newUser => Ok(html.dashboard() ).withSession("userId" -> newUser.id)
            //Option is empty (meaning DB creation failed)
            case None => Ok(html.users.register(memberRegisterForm)).flashing("error" -> "")
          }
          //error message returned (no Option[User])
          case Right(message) => Ok(html.users.register(memberRegisterForm)).flashing("error" -> message)
        }
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

  
  /****
   * 
   * 	Projects
   * 
   ****/
  
    /**
   * Display the projects dashboard.
   */
  def projectsDashboard = IsAuthenticated { email => _ =>
    User.findByEmail(email).map { user =>
    html.dashboard(
      Ok(
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
    Form("userId" -> of[Long]).bindFromRequest.fold(
      errors => BadRequest,
      userId => {  
        Project.addMember(projectId, userId)           
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
        Project.findByNodeId(projectId).map { p =>
          Project.removeMember(p, user)
        }          
        Redirect(html.projects.group(Project.findByNodeId(projectId))) 
      }
    )
  }

  
  /****
   * 
   * 	Tasks
   * 
   ****/

    /**
   * Display the tasks panel for a project.
   */
  def tasksDashboard(projectId: Long) = IsProjectOwner(projectId) { _ => implicit request =>
    Project.findByNodeId(projectId).map { p =>
      val tasks = Task.findByProject(p.id)
      val team = Project.membersOf(p.id)
      Ok(html.tasks.index(p, tasks, team))
    }.getOrElse(NotFound)
  }

    /**
   * Create a task or update an existing task in this project.
   */  
  def addOrUpdateTask(projectIdAsString: String) =  IsProjectOwner(projectIdAsString.toLong) { _ => implicit request =>
    taskForm.bindFromRequest.fold(
      errors => BadRequest,
      {
        case (taskId, name, folder, description, dueDate, assignedToUserId, projectId) => {
          val task = taskId map { 
            Task.update(
              _, name, folder, false, description, dueDate=Some(new DateTime(dueDate)), assignedToUserId
            )            
          } getOrElse {
            Task.create(
              name, folder, projectId, false, description, dueDate=Some(new DateTime(dueDate)), assignedToUserId
            )                      
          }
        } 
        Ok(html.tasks.item(task))
      }
    )
  }

  /**
   * Update a task
   */
//  def updateTask(taskIdAsString: String) = IsProjectMember(Task.findByNodeId(taskIdAsString.toLong).get.projectId) { _ => implicit request =>
//    taskForm.bindFromRequest.fold(
//      errors => BadRequest,
//      {
//        case (taskId, name, folder, description, dueDate, assignedToUserId, projectId) => 
//          val task =  Task.update(
//            taskId, name, folder, false, description, dueDate=Some(new DateTime(dueDate)), assignedToUserId
//          )
//          Ok(html.tasks.item(task))
//      }
//    )
//  }

  /**
   * Delete a task
   */
  def deleteTask(taskId: Long) = IsProjectOwner(Task.findByNodeId(taskId).get.projectId) { _ => implicit request =>
    Task.findByNodeId(taskId) map { task => 
      val project = Project.findByNodeId(task.projectId)
      val taskName = task.name
      Task.delete(taskId)      
      Redirect(html.tasks.index(project)).flashing(("response", "Task " + taskName + " deleted"))
    }
    BadRequest("Task id " + taskId + " not found")
  }

  // -- Task folders

  /**
   * Delete a full tasks folder.
   */
  def deleteFolder(projectId: Long, folder: String) = IsProjectOwner(projectId) { _ => implicit request =>
    Task.deleteAllInFolder(projectId, folder)
    Redirect(html.tasks.index(Project.findByNodeId(projectId))).flashing(("response", "Folder " + folder + " deleted"))
  }

  /**
   * Rename a tasks folder.
   */
  def renameFolder(projectId: Long, folder: String) = IsProjectOwner(projectId) { _ => implicit request =>
    Form("name" -> nonEmptyText).bindFromRequest.fold(
      errors => BadRequest,
      newName => { 
        Task.renameFolder(projectId, folder, newName) 
        Redirect(html.tasks.index(Project.findByNodeId(projectId))).flashing(("response", "Folder " + folder + " was renamed " + newName))
      }
    )
  }
}
