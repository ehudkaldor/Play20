package controllers

import play.api._
import play.api.mvc._
import play.api.data._
import play.api.data.Forms._
import java.util.{Date}
import models.projects._
import views._
import org.joda.time.DateTime
import play.api.data.format.Formats._
import views.html.defaultpages.badRequest

/**
 * Manage tasks related operations.
 */
object Tasks extends Controller with Secured {

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

  // -- Tasks

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

