package controllers

import play.api._
import play.api.mvc._
import play.api.data._
import play.api.data.Forms._
import java.util.{Date}
import anorm._
import models.projects._
import views._
import org.joda.time.DateTime

/**
 * Manage tasks related operations.
 */
object Tasks extends Controller with Secured {

  /**
   * Display the tasks panel for this project.
   */
  def index(projectName: String) = IsProjectOwner(projectName) { _ => implicit request =>
    Project.findByName(projectName).map { p =>
      val tasks = Task.findByProject(p)
      val team = Project.membersOf(p)
      Ok(html.tasks.index(p, tasks, team))
    }.getOrElse(NotFound)
  }

  val taskForm = Form(
    tuple(
      "taskTitle" -> nonEmptyText,
      "taskFolder" -> nonEmptyText,
      "taskDescription" -> optional(text),
      "dueDate" -> optional(date("MM/dd/yy")),
      "assignedTo" -> optional(text)
    )
  )

  // -- Tasks

  /**
   * Create a task in this project.
   */  
  def addTask(projectName: String) =  IsProjectOwner(projectName) { _ => implicit request =>
    taskForm.bindFromRequest.fold(
      errors => BadRequest,
      {
        case (taskTitle, taskFolder, taskDescription, dueDate, assignedTo) => 
          val task =  Task.create(
            Task(taskTitle, taskFolder, projectName, false, dueDate=Some(new DateTime(dueDate)), ownerEmail = assignedTo)
          )
          Ok(html.tasks.item(task))
      }
    )
  }

  /**
   * Update a task
   */
  def updateTask() = IsOwnerOf(task) { _ => implicit request =>
    taskForm.bindFromRequest.fold(
      errors => BadRequest,
      {
        case (taskTitle, taskFolder, taskDescription, dueDate, assignedTo) => 
          val task =  Task.create(
            Task(taskTitle, taskFolder, projectName, false, dueDate=Some(new DateTime(dueDate)), ownerEmail = assignedTo)
          )
          Ok(html.tasks.item(task))
      }
    )
  }

  /**
   * Delete a task
   */
  def delete(taskId: Long) = IsTaskOwner(task) { _ => implicit request =>
    Task.delete(taskId)
    Ok
  }

  // -- Task folders

  /**
   * Add a new folder.
   */
  def addFolder = Action {
    Ok(html.tasks.folder("New folder"))
  }

  /**
   * Delete a full tasks folder.
   */
  def deleteFolder(projectName: String, folder: String) = IsProjectOwner(projectName) { _ => implicit request =>
    Task.deleteInFolder(projectName, folder)
    Ok
  }

  /**
   * Rename a tasks folder.
   */
  def renameFolder(projectName: String, folder: String) = IsProjectOwner(projectName) { _ => implicit request =>
    Form("name" -> nonEmptyText).bindFromRequest.fold(
      errors => BadRequest,
      newName => { 
        Task.renameFolder(projectName, folder, newName) 
        Ok(newName) 
      }
    )
  }

}

