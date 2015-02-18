package controllers

import models._
import play.api._
import play.api.mvc._
import reactivemongo.api._
import play.modules.reactivemongo.MongoController
import play.modules.reactivemongo.json.collection.JSONCollection

import play.api.libs.json._
import play.api.data._
import play.api.data.Forms._
import scala.concurrent._

import play.api.libs.concurrent.Execution.Implicits.defaultContext

object UserManager extends Controller with MongoController {

  def usersCollection = db.collection[JSONCollection]("users")

  case class LoginInfo(login: String, passwd : String)

     private val loginForm = Form[LoginInfo](
        mapping(
            "login" -> nonEmptyText,
            "passwd" -> nonEmptyText)(LoginInfo.apply)(LoginInfo.unapply)
        )

  def welcome = Action { request =>
    request.session.get("connected").map { user => Ok("Hello " + user)
    }.getOrElse { Ok(views.html.index("Redirigé car pas connecté")).withSession("connected" -> "Coco l'asticot") }
  }

  def loginPage = Action {
    Ok(views.html.login(loginForm))
  }

  def loginSubmit = Action.async { implicit request =>
    loginForm.bindFromRequest.fold(
      formWithErrors => {
        Future.successful(BadRequest(views.html.login(formWithErrors)))
      },
      loginData => {
        val login = loginData.login
        val passwd = loginData.passwd

        val cursor = usersCollection.
        find(Json.obj("username" -> login, "password" -> passwd)).
        cursor[JsObject].        
        collect[List]()

        // val newUser = models.User(userData.name, userData.age)
        // val id = models.User.create(newUser)
        // Redirect(routes.Application.home(id))
        cursor.map { result =>
          if(result.isEmpty) {
            Ok(views.html.login(loginForm.withError("login", "login/password incorrect")))
          } else {
            
            Ok(views.html.dashboard(models.User("Dummy","Dummy"))).withSession("user" -> login)
          }
        }
      }
    )
  }

  def userWelcome = Action { request =>
      request.session.get("user").map { loggedUserName =>
      val u = User(loggedUserName,"test")
        Ok(views.html.dashboard(u))  
      }.getOrElse {Ok("Are no logged anymore")}
   }

}