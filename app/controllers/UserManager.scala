package controllers

import play.api._
import play.api.mvc._
import reactivemongo.api._
import play.modules.reactivemongo.MongoController
import play.modules.reactivemongo.json.collection.JSONCollection

import play.api.data._
import play.api.data.Forms._

object UserManager extends Controller with MongoController {

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
  
  def userWelcome = Action { request =>
    val inputUser
    
    
  }
}