package controllers

import play.api._
import play.api.mvc._
import reactivemongo.api._
import play.modules.reactivemongo.MongoController

object Dashboard extends Controller with MongoController {


  def index = Action {
    request =>
      request.session.get("user").map { user => Ok(views.html.dashboard())
      }.getOrElse {
        Redirect(routes.UserManager.loginPage())
      }
  }

  def notFound(path:String) = Action {
    request =>
      request.session.get("user").map { user => Ok(views.html.dashboard())
      }.getOrElse {
        Redirect(routes.UserManager.loginPage())
      }
  }

}
