package controllers

import play.api.mvc.{Controller, Action}

object Application extends Controller {

  def swagger = Action {
    request =>
      Ok(views.html.swagger())
  }

}
