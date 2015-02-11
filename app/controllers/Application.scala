package controllers

import play.api._
import play.api.mvc._
import reactivemongo.api._
import play.modules.reactivemongo.MongoController
import play.modules.reactivemongo.json.collection.JSONCollection


object Application extends Controller with MongoController {

       
    def index = Action {
      Ok(views.html.index("Bonjour")).withNewSession }

}               