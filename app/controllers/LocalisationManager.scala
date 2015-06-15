package controllers

import java.lang.annotation.Annotation

import com.wordnik.swagger.annotations._
import models._
import play.api.data.Form
import play.api.data.Forms._
import play.api.i18n.Messages
import play.api.libs.json.{JsObject, Json}
import play.api.mvc._
import reactivemongo.bson.BSONObjectID
import reactivemongo.extensions.BSONFormats.BSONObjectIDFormat
import scala.concurrent._

import play.api.libs.concurrent.Execution.Implicits.defaultContext

/**
 * This object is a controller for manage all localisation
 */
trait LocalisationManagerLike extends Controller{

  /************* Property *********************/

  /**
   * DAO for localisations
   */
  val localisationDao:LocalisationDao=LocalisationDaoObj

  /****************** Route methods ***********/

  /**
   * This method is call when the user is on the page /campaigns/localisations. It list localisations
   * @return Return Ok Action when the user is on the page /campaigns/localisations with the list of localisations
   *         Return Redirect Action when the user is not log in
   *         Return Internal Server Error Action when have mongoDB error
   */
  @ApiOperation(
    nickname = "parameter",
    value = "Get the html page for list localisation",
    notes = "Get the html page for list localisation",
    httpMethod = "GET")
  @ApiResponses(Array(
    new ApiResponse(code=303,message="Move resource to the login page at /login if the user is not log"),
    new ApiResponse(code=500,message="Have a mongoDB error")
  ))
  def listLocalisation()=Action.async{
    implicit request =>
      //Verify if user is connect
      UserManager.doIfconnectAsync(request) {
        future{Ok(views.html.localisation.listLocalisation())}
      }
  }
}

@Api(value = "/localisation", description = "Operations for manage localisation of ground condition")
object LocalisationManager extends LocalisationManagerLike