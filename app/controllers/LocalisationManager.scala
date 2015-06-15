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
import play.api.data.format.Formats._

import play.api.libs.concurrent.Execution.Implicits.defaultContext

case class LocalisationForm(nom:String,lat:Option[Float],lon:Option[Float],commentaire:Option[String])

/**
 * This object is a controller for manage all localisation
 */
trait LocalisationManagerLike extends Controller{

  /************* Property *********************/

  /**
   * DAO for localisations
   */
  val localisationDao:LocalisationDao=LocalisationDaoObj

  val form=Form[LocalisationForm](
    mapping(
      "nom"->nonEmptyText,
      "lat"->optional(of(floatFormat)),
      "lon"->optional(of(floatFormat)),
      "commentaire"->optional(text)
    )(LocalisationForm.apply)(LocalisationForm.unapply)
  )

  /****************** Route methods ***********/

  /**
   * This method is call when the user is on the page /campaigns/localisations. It list localisations
   * @return Return Ok Action when the user is on the page /campaigns/localisations with the list of localisations
   *         Return Redirect Action when the user is not log in
   *         Return Internal Server Error Action when have mongoDB error
   */
  @ApiOperation(
    nickname = "localisation",
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

  /**
   * This method is call when the user is on the page /campaigns/localisations/localisation. It display form for add new localisation
   * @return Return Ok Action when the user is on the page /campaigns/localisations/localisation with form for add new localisation
   *         Return Redirect Action when the user is not log in
   */
  @ApiOperation(
    nickname = "localisation/insert",
    value = "Get the html page with a form for add new localisation",
    notes = "Get the html page with a form for add new localisation",
    httpMethod = "GET")
  @ApiResponses(Array(
    new ApiResponse(code=303,message="Move resource to the login page at /login if the user is not log")
  ))
  def addLocalisationPage()=Action{
    implicit request=>
      //Verify if user is connect
      UserManager.doIfconnect(request) {
        Ok(views.html.localisation.formLocalisation(form,"",routes.LocalisationManager.addLocalisationPage()))
      }
  }
}

@Api(value = "/localisation", description = "Operations for manage localisation of ground condition")
object LocalisationManager extends LocalisationManagerLike