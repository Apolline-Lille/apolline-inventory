package controllers

import java.io.File
import java.lang.annotation.Annotation
import java.util.UUID

import com.wordnik.swagger.annotations._
import models._
import play.api.Play
import play.api.data.Form
import play.api.data.Forms._
import play.api.i18n.Messages
import play.api.libs.Files.TemporaryFile
import play.api.libs.json.{JsObject, Json}
import play.api.mvc._
import reactivemongo.bson.BSONObjectID
import reactivemongo.extensions.BSONFormats.BSONObjectIDFormat
import scala.concurrent._
import play.api.data.format.Formats._

import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.Play.current

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

  val app=Play.application

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
        Ok(views.html.localisation.formLocalisation(form,"",routes.LocalisationManager.addLocalisation()))
      }
  }

  /**
   * This method is call when the user is on the page /campaigns/localisations/localisation. It add new localisation
   * @return Return Redirect Action when the user is not log in or after insert localisation
   *         Return Bad request if data received have an error
   */
  @ApiOperation(
    nickname = "localisation/insert",
    value = "Insert new localisation",
    notes = "Insert new localisation into the database",
    httpMethod = "POST")
  @ApiResponses(Array(
    new ApiResponse(code=303,message="<ul><li>Move resource to the login page at /login if the user is not log</li><li>Move resource to the list of localisations at /campaigns/localisations after insert the new localisation</li></ul>"),
    new ApiResponse(code=400,message="Error in data received")
  ))
  def addLocalisation()=Action.async(parse.multipartFormData) {implicit request =>
    //Verify data received
    submitForm(routes.LocalisationManager.addLocalisation()){
      //Query for get localisation
      localisation=>Json.obj("nom"->localisation.nom)
    }{
      (loc,img)=> {
        //Create the new localisation
        val localisation=Localisation(nom = loc.nom, lat = loc.lat, lon = loc.lon, commentaire = loc.commentaire, photo = img)

        //Insert the new localisation
        localisationDao.insert(localisation).map(
          //Redirect to the list of localisation
          data => Redirect(routes.LocalisationManager.listLocalisation())
        )
      }
    }
  }

  /**
   * This method verify data received
   * @param route Route used when submit the form
   * @param verif Function for get query to find localisation
   * @param f Function dedicated
   * @param request Request received
   * @return
   */
  def submitForm(route:Call)(verif:LocalisationForm=>JsObject)(f:(LocalisationForm,List[String])=>Future[Result])(implicit request:Request[MultipartFormData[TemporaryFile]]):Future[Result]={
    //Verify if user is connect
    UserManager.doIfconnectAsync(request.asInstanceOf[Request[AnyContent]]) {

      form.bindFromRequest.fold(

        //If have error in data received
        formWithError => future{BadRequest(views.html.localisation.formLocalisation(formWithError, "", route)(request.asInstanceOf[Request[AnyContent]]))}
        ,
        data =>
          //Find a localisation
          localisationDao.findOne(verif(data)).flatMap(
            localisationOpt=>localisationOpt match{

              //If localisation not found
              case None =>{
                //Insert image
                val img=insertImage(request.body.files.toList)

                //Execute dedicated function
                f(data,img)
              }

              //If localisation found redirect to the form
              case _ =>future{BadRequest(views.html.localisation.formLocalisation(form.fill(data).withGlobalError(Messages("campaign.localisation.error.localisationExist")), "", route)(request.asInstanceOf[Request[AnyContent]]))}
            }
          )
      )
    }
  }

  /**
   * This method insert image in the folder public/images/campaign
   * @param list Images to insert in the folder
   * @return List of image name
   */
  def insertImage(list:List[MultipartFormData.FilePart[TemporaryFile]]):List[String]=list match{
    //If not have image
    case Nil=>List()

    //If have image
    case h::t=> {
      //Find the image extension
      val extension=h.filename.split("\\.").last

      //Create an uniq name
      val filename = UUID.randomUUID().toString+"."+extension

      //Find the location
      val r=app.path

      //Move the file
      h.ref.moveTo(new File((r+"/public/images/campaign/"+filename)))

      //Insert other image
      filename::insertImage(t)
    }
  }
}

@Api(value = "/localisation", description = "Operations for manage localisation of ground condition")
object LocalisationManager extends LocalisationManagerLike