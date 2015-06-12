package controllers

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

case class ParameterForm(key:String,value:String)

/**
 * This object is a controller for manage all parameter
 */
trait ParametreManagerLike extends Controller{

  /************* Property *********************/

  /**
   * DAO for parameters
   */
  val parameterDao:ParametresDao=ParametresDaoObj

  val form=Form[ParameterForm](
    mapping(
      "key"->nonEmptyText,
      "value"->nonEmptyText
    )(ParameterForm.apply)(ParameterForm.unapply)
  )

  /****************** Route methods ***********/

  /**
   * This method is call when the user is on the page /campaigns/parameter. It list calibration parameter
   * @return Return Ok Action when the user is on the page /campaigns/parameter with the list of calibration parameter
   *         Return Redirect Action when the user is not log in
   *         Return Internal Server Error Action when have mongoDB error
   */
  @ApiOperation(
    nickname = "parameter",
    value = "Get the html page for list calibration parameter",
    notes = "Get the html page for list calibration parameter",
    httpMethod = "GET")
  @ApiResponses(Array(
    new ApiResponse(code=303,message="Move resource to the login page at /login if the user is not log"),
    new ApiResponse(code=500,message="Have a mongoDB error")
  ))
  def listParameter()=Action.async{
    implicit request =>
      //Verify if user is connect
      UserManager.doIfconnectAsync(request) {
        future{Ok(views.html.param.listParam())}
      }
  }

  def addParameterPage()=Action{
    implicit request =>
      //Verify if user is connect
      UserManager.doIfconnect(request) {
        Ok(views.html.param.formParam(form,routes.ParametreManager.addParameter()))
      }
  }

  def addParameter()=Action.async{
    implicit request =>
      //Verify if user is connect
      UserManager.doIfconnectAsync(request){

        //Verify data submit
        form.bindFromRequest.fold(

          //If data submit contains an error, display the form with prefilled data
          formWithError=>future{BadRequest(views.html.param.formParam(formWithError,routes.ParametreManager.addParameter()))}
          ,

          //If data submit not contains error
          data=>
          //Find parameter
          parameterDao.findOne(Json.obj("cle"->data.key)).flatMap(
            paramOpt=>paramOpt match{
              //If parameter not found, insert data and redirect to the list of parameter
              case None=>parameterDao.insert(Parametres(cle=data.key,valeur=data.value)).map(
                  _=>Redirect(routes.ParametreManager.listParameter())
                )
              //If parameter found, display the form with prefilled data
              case _=>future{BadRequest(views.html.param.formParam(form.fill(data).withGlobalError(Messages("campaign.param.error.paramExist")),routes.ParametreManager.addParameterPage()))}
            }
          )
        )
      }
  }
}

@Api(value = "/param", description = "Operations for calibration parameter")
object ParametreManager extends ParametreManagerLike