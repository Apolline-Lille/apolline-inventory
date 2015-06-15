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
   * This method is call when the user is on the page /campaigns/parameters. It list calibration parameter
   * @return Return Ok Action when the user is on the page /campaigns/parameters with the list of calibration parameter
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
        parameterDao.findAll().map(
          params=>Ok(views.html.param.listParam(params))
        )
      }
  }

  /**
   * This method is call when the user is on the page /campaigns/parameters/parameter. It display a form for insert parameter
   * @return Return Ok Action when the user is on the page /campaigns/parameters/parameter with the a form for insert parameter
   *         Return Redirect Action when the user is not log in
   */
  @ApiOperation(
    nickname = "parameter/insert",
    value = "Get the html page for display a form for insert parameter",
    notes = "Get the html page for display a form for insert parameter",
    httpMethod = "GET")
  @ApiResponses(Array(
    new ApiResponse(code=303,message="Move resource to the login page at /login if the user is not log")
  ))
  def addParameterPage()=Action{
    implicit request =>
      //Verify if user is connect
      UserManager.doIfconnect(request) {
        Ok(views.html.param.formParam(form,routes.ParametreManager.addParameter()))
      }
  }

  /**
   * This method is call when the user is on the page /campaigns/parameters/parameter/:id. It display a form for update parameter
   * @return Return Ok Action when the user is on the page /campaigns/parameters/parameter/:id with the a form for update parameter
   *         Return Redirect Action when the user is not log in or when the parameter not found
   */
  @ApiOperation(
    nickname = "parameter/update",
    value = "Get the html page with a prefilled form for update parameter",
    notes = "Get the html page with a prefilled form for update parameter",
    httpMethod = "GET")
  @ApiResponses(Array(
    new ApiResponse(code=303,message="<ul><li>Move resource to the login page at /login if the user is not log</li><li>Move resource to the parameters list at /campaigns/parameters if parameter not found</li></ul>")
  ))
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name="id",value="Parameter id",required = true,paramType = "String",dataType="path")
  ))
  def updateParameterPage(id:String)=Action.async{
    implicit request=>
      //Verify if user is connect
      UserManager.doIfconnectAsync(request) {

        //Find the parameter
        parameterDao.findById(BSONObjectID(id)).map(
          data => data match{

            //If parameter not found redirect to the parameters list
            case None => Redirect(routes.ParametreManager.listParameter())

            //If parameter found display form with prefilled data
            case Some(param)=> Ok(views.html.param.formParam(form.fill(ParameterForm(param.cle,param.valeur)),routes.ParametreManager.updateParameter(id)))
          }
        )
      }
  }

  /**
   * This method is call when the user is on the page /campaigns/parameters/parameter. It insert parameter into the database
   * @return Return Redirect Action when the user is not log in or after insert parameter
   *         Return Internal Server Error Action when have mongoDB error
   */
  @ApiOperation(
    nickname = "parameter/insert",
    value = "Insert parameter into the database",
    notes = "Insert parameter into the database",
    httpMethod = "POST")
  @ApiResponses(Array(
    new ApiResponse(code=303,message="<ul><li>Move resource to the login page at /login if the user is not log</li><li>Move resource to the parameter list at /campaigns/parameters if parameter was insert</li></ul>"),
    new ApiResponse(code=500,message="Have a mongoDB error")
  ))
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name="key",value="Key of the parameter",required=true,dataType="String",paramType="form"),
    new ApiImplicitParam(name="value",value="Value of the parameter",required=true,dataType="String",paramType="form")
  ))
  def addParameter()=Action.async{
    implicit request =>
      //Verify data received
      submitForm(routes.ParametreManager.addParameter()){
        //Create query for find parameter
        param=>Json.obj("cle"->param.key)
      }{
        //Insert parameter into the database
        param=>parameterDao.insert(Parametres(cle=param.key,valeur=param.value)).map(
          _=>Redirect(routes.ParametreManager.listParameter())
        )
      }
  }

  /**
   * This method is call when the user is on the page /campaigns/parameters/parameter/:id. It update parameter into the database
   * @return Return Redirect Action when the user is not log in or after update parameter
   *         Return Internal Server Error Action when have mongoDB error
   */
  @ApiOperation(
    nickname = "parameter/update",
    value = "Update parameter into the database",
    notes = "Update parameter into the database",
    httpMethod = "POST")
  @ApiResponses(Array(
    new ApiResponse(code=303,message="<ul><li>Move resource to the login page at /login if the user is not log</li><li>Move resource to the parameter list at /campaigns/parameters if parameter was update</li></ul>"),
    new ApiResponse(code=500,message="Have a mongoDB error")
  ))
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name="id",value="Parameter id",required=true,dataType="String",paramType="path"),
    new ApiImplicitParam(name="key",value="Key of the parameter",required=true,dataType="String",paramType="form"),
    new ApiImplicitParam(name="value",value="Value of the parameter",required=true,dataType="String",paramType="form")
  ))
  def updateParameter(id:String)=Action.async{
    implicit request =>
      //Verify data received
      submitForm(routes.ParametreManager.updateParameter(id)){
        //Create query for find parameter
        param=>Json.obj("cle"->param.key,"_id"->Json.obj("$ne"->BSONObjectID(id)))
      }{
        //Parameter parameter into the database
        param=>parameterDao.updateById(BSONObjectID(id),Parametres(BSONObjectID(id),param.key,param.value)).map(
          _=>Redirect(routes.ParametreManager.listParameter())
        )
      }
  }

  /**
   * Verify if the user is connect and if data received are valid then apply function dedicated
   * @param route Route use for submit the form
   * @param verif Function use for get card selector
   * @param f Function dedicated
   * @param request
   * @return Return Bad request Action if the form is not valid
   *         Return Redirect if dedicated function is a success
   *         Return Internal server error if have mongoDB error
   */
  def submitForm(route:Call)(verif:ParameterForm=>JsObject)(f:ParameterForm=>Future[Result])(implicit request:Request[AnyContent]):Future[Result]={
    //Verify if user is connect
    UserManager.doIfconnectAsync(request){

      //Verify data submit
      form.bindFromRequest.fold(

        //If data submit contains an error, display the form with prefilled data
        formWithError=>future{BadRequest(views.html.param.formParam(formWithError,route))}
        ,

        //If data submit not contains error
        data=>
          //Find parameter
          parameterDao.findOne(verif(data)).flatMap(
            paramOpt=>paramOpt match{
              //If parameter not found, insert data and redirect to the list of parameter
              case None=>f(data)
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