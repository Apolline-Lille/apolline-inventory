package controllers

import java.util.Date

import com.wordnik.swagger.annotations._
import models._
import play.api.data.Form
import play.api.data.Forms._
import play.api.i18n.Messages
import play.api.mvc._
import reactivemongo.bson.BSONObjectID
import scala.concurrent._

import play.api.libs.concurrent.Execution.Implicits.defaultContext

case class ModuleForm(
   id:String,
   acquisition:Date,
   firstUse:Option[Date],
   agregateur:Boolean,
   apolline:Option[String],
   firmware:String,
   versionFirmware:String,
   hs:Boolean,
   commentaire:Option[String]
)

trait ModuleManagerLike extends Controller{

  val typeModuleDao:TypeModuleDao=TypeModuleDaoObj
  val moduleDao:ModuleDao=ModuleDaoObj
  val firmwareDao:FirmwareDao=FirmwareDaoObj

  val typeModuleManager:TypeModuleManagerLike=TypeModuleManager

  val form=Form[ModuleForm](
    mapping(
      "id"->nonEmptyText,
      "acquisition"->date,
      "firstUse"->optional(date),
      "agregateur"->boolean,
      "apolline"->optional(text),
      "firmware"->nonEmptyText,
      "versionFirmware"->nonEmptyText,
      "hs"->boolean,
      "commentaire"->optional(text)
    )(ModuleForm.apply)(ModuleForm.unapply)
  )

  /**
   * This method is call when the user is on the page /inventary/modules/:id. It list sensors available for a particular type
   * @return Return Ok Action when the user is on the page /inventary/sensors/:id with the list of sensors
   *         Return Redirect Action when the user is not log in
   *         Return Internal Server Error Action when have mongoDB error
   */
  @ApiOperation(
    nickname = "inventary",
    value = "Get the html page for list modules",
    notes = "Get the html page for list modules",
    httpMethod = "GET")
  @ApiResponses(Array(
    new ApiResponse(code=303,message="Move resource to the login page at /login if the user is not log"),
    new ApiResponse(code=500,message="Have a mongoDB error")
  ))
  @ApiImplicitParams(Array(
    new ApiImplicitParam(value = "Id of the module type for list modules associated",required=true,name="id", dataType = "String", paramType = "path")
  ))
  def inventary(id:String,sort:String="id",sens:Int=1)=Action.async{
    implicit request =>
      //Verify if user is connect
      UserManager.doIfconnectAsync(request) {
        //Find the module type
        typeModuleDao.findById(BSONObjectID(id)).map(
          data=> data match{

              //If module type not found
            case None => Redirect(routes.TypeModuleManager.inventary())

              //If module type found
            case Some(typeModule) => Ok(views.html.module.listModule(typeModule))
          }
        ).recover({case _=>InternalServerError("error")})
      }
  }

  /**
   * This method is call when the user is on the page /inventary/modules/:id/module. It display a form for add new module
   * @return Return Ok Action when the user is on the page /inventary/modules/:id/module with the form for add new module
   *         Return Redirect Action when the user is not log in
   */
  @ApiOperation(
    nickname = "inventary/module/insert",
    value = "Get the html page a form for add new module",
    notes = "Get the html page a form for add new module",
    httpMethod = "GET")
  @ApiResponses(Array(
    new ApiResponse(code=303,message="Move resource to the login page at /login if the user is not log")
  ))
  @ApiImplicitParams(Array(
    new ApiImplicitParam(value = "Id of the module type for list modules associated",required=true,name="id", dataType = "String", paramType = "path")
  ))
  def modulePage(id:String)=Action.async{
    implicit request =>
      //Verify if user is connect
      UserManager.doIfconnectAsync(request) {
        //Verify if module type found
        typeModuleManager.doIfTypeModuleFound(BSONObjectID(id)) {_=>
          //Print an empty form for add new module
          printForm(Results.Ok,id,form,routes.SensorManager.sensorInsert(id))
        }{_=>
          //Print an empty form with error type not found
          printForm(Results.BadRequest,id,form.withGlobalError(Messages("inventary.typeModule.error.typeNotExist")),routes.SensorManager.sensorInsert(id))
        }
      }
  }

  def printForm(status: Results.Status,id:String,form:Form[ModuleForm],r:Call):Future[Result]={
    val futureAppoline=moduleDao.findApolline()
    val futureFirmware=firmwareDao.findFirmware()
    firmwareDao.findVersionFirmware().flatMap(versionFirmware=>
      futureFirmware.flatMap(firmware=>
        futureAppoline.map(apolline=>
          status(views.html.module.formModule(form, id, r,apolline.toList,firmware.toList,versionFirmware.toList))
        ).recover({case _=>InternalServerError("error")})
      ).recover({case _=>InternalServerError("error")})
    ).recover({case _=>InternalServerError("error")})
  }
}

@Api(value = "/module", description = "Operations for modules")
object ModuleManager extends ModuleManagerLike
