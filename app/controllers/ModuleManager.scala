package controllers

import java.util.Date

import com.wordnik.swagger.annotations._
import models._
import play.api.data.Form
import play.api.data.Forms._
import play.api.i18n.Messages
import play.api.libs.json.{Json, JsObject}
import play.api.mvc._
import play.modules.reactivemongo.json.BSONFormats
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
   commentaire:Option[String],
   send:String
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
      "commentaire"->optional(text),
      "send"->text
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
        val futureModule=moduleDao.findAll(Json.obj("delete"->false,"types"->BSONFormats.BSONObjectIDFormat.writes(BSONObjectID(id))),Json.obj(sort->sens))
        val futureFirmware=firmwareDao.findAll()
        //Find the module type
        typeModuleDao.findById(BSONObjectID(id)).flatMap(
          data=> data match{

              //If module type not found
            case None => future{Redirect(routes.TypeModuleManager.inventary())}

              //If module type found
            case Some(typeModule) => {
              futureModule.flatMap(listModule=>
                futureFirmware.map(firmware=>
                  Ok(views.html.module.listModule(typeModule,listModule,firmware,sort,sens))
                ).recover({case _=>InternalServerError("error")})
              ).recover({case _=>InternalServerError("error")})

            }
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
          printForm(Results.Ok,id,form,routes.ModuleManager.moduleInsert(id))
        }{_=>
          //Print an empty form with error type not found
          printForm(Results.BadRequest,id,form.withGlobalError(Messages("inventary.typeModule.error.typeNotExist")),routes.SensorManager.sensorInsert(id))
        }
      }
  }

  /**
   * This method is call when the user submit a form for insert new module
   * @return Return Ok Action when the user module was insert and return prefilled form for insert a new module
   *         Return Redirect Action when the user is not log in or if module was insert
   *         Return Bad request Action if the form was submit with data error
   *         Return Internal Server Error Action when have mongoDB error
   */
  @ApiOperation(
    nickname = "inventary/module/insert",
    value = "Insert a new module",
    notes = "Insert a new module to the mongoDB database",
    httpMethod = "POST")
  @ApiResponses(Array(
    new ApiResponse(code=303,message="<ul><li>Move resource to the login page at /login if the user is not log</li><li>Move resource to the sensors inventary at /inventary/sensors/:id if sensor was insert</li></ul>"),
    new ApiResponse(code=400,message="Fields required or not valid"),
    new ApiResponse(code=500,message="Have a mongoDB error")
  ))
  @ApiImplicitParams(Array(
    new ApiImplicitParam(value = "Id of the module type",required=true,name="id", dataType = "String", paramType = "path"),
    new ApiImplicitParam(value = "Module id",required=true,name="id", dataType = "String", paramType = "form"),
    new ApiImplicitParam(value = "Acquisition date of the module",required=true,name="acquisition", dataType = "Date", paramType = "form"),
    new ApiImplicitParam(value = "First use date of the sensor",name="firstUse", dataType = "Date", paramType = "form"),
    new ApiImplicitParam(value = "Flag indicate if the sensor is out of order",name="hs",dataType = "Boolean", paramType = "form"),
    new ApiImplicitParam(value = "Comment for the sensor",required=true,name="commentaire",dataType = "String", paramType = "form"),
    new ApiImplicitParam(value = "Value on the button used for send the form",required=true,name="send",defaultValue="Envoyer et continuer",dataType="String",paramType="form")
  ))
  def moduleInsert(id:String)=Action.async {
    implicit request =>

      //Verify if the user is connect and if data received are valid
      submitForm(id,routes.ModuleManager.moduleInsert(id)){

        //Filter for verify if module exists
        moduleData=>Json.obj("id" -> moduleData.id, "types" -> BSONFormats.BSONObjectIDFormat.writes(BSONObjectID(id)))

      }{
        (moduleData,firmware)=>

          //Insert the module into the mongoDB database
          moduleDao.insert(
            Module(
              id = moduleData.id,
              types = BSONObjectID(id),
              firmware = firmware,
              acquisition = moduleData.acquisition,
              firstUse = moduleData.firstUse,
              hs = moduleData.hs,
              commentaire = moduleData.commentaire,
              agregateur=moduleData.agregateur,
              apolline=moduleData.apolline
            )
          ).flatMap(e=>
            //When the module was insert
            moduleData.send match {

              //If use click on the button "Envoyer et continuer"
              case "Envoyer et continuer" =>{
                //Prepare prefilled data
                val moduleForm=moduleData.copy(id="")
                //Print the form with prefilled data
                printForm(Results.Ok,id,form.fill(moduleForm),routes.ModuleManager.moduleInsert(id))
              }

              //If user click on an other button redirect her to the module inventary for the current module type
              case _ => future{Redirect(routes.ModuleManager.inventary(id))}
            }
            ).recover({
            //Send Internal Server Error if have mongoDB error
            case e => InternalServerError("error")
          })
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

  /**
   * Verify if the user is connect and if data received are valid then apply function dedicated
   * @param id Module type id
   * @param routeSubmit Route use for submit the form
   * @param verif Function use for get module selector
   * @param f Function dedicated
   * @param request
   * @return Return Bad request Action if the form is not valid
   *         Return Redirect if dedicated function is a success
   *         Return Internal server error if have mongoDB error
   */
  def submitForm(id:String,routeSubmit:Call)(verif:ModuleForm=>JsObject)(f:(ModuleForm,BSONObjectID)=>Future[Result])(implicit request: Request[AnyContent]):Future[Result]={
    //Verify if user is connect
    UserManager.doIfconnectAsync(request) {
      //Verify if sensor type found
      typeModuleManager.doIfTypeModuleFound(BSONObjectID(id)) {_=>
        form.bindFromRequest.fold(

          //If form contains errors
          formWithErrors => {
            //the form is redisplay with error descriptions
            printForm(Results.BadRequest,id,formWithErrors,routeSubmit)
          },

          // Else if form no contains errors
          moduleData => {
            val formDate=verifyErrorAcquisitionAfterFirstUse(moduleData,form)
            if(formDate.equals(form)) {
              //Find the module
              moduleDao.findOne(verif(moduleData)).flatMap(
                module => module match {

                  //If module not found, execute dedicated function
                  case None => insertFirmwareIfNotFound(moduleData, f)

                  //If module found, return bad request with prefilled form
                  case _ => printForm(Results.BadRequest, id, form.withGlobalError(Messages("inventary.module.error.moduleExist")).fill(moduleData), routeSubmit)
                }
              ).recover({
                //Send Internal Server Error if have mongoDB error
                case e => InternalServerError("error")
              })
            }else{
              printForm(Results.BadRequest, id, formDate.fill(moduleData), routeSubmit)
            }
          }
        )
      } {_=> printForm(Results.BadRequest,id,form.withGlobalError(Messages("inventary.typeModule.error.typeNotExist")),routeSubmit)}
    }
  }

  /**
   * Insert, if not exist, the firmware into the database before applyed the module dedicated function
   * @param moduleData Data received when the form was submit
   * @param f Dedicated function
   * @return
   */
  def insertFirmwareIfNotFound(moduleData:ModuleForm,f:(ModuleForm,BSONObjectID)=>Future[Result]): Future[Result] ={
    //Find the firmware
    firmwareDao.findOne(Json.obj("nom"->moduleData.firmware,"version"->moduleData.versionFirmware)).flatMap(
      data => data match{

        //If the firmware not found
        case None=>{
          val firmware=Firmware(nom=moduleData.firmware,version=moduleData.versionFirmware)
          //Insert the firmware into the database then execute the module dedicated function
          firmwareDao.insert(firmware).flatMap(_=>f(moduleData,firmware._id)).recover({case _=> InternalServerError("error 1")})
        }

        //If the firmware found, execute the module dedicated function
        case Some(firmware)=>f(moduleData,firmware._id)
      }
    ).recover({case _=>InternalServerError("error")})
  }

  /**
   * Verify if acquisition date is before first use date
   * @param moduleData Data received from the form
   * @param form The current form
   * @return Return the form after verification
   */
  def verifyErrorAcquisitionAfterFirstUse(moduleData:ModuleForm,form:Form[ModuleForm]):Form[ModuleForm]={
    //If first use date is defined and acquisition date is after
    if(moduleData.firstUse.nonEmpty && moduleData.acquisition.after(moduleData.firstUse.get)){
      //Return form with an error
      form.withError("firstUse",Messages("inventary.module.error.firstUseBeforeAcquisition"))
    }else{
      form
    }
  }
}

@Api(value = "/module", description = "Operations for modules")
object ModuleManager extends ModuleManagerLike