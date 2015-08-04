package controllers

import java.lang.annotation.Annotation

import com.wordnik.swagger.annotations._
import models._
import play.api.data.Form
import play.api.data.Forms._
import play.api.libs.json.{JsObject, JsValue, JsArray, Json}
import play.api.mvc._
import play.modules.reactivemongo.json.BSONFormats.BSONObjectIDFormat
import reactivemongo.bson.BSONObjectID

import scala.collection.immutable.HashSet
import scala.concurrent._

import scala.concurrent.ExecutionContext.Implicits.global

case class ConfigurationForm(port:String,timeout:Int=10000,baud:Int=9600,bits:Int=8,stopBits:Int=1,parity:Int=0,timeFilter:Int=1000,types:String)

case class InfoMesureForm(index:Int,id:String,mesure:String,unite:String)
/**
 * This trait is a controller for manager module configuration
 */
trait ConfigurationManagerLike extends Controller{

  implicit val formatsForm=Json.format[ConfigurationForm]
  implicit val formatsFormMesure=Json.format[InfoMesureForm]

  val form=Form[ConfigurationForm](
    mapping(
      "port"->nonEmptyText,
      "timeout"->number(min=0),
      "baud"->number(min=0),
      "bits"->number(min=5,max=8),
      "stopBits"->number(min=1,max=3),
      "parity"->number(min=0,max=4),
      "timeFilter"->number(min=0),
      "types"->nonEmptyText
    )(ConfigurationForm.apply)(ConfigurationForm.unapply)
  )

  val formMesure=Form[InfoMesureForm](
    mapping(
      "index"->number(min=0),
      "id"->nonEmptyText,
      "mesure"->nonEmptyText,
      "unite"->nonEmptyText
    )(InfoMesureForm.apply)(InfoMesureForm.unapply)
  )

  val sensorsDao:SensorDao=SensorDaoObj

  val typeSensorsDao:TypeSensorDao=TypeSensorDaoObj

  val typeMesureDao:TypeMesureDao=TypeMesureDaoObj

  val moduleManager:ModuleManagerLike=ModuleManager

  /**
   * Display a form for insert module configuration
   * @param id Module id
   * @return A 200 OK page, with the form for insert module configuration
   *         Redirect if module not found or if the user is not log in
   */
  @ApiOperation(
    nickname = "inventary/modules/:id/configuration",
    value = "Display form for insert module configuration",
    notes = "Display form for insert module configuration",
    httpMethod = "GET")
  @ApiResponses(Array(
    new ApiResponse(code=303,message="<ul><li>Move resource to the login page at /login if the user is not log</li></ul>")
  ))
  @ApiImplicitParams(Array(
    new ApiImplicitParam(value = "Module id",required=true,name="id", dataType = "String", paramType = "path")
  ))
  def formConfiguration(id:String)=Action.async{
    implicit request =>
      //Verify if user is connect
      UserManager.doIfconnectAsync(request) {

        //Verify if module exists
        moduleManager.doIfModuleFound(BSONObjectID(id)){

          //Display the form for insert module configuration
          module=> future{Ok(views.html.configuration.form(form.fill(ConfigurationForm(port="",types = "")),module))}
        }{
           //Redirect to the list of modules
          _ =>
            future{Redirect(routes.ModuleManager.inventary())}
        }
      }
  }

  /**
   * Insert module configuration to the session and redirect to the page for set index associat to data receive
   * @param id Module id
   * @return Redirect if module not found or if the user is not log in
   *         Bad request with a prefilled form if form contains errors
   */
  @ApiOperation(
    nickname = "inventary/modules/:id/configuration",
    value = "Insert module configuration",
    notes = "Insert module configuration",
    httpMethod = "POST")
  @ApiResponses(Array(
    new ApiResponse(code=303,message="<ul><li>Move resource to the login page at /login if the user is not log</li><li>Move resource to the list of sensors at /inventary/modules/:id/configuration/sensors</li></ul>"),
    new ApiResponse(code=400,message="Form was submit with an error")
  ))
  @ApiImplicitParams(Array(
    new ApiImplicitParam(value = "Module id",required=true,name="id", dataType = "String", paramType = "path"),
    new ApiImplicitParam(value = "Port name",required=true,name="port", dataType = "String", paramType = "body"),
    new ApiImplicitParam(value = "Connection timeout to the port",required=true,name="timeout", dataType = "Int", paramType = "body"),
    new ApiImplicitParam(value = "Baud number",required=true,name="baud", dataType = "Int", paramType = "body"),
    new ApiImplicitParam(value = "Bits number",required=true,name="bits", dataType = "Int", paramType = "body"),
    new ApiImplicitParam(value = "Parity",required=true,name="parity", dataType = "Int", paramType = "body"),
    new ApiImplicitParam(value = "Stop bits",required=true,name="stopBits", dataType = "Int", paramType = "body"),
    new ApiImplicitParam(value = "Time for filter data received",required=true,name="timeFilter", dataType = "Int", paramType = "body"),
    new ApiImplicitParam(value = "Type name of datalogger",required=true,name="types", dataType = "String", paramType = "body")
  ))
  def insertFormConfiguration(id:String)=Action.async{
    implicit request =>
      //Verify if user is connect
      UserManager.doIfconnectAsync(request) {

        //Verify if module exists
        moduleManager.doIfModuleFound(BSONObjectID(id)){

          module=> form.bindFromRequest().fold(

            //If form contains errors, send bad request with a prefilled form
            formWithError =>future{BadRequest(views.html.configuration.form(formWithError,module))},

            //If contains not errors, redirect to the list of sensors contains in the module
            data=>future{Redirect(routes.ConfigurationManager.listSensors(id)).withSession(request.session + ("configForm"->"insert") + ("config"->Json.stringify(formatsForm.writes(data))))}
          )
        }{

          //Redirect to the list of modules
          _ =>future{Redirect(routes.ModuleManager.inventary())}
        }
      }
  }

  /**
   * Display the list of sensors associat to the module
   * @param id Module id
   * @return Redirect if user is not log in or if module not found
   *         200 OK page with the list of sensors
   */
  @ApiOperation(
    nickname = "inventary/modules/:id/configuration/sensors",
    value = "Display the list of sensors in the module",
    notes = "Display the list of sensors in the module",
    httpMethod = "GET")
  @ApiResponses(Array(
    new ApiResponse(code=303,message="<ul><li>Move resource to the login page at /login if the user is not log</li><li>Move resource to modules inventary at /inventary/modules if module not found</li></ul>")
  ))
  @ApiImplicitParams(Array(
    new ApiImplicitParam(value = "Module id",required=true,name="id", dataType = "String", paramType = "path")
  ))
  def listSensors(id:String)=Action.async{
    implicit request =>
      //Verify if user is connect
      UserManager.doIfconnectAsync(request) {

        //Verify if module exists
        moduleManager.doIfModuleFound(BSONObjectID(id)) {

          module =>{

           //Find the list of sensors with sensor type and mesure type
           findListSensor(module.capteurs).map(

            data => data match{

              //Display the list of sensors
              case (typeSensor,typeMesure,sensor)=>Ok(views.html.configuration.listSensor(module,typeSensor,typeMesure,sensor))

            }
           )
          }
        }{

          //Redirect to the list of modules
          _ =>future{Redirect(routes.ModuleManager.inventary())}
        }
      }
  }

  /**
   * Display form to insert mesure information for module configuration
   * @param id Module id
   * @param idSensor Sensor id
   * @return Redirect if user is not log in or if module not found
   *         200 Ok page with the form
   */
  @ApiOperation(
    nickname = "inventary/modules/:id/configuration/sensors/:id",
    value = "Display form for insert mesure information",
    notes = "Display form for insert mesure information",
    httpMethod = "GET")
  @ApiResponses(Array(
    new ApiResponse(code=303,message="<ul><li>Move resource to the login page at /login if the user is not log</li><li>Move resource to modules inventary at /inventary/modules if module not found</li><li>Move resouce to the list of sensors at /inventary/modules/:id/configuration/sensors/:id2 if sensor not found</li></ul>")
  ))
  @ApiImplicitParams(Array(
    new ApiImplicitParam(value = "Module id",required=true,name="id", dataType = "String", paramType = "path"),
    new ApiImplicitParam(value = "Sensor id",required=true,name="id2", dataType = "String", paramType = "path")
  ))
  def formInfoMesure(id:String,idSensor:String)=Action.async{
    implicit request =>
      //Verify if user is connect
      UserManager.doIfconnectAsync(request) {

        //Verify if module and sensor are found
        verifyModuleAndSensorFound(id,idSensor) {

          module => {

            //Find the list of mesure name
            typeMesureDao.findListMesure("mesure").flatMap(
              mesure=>

                //Find the list mesure unity
                typeMesureDao.findListUnite("unite").map(

                  //Display the form
                  unite=>Ok(views.html.configuration.formMesure(formMesure,module,mesure.toList,unite.toList,idSensor))
                )
            )
          }
        }
      }
  }

  /**
   * Insert mesure information to the session and redirect to the page for select sensors
   * @param id Module id
   * @param idSensor Sensor id
   * @return Redirect if user is not log in, if module not found or after insert mesure information to the session
   *         Bad request if data received ara not valid
   */
  @ApiOperation(
    nickname = "inventary/modules/:id/configuration/sensors/:id",
    value = "Insert mesure information in the session",
    notes = "Insert mesure information in the session",
    httpMethod = "GET")
  @ApiResponses(Array(
    new ApiResponse(code=303,message="<ul><li>Move resource to the login page at /login if the user is not log</li><li>Move resource to modules inventary at /inventary/modules if module not found</li><li>Move resouce to the list of sensors at /inventary/modules/:id/configuration/sensors/:id2 if sensors not found or after insert module information</li></ul>")
  ))
  @ApiImplicitParams(Array(
    new ApiImplicitParam(value = "Module id",required=true,name="id", dataType = "String", paramType = "path"),
    new ApiImplicitParam(value = "Sensor id",required=true,name="id2", dataType = "String", paramType = "path"),
    new ApiImplicitParam(value = "Index where get the sensor valure",required=true,name="index", dataType = "Int", paramType = "body"),
    new ApiImplicitParam(value = "Mesure information id",required=true,name="id", dataType = "String", paramType = "body"),
    new ApiImplicitParam(value = "Name of the mesure",required=true,name="mesure", dataType = "String", paramType = "body"),
    new ApiImplicitParam(value = "Unity of the mesure",required=true,name="unite", dataType = "String", paramType = "body")
  ))
  def insertInfoMesure(id:String,idSensor:String) = Action.async{
    implicit request =>
      //Verify if user is connect
      UserManager.doIfconnectAsync(request) {

        //Verify if module and sensor are found
        verifyModuleAndSensorFound(id,idSensor){
          module =>formMesure.bindFromRequest().fold(

            //If form contains errors, send bad request with a prefilled form
            formWithError =>{
              //Find the list of mesure name
              typeMesureDao.findListMesure("mesure").flatMap(
                mesure=>

                  //Find the list mesure unity
                  typeMesureDao.findListUnite("unite").map(

                    //Display the form
                    unite=>BadRequest(views.html.configuration.formMesure(formWithError,module,mesure.toList,unite.toList,idSensor))
                  )
              )
            },

            //If contains not errors, redirect to the list of sensors contains in the module
            data=>future{Redirect(routes.ConfigurationManager.listSensors(id)).withSession(request.session + ("infoMesure"->Json.stringify(createJsonInfoMesure(idSensor,data))))}
          )
        }
      }
  }

  /**
   * Find the list of sensors, type sensors and type mesure for sensors associat to the module
   * @param listSensors List of sensors associat to the module
   * @return The list of sensors, type sensors and type mesure
   */
  def findListSensor(listSensors:List[BSONObjectID]):Future[(List[TypeSensor],List[TypeMesure],List[Sensor])]={
    //Get the list of type sensors for sensors selected available
    val selector=Json.obj("delete"->false,"_id" -> Json.obj("$in" -> JsArray(listSensors.mapConserve(id=>BSONObjectIDFormat.writes(id)).asInstanceOf[List[JsValue]].toSeq)))
    val getTypeSensors = sensorsDao.fold(selector, Json.obj(), HashSet[BSONObjectID]())((set, sensors) => set + sensors.types).flatMap(
      types => typeSensorsDao.findAll(Json.obj("delete"->false,"_id" -> Json.obj("$in" -> JsArray(types.toList.mapConserve(p => BSONObjectIDFormat.writes(p)).asInstanceOf[List[JsValue]].toSeq))))
    )

    //Get the list of type mesure
    val getMesure = typeMesureDao.findAll()

    //Find sensors
    sensorsDao.findAll(selector).flatMap(
      sensors =>
        getTypeSensors.flatMap(
          typeSensors =>
            getMesure.map(

              typeMesure => (typeSensors, typeMesure, sensors)

            )
        )
    )
  }

  /**
   * Insert mesure information to an JSON array
   * @param id Sensor id
   * @param info Mesure information
   * @param request HttpRequest received
   * @return A JSON array with the mesure information
   */
  def createJsonInfoMesure(id:String,info:InfoMesureForm)(implicit request:Request[AnyContent]): JsValue =
  //Find the JSON array into the session
  request.session.get("infoMesure") match{

      //If the JSON array is not found, create new JSON array with the mesure information
    case None => JsArray(List(Json.obj("sensor"->id,"info"->info)))

      //If the JSON array is found, insert the mesure information
    case Some(data)=>Json.parse(data).as[JsArray] :+ Json.obj("sensor"->id,"info"->info)
  }

  /**
   * Verify if module and sensor are found. If they are found then execute function pass in parameter, else redirect to module inventary or sensor list
   * @param idModule Module id
   * @param idSensor Sensor if
   * @param f Function execute if module and sensor are found
   * @param request
   * @return The result of the parameter function
   *         Redirect to the module inventary or the sensor list
   */
  def verifyModuleAndSensorFound(idModule:String,idSensor:String)(f:Module=>Future[Result])(implicit request:Request[AnyContent]):Future[Result]={
    //Verify if module exists
    moduleManager.doIfModuleFound(BSONObjectID(idModule)) {

      //Find the sensor
      module=>sensorsDao.findOne(Json.obj("delete"->false,"_id"->BSONObjectID(idSensor))).flatMap(

        data=>data match{

            //If the sensor not found, redirect to the sensor list
         case None => future{Redirect(routes.ConfigurationManager.listSensors(idModule))}

            //If the sensor found, execute parameter function
         case _=>f(module)
       }
      )
    }{
      //Redirect to the list of modules
      _ =>future{Redirect(routes.ModuleManager.inventary())}
    }
  }
}

@Api(value = "/configuration", description = "Operations for modules configuration")
object ConfigurationManager extends ConfigurationManagerLike
