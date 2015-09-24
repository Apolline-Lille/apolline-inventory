package controllers

import java.io.ByteArrayOutputStream
import java.lang.annotation.Annotation
import java.util.zip.{ZipEntry, ZipOutputStream}

import com.wordnik.swagger.annotations._
import models._
import play.api.Play
import play.api.data.Form
import play.api.data.Forms._
import play.api.i18n.Messages
import play.api.libs.iteratee.Enumerator
import play.api.libs.json.{JsObject, JsValue, JsArray, Json}
import play.api.mvc._
import play.modules.reactivemongo.json.BSONFormats.BSONObjectIDFormat
import reactivemongo.bson.BSONObjectID

import scala.collection.immutable.HashSet
import scala.concurrent._

import scala.concurrent.ExecutionContext.Implicits.global
import play.api.Play.current

case class ConfigurationForm(port:String,timeout:Int=10000,baud:Int=9600,bits:Int=8,stopBits:Int=1,parity:Int=0,timeFilter:Int=1000,types:String,numberOfValue:Int)

case class InfoMesureForm(index:Int,espece:String)
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
      "types"->nonEmptyText,
      "numberOfValue"->number(min=0)
    )(ConfigurationForm.apply)(ConfigurationForm.unapply)
  )

  val formMesure=Form[InfoMesureForm](
    mapping(
      "index"->number(min=0),
      "espece"->nonEmptyText
    )(InfoMesureForm.apply)(InfoMesureForm.unapply)
  )

  val sensorsDao:SensorDao=SensorDaoObj

  val typeSensorsDao:TypeSensorDao=TypeSensorDaoObj

  val typeMesureDao:TypeMesureDao=TypeMesureDaoObj

  val moduleDao:ModuleDao=ModuleDaoObj

  val informationMesureDao:InformationMesureDao=InformationMesureDaoObj

  val configurationDao:ConfigurationDao=ConfigurationDaoObj

  val especeDao:EspeceDao=EspeceDaoObj

  val moduleManager:ModuleManagerLike=ModuleManager

  val zipOutputStreamBuilder=new ZipOutputStreamBuilder

  val appConfig=Play.configuration

  /**
   * Display information about the configuration
   * @param id Module id
   * @param id2 Configuration id
   * @return Redirect if the user is not log in, if the module is not found or if the configuration is not found
   *         200 OK page with configuration information
   */
  @ApiOperation(
    nickname = "inventary/modules/:id/configuration/:id2",
    value = "Display a configuration",
    notes = "Display a configuration",
    httpMethod = "GET")
  @ApiResponses(Array(
    new ApiResponse(code=303,message="<ul><li>Move resource to the login page at /login if the user is not log</li><li>Move resouce to the module inventary at /inventary/modules if the module is not found</li><li>Move resource to module information if the configuration is not found</li></ul>")
  ))
  @ApiImplicitParams(Array(
    new ApiImplicitParam(value = "Module id",required=true,name="id", dataType = "String", paramType = "path"),
    new ApiImplicitParam(value = "configuration id",required=true,name="id",dataType="String",paramType="path")
  ))
  def configuration(id:String,id2:String)=Action.async {
    implicit request =>
      //Verify if user is connect
      UserManager.doIfconnectAsync(request) {

        //Verify if module exists
        moduleManager.doIfModuleFound(BSONObjectID(id)) {

          //Find the configuration
          module => configurationDao.findOne(Json.obj("_id" -> BSONObjectID(id2))).flatMap(
            configOpt => configOpt match {

              //If the configuration is not found
              case None => future{Redirect(routes.ModuleManager.moreInformation(id))}

              //if the configuration is found, find mesure informations
              case Some(config) => informationMesureDao.findAll(Json.obj("_id"->Json.obj("$in"->config.infoMesure))).flatMap(
                infos=>{

                  //Find id of sensors and mesure type
                  val ids=infos.foldLeft(HashSet[BSONObjectID]()){(set,info)=>set + info.sensor}

                  //Find sensors information
                  findListSensor(ids.toList).map(
                    data=>data match{

                        //Display the configuration information
                      case (typeSensor,espece, typeMesure, sensor)=>Ok(views.html.configuration.showConfiguration(module, config,infos,typeSensor,sensor))
                    }
                  )
                }
              )
            }
          )
        } {
          //Redirect to the list of modules
          _ =>
            future {
              Redirect(routes.ModuleManager.inventary())
            }
        }
      }
  }

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
          module=> future{Ok(views.html.configuration.form(form.fill(ConfigurationForm(port="",types = "",numberOfValue=0)),module)).withSession(request.session + ("configForm"->"insert") + ("config"->Json.stringify(formatsForm.writes(ConfigurationForm(port="",types="",numberOfValue=0)))) + ("infoMesure"->"[]"))}
        }{
           //Redirect to the list of modules
          _ =>
            future{Redirect(routes.ModuleManager.inventary())}
        }
      }
  }

  /**
   * Display a form for update a module configuration
   * @param id Module id
   * @param id2 Configuration id
   * @return A 200 OK page, with the form for update module configuration
   *         Redirect if the module is not found, if the user is not log in, or if the configuration is not found
   */
  @ApiOperation(
    nickname = "inventary/modules/:id/configuration/:id2/update",
    value = "Display form for update a module configuration",
    notes = "Display form for update a module configuration",
    httpMethod = "GET")
  @ApiResponses(Array(
    new ApiResponse(code=303,message="<ul><li>Move resource to the login page at /login if the user is not log</li><li>Move resource to module inventary at /inventary/modules if module is not found</li><li>Move resource to module information at /inventary/modules/:id if configuration is not found</li></ul>")
  ))
  @ApiImplicitParams(Array(
    new ApiImplicitParam(value = "Module id",required=true,name="id", dataType = "String", paramType = "path"),
    new ApiImplicitParam(value = "Configuration id", required=true,name="id2",dataType="String",paramType="path")
  ))
  def formUpdate(id:String,id2:String)=Action.async{
    implicit request =>
    //Verify if user is connect
    UserManager.doIfconnectAsync(request) {

      //Verify if module exists
      moduleManager.doIfModuleFound(BSONObjectID(id)){

        module=>
          //Find the configuration
          configurationDao.findOne(Json.obj("_id"->BSONObjectID(id2))).flatMap(
            configOpt=>configOpt match{

              //If configuration is not found redirect
              case None=>future{Redirect(routes.ModuleManager.moreInformation(id))}

              //If configuration is found, find tis mesure informations
              case Some(config)=>findInformationForForm(config.infoMesure).map(
                infos=>{
                  val configForm=ConfigurationForm(config.port,config.timeout,config.baud,config.bits,config.stopBits,config.parity,config.timeFilter,config.types,config.numberOfValue)

                  //Display the form
                  Results.Ok(views.html.configuration.form(form.fill(configForm),module)).withSession(request.session + ("configForm"->"update") + ("config"->Json.stringify(formatsForm.writes(configForm))) + ("configUpdate"->config._id.stringify) + ("infoMesure"->Json.stringify(JsArray(infos))))
                }
              )
            }
          )
      }{
        //Redirect to the list of modules
        _ =>
          future{Redirect(routes.ModuleManager.inventary())}
      }
    }
  }

  /**
   * Display a form for update current module configuration
   * @param id Module id
   * @return A 200 OK page, with the form for update current module configuration
   *         Redirect if module not found or if the user is not log in
   */
  @ApiOperation(
    nickname = "inventary/modules/:id/configuration/update",
    value = "Display form for update current module configuration",
    notes = "Display form for update current module configuration",
    httpMethod = "GET")
  @ApiResponses(Array(
    new ApiResponse(code=303,message="<ul><li>Move resource to the login page at /login if the user is not log</li><li>Move resource to the module inventary at /inventary/modules if module not found</li></ul>")
  ))
  @ApiImplicitParams(Array(
    new ApiImplicitParam(value = "Module id",required=true,name="id", dataType = "String", paramType = "path")
  ))
  def formUpdateCurrent(id:String)=Action.async{
    implicit request =>
      //Verify if user is connect
      UserManager.doIfconnectAsync(request) {

        //Verify if module exists
        moduleManager.doIfModuleFound(BSONObjectID(id)){

          //Display the form
          module=> future{Ok(views.html.configuration.form(form.fill(getConfiguration),module))}
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
            data=>future{Redirect(routes.ConfigurationManager.listSensors(id)).withSession(request.session + ("config"->Json.stringify(formatsForm.writes(data))))}
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
              case (typeSensor,espece,typeMesure,sensor)=>Ok(views.html.configuration.listSensor(module,typeSensor,espece,typeMesure,sensor))

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

          (module,sensor) => {
            typeSensorsDao.findOne(Json.obj("_id"->sensor.types)).flatMap(
              typeOpt=>typeOpt match{
                case None=>future{Redirect(routes.ConfigurationManager.listSensors(id))}
                case Some(types)=>especeDao.findAll(Json.obj("_id"->Json.obj("$in"->types.espece))).map{
                  espece=>Ok(views.html.configuration.formMesure(formMesure,module,espece,idSensor))
                }
              }
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
          (module,sensor) =>formMesure.bindFromRequest().fold(

            //If form contains errors, send bad request with a prefilled form
            formWithError =>{
              typeSensorsDao.findOne(Json.obj("_id"->sensor.types)).flatMap(
                typeOpt=>typeOpt match{
                  case None=>future{Redirect(routes.ConfigurationManager.listSensors(id))}
                  case Some(types)=>especeDao.findAll(Json.obj("_id"->Json.obj("$in"->types.espece))).map{
                    espece=>BadRequest(views.html.configuration.formMesure(formWithError,module,espece,idSensor))
                  }
                }
              )
            },

            //If contains not errors, redirect to the list of sensors contains in the module
            data=>future{Redirect(routes.ConfigurationManager.listSensors(id)).withSession(request.session + ("infoMesure"->Json.stringify(createJsonInfoMesure(idSensor,data))))}
          )
        }
      }
  }

  /**
   * Display the summary of the configuration before validate and insert it in mongoDB
   * @param id Module id
   * @return Redirect if user is not log in or if module is not found
   *         Ok with the summary if the configuration haven't got errors
   *         Bad request  with the summary and error message
   */
  @ApiOperation(
    nickname = "inventary/modules/:id/configuration/validation",
    value = "Display the summary of the configuration before insert it to mongoDB",
    notes = "Display the summary of the configuration before insert it to mongoDB",
    httpMethod = "GET")
  @ApiResponses(Array(
    new ApiResponse(code=303,message="<ul><li>Move resource to the login page at /login if the user is not log</li><li>Move resource to modules inventary at /inventary/modules if module not found</li></ul>"),
    new ApiResponse(code=400,message="The configuration contains an error")
  ))
  @ApiImplicitParams(Array(
    new ApiImplicitParam(value = "Module id",required=true,name="id", dataType = "String", paramType = "path")
  ))
  def formValidation(id:String)=Action.async{
    implicit request =>
      //Verify if user is connect
      UserManager.doIfconnectAsync(request) {

        //Verify if module exists
        moduleManager.doIfModuleFound(BSONObjectID(id)) {

          module =>{
            //Find the configuration
            val config=getConfiguration

            //Find mesure informations
            val infoMesure=getInfoMesure

            //Verify configuration and mesure informations
            val errors=verifyInfoMesure(infoMesure,verifyConfiguration(config,List()))

            //Display the summary of the configuration
            display_validation((if(errors.isEmpty){Results.Ok}else{Results.BadRequest}),module,config,infoMesure,errors)
          }
        }{

          //Redirect to the list of modules
          _ =>future{Redirect(routes.ModuleManager.inventary())}
        }
      }
  }

  /**
   * Validate the configuration and insert it in mongoDB
   * @param id Module id
   * @return Bad request if configuration is not valid
   *         Redirect after insert the configuration in mongoDB or if user is not log or if module not found
   */
  @ApiOperation(
    nickname = "inventary/modules/:id/configuration/validation",
    value = "Validate and insert the configuration",
    notes = "Validate and insert the configuration",
    httpMethod = "POST")
  @ApiResponses(Array(
    new ApiResponse(code=303,message="<ul><li>Move resource to the login page at /login if the user is not log</li><li>Move resource to modules inventary at /inventary/modules if module not found</li><li>The configuration is valid and insert in mongoDB then redirect to module informations</li></ul>"),
    new ApiResponse(code=400,message="The configuration contains an error")
  ))
  @ApiImplicitParams(Array(
    new ApiImplicitParam(value = "Module id",required=true,name="id", dataType = "String", paramType = "path"),
    new ApiImplicitParam(value = "Sensor id",required=true,name="id2", dataType = "String", paramType = "path"),
    new ApiImplicitParam(value = "Index where get the sensor valure",required=true,name="index", dataType = "Int", paramType = "body"),
    new ApiImplicitParam(value = "Mesure information id",required=true,name="id", dataType = "String", paramType = "body"),
    new ApiImplicitParam(value = "Name of the mesure",required=true,name="mesure", dataType = "String", paramType = "body"),
    new ApiImplicitParam(value = "Unity of the mesure",required=true,name="unite", dataType = "String", paramType = "body")
  ))
  def validation(id:String)=Action.async{
    implicit request =>
      //Verify if user is connect
      UserManager.doIfconnectAsync(request) {

        //Verify if module exists
        moduleManager.doIfModuleFound(BSONObjectID(id)) {

          module => {
            //Find the configuration
            val config = getConfiguration

            //Find mesure informations
            val infoMesure = getInfoMesure

            //Verify configuration and mesure informations
            val errors = verifyInfoMesure(infoMesure, verifyConfiguration(config, List()))
            if(errors.nonEmpty){

              //Display the summary of the configuration
              display_validation(Results.BadRequest,module,config,infoMesure,errors)
            }else{
              if(request.session.get("configForm").equals(Some("update"))) {
                updateConfiguration(id,request.session.get("configUpdate").getOrElse(""),config,infoMesure)
              }else{
                //Insert the configuration to mongoDB
                insertConfiguration(id, config, infoMesure)
              }
            }
          }
        }{

          //Redirect to the list of modules
          _ =>future{Redirect(routes.ModuleManager.inventary())}
        }
      }
  }

  /**
   * Create and download a zip with module configurations
   * @param id Module id
   * @return 404 Not found if module not exist
   *         Generate ZIP with module configurations
   */
  @ApiOperation(
    nickname = "inventary/modules/:id/configuration/download",
    value = "Create and download a zip with module configurations",
    notes = "Create and download a zip with module configurations",
    httpMethod = "GET")
  @ApiResponses(Array(
    new ApiResponse(code=303,message="Move resource to the login page at /login if the user is not log"),
    new ApiResponse(code=404,message="Module not found")
  ))
  @ApiImplicitParams(Array(
    new ApiImplicitParam(value = "Module id",required=true,name="id", dataType = "String", paramType = "path")
  ))
  def downloadConfiguration(id:String)= Action.async{
    implicit request =>
      //Verify if user is connect
      UserManager.doIfconnectAsync(request) {

        //Verify if module exists
        moduleManager.doIfModuleFound(BSONObjectID(id)) {

          module => {

            //Find module configuration
            configurationDao.findAll(Json.obj("_id"->Json.obj("$in"->module.configuration))).flatMap(
              config=>

                //Find mesure informations associat to module configurations
                informationMesureDao.findAll(Json.obj("_id"->Json.obj("$in"->config.foldLeft(List[BSONObjectID]())((list,conf)=>list ++ conf.infoMesure)))).map(
                  infos=>{

                    //Create a zip with configurations file
                    val zip = create_zip(id,config,infos)

                    //Return the zip
                    Result(
                      header = ResponseHeader(200),
                      body = Enumerator(zip)
                    ).withHeaders(
                      "Content-Type"->"application/zip",
                      "Content-Disposition"->"attachment; filename=configuration.zip"
                    )
                  }
                )
            )
          }
        }{

          //Redirect to the list of modules
          _ =>future{NotFound}
        }
      }
  }

  /**
   * Find mesure informations associat to a module and format its informations for set in session
   * @param infos List of informations id
   * @return Return a list of mesure informations format for set in session
   */
  def findInformationForForm(infos:List[BSONObjectID]):Future[List[JsObject]]=infos match{
    //If the list is empty return an empty list
    case Nil => future{List()}

    //Else find mesure informations
    case _ => informationMesureDao.findAll(Json.obj("_id"->Json.obj("$in"->infos))).map(
      data=>
        //for each mesure information
        data.foldLeft(List[JsObject]())((list,info)=>{

          //Insert to the list a Json represent the mesure information
          Json.obj("sensor"->info.sensor.stringify,"info"->InfoMesureForm(info.index,info.espece.stringify)) :: list
        })
    )
  }

  /**
   * Create a zip with file of module configurations
   * @param id Module id
   * @param config Module configurations
   * @param infos Mesure informations associat to the configuration
   * @return Byte code of the zip
   */
  def create_zip(id:String,config:List[Configuration],infos:List[InformationMesure])={
    //Initialize byte array for save zip informations
    val os=new ByteArrayOutputStream

    //Create a zip file
    val zip = zipOutputStreamBuilder.createZipOutputStream(os)

    //For each module configurations
    config.foreach{ conf =>

      //Find mesure informations associat to the configuration
      val infoConfig=infos.filter(i=>conf.infoMesure.contains(i._id))

      //Create new file in the zip
      zip.putNextEntry(new ZipEntry(conf.types+".properties"))

      //Write configuration in the file
      write_configuration(zip,conf)

      //Write module id in the file
      zip.write(("moduleId="+id+"\n").getBytes)

      zip.write(("website="+appConfig.getString("hostname").getOrElse("")+"\n").getBytes)

      //Write mesure informations
      zip.write(("sensors={\\\n").getBytes)
      write_sensors_properties(zip,infoConfig.sortWith((info1,info2)=>info1.sensor.stringify.compareTo(info2.sensor.stringify)<0))
      zip.write(("}").getBytes)

      //End of the file
      zip.closeEntry()
    }

    //End of the zip
    zip.close()

    //Return byte code of the zip
    os.toByteArray
  }

  /**
   * Write module configurations on the file in the zip
   * @param zip Zip with module configurations
   * @param conf Module configuration
   */
  def write_configuration(zip:ZipOutputStream,conf:Configuration): Unit ={
    //Write the port
    zip.write(("device="+conf.port+"\n").getBytes)
    //Write the connection timeout
    zip.write(("timeout="+conf.timeout+"\n").getBytes)
    //Write the baud number
    zip.write(("baud="+conf.baud+"\n").getBytes)
    //Write the bits number
    zip.write(("bits="+conf.bits+"\n").getBytes)
    //Write the stop bits
    zip.write(("stopBits="+conf.stopBits+"\n").getBytes)
    //Write the parity
    zip.write(("parity="+conf.parity+"\n").getBytes)
    //Write the data time filter
    zip.write(("timeFilter="+conf.timeFilter+"\n").getBytes)
    //Write the type of datalogger
    zip.write(("type="+conf.types+"\n").getBytes)
    zip.write(("numberOfValue="+conf.numberOfValue+"\n").getBytes)
  }

  /**
   * Write sensors informations associat to a configuration
   * @param zip Zip with module configurations
   * @param sensors List of mesure informations
   * @param current Id of the current sensor
   */
  def write_sensors_properties(zip:ZipOutputStream,sensors:List[InformationMesure],current:BSONObjectID=null)=(sensors,current) match{
    //If not have mesure information
    case (Nil,null)=>
    //If is the end of mesure informations, write end of the array
    case (Nil,_)=>zip.write("\\\n]".getBytes)
    //If is the first mesure informations
    case (h::t,null) => {
      //Write sensor id and begin of the array
      zip.write(("\""+h.sensor.stringify+"\":[\\\n").getBytes)
      //Write mesure informations associat to the sensor
      write_info_mesure_properties(zip,sensors,h.sensor,0)
    }
    //If is a new mesure id
    case (h::t,_)=>{
      //Write end of the previous array, the sensor id and begin of the array
      zip.write(("\\\n],\\\n\""+h.sensor.stringify+"\":[\\\n").getBytes)
      //Write mesure informations associat to the sensor
      write_info_mesure_properties(zip,sensors,h.sensor,0)
    }
  }

  /**
   * Write mesure informations associat to a sensors
   * @param zip Zip with module configurations
   * @param sensors List of mesure informations
   * @param current Id of the current sensor
   * @param value Index of the mesure information
   */
  def write_info_mesure_properties(zip:ZipOutputStream,sensors:List[InformationMesure],current:BSONObjectID,value:Int):Unit=(sensors,current) match{
    //If not have mesure informations, write the end of mesure informations
    case (Nil,_)=>write_sensors_properties(zip,sensors,current)
    //If is a new sensor, write the end of the previous sensor and begin the new sensor
    case(h::t,bson) if ! bson.equals(h.sensor) =>write_sensors_properties(zip,sensors,current)
      //Write mesure information
    case (h::t,_) => {
      //If is a new mesure informations, write a comma
      if(value!=0) zip.write(",\\\n".getBytes)

      //Write index of the information
      zip.write(("{\"index\":"+h.index+",\"espece\":\""+h.espece.stringify+"\"}").getBytes)

      //Write new mesure information
      write_info_mesure_properties(zip,t,current,value+1)
    }
  }

  /**
   * Insert mesure informations in mongoDB
   * @param infos List of mesure informations
   * @return Informations _id
   */
  def insertInformation(infos:List[(BSONObjectID,InfoMesureForm)]):List[Future[BSONObjectID]]=infos match{
    //If not have informations
    case Nil=>List()

    //If have informations
    case h::t=>informationMesureDao.findOne(Json.obj("index"->h._2.index,"sensor"->h._1)).flatMap(
      data => data match{
        case None=>{
          //Create the mesure information
          val info=InformationMesure(index=h._2.index,sensor=h._1,espece=BSONObjectID(h._2.espece))

          //Insert the mesure information
          informationMesureDao.insert(info).map(
            data=>info._id
          )
        }
        case Some(info)=>future{info._id}
      }
    ) :: insertInformation(t)
  }

  /**
   * Update a module configuration in mongoDB
   * @param idModule Module id
   * @param id Configuration id
   * @param config The new configuration
   * @param infoMesure The list of mesure informations associat to the module
   * @param request HTTPRequest received
   * @return
   */
  def updateConfiguration(idModule:String,id:String,config:ConfigurationForm,infoMesure:List[(BSONObjectID,InfoMesureForm)])(implicit request:Request[AnyContent]):Future[Result]={
    //Transform the List[Future] to Future[List] after insert mesure information
    Future.sequence(insertInformation(infoMesure)).flatMap(list=> {

      //Create the configuration
      val configFinal = Configuration(BSONObjectID(id),config.port,config.timeout,config.baud,config.bits,config.stopBits,config.parity,config.timeFilter,config.types,config.numberOfValue, list)

      //Update the configuration
      configurationDao.updateById(BSONObjectID(id),configFinal).map(

        //redirect to the module informations
        data => Redirect(routes.ModuleManager.moreInformation(idModule)).withSession(request.session - "configForm" - "config" - "infoMesure" - "configUpdate")
      )
    })
  }

  /**
   * Insert the module configuration in mongoDB
   * @param id Module id
   * @param config Module configuration
   * @param infoMesure Mesure informations
   * @return Redirect after insert configuration in mongoDB
   */
  def insertConfiguration(id:String,config:ConfigurationForm,infoMesure:List[(BSONObjectID,InfoMesureForm)])(implicit request:Request[AnyContent]):Future[Result]={
    //Transform the List[Future] to Future[List] after insert mesure information
    Future.sequence(insertInformation(infoMesure)).flatMap(list=> {

      //Create the configuration
      val configFinal = Configuration(port = config.port, timeout = config.timeout, baud = config.baud, bits = config.bits, parity = config.parity, stopBits = config.stopBits, timeFilter = config.timeFilter, types = config.types,numberOfValue=config.numberOfValue, infoMesure = list)

      //Insert the configuration
      configurationDao.insert(configFinal).flatMap(

        //Update the module for associat the configuration to it
        data => moduleDao.updateById(BSONObjectID(id), Json.obj("$push" -> Json.obj("configuration" ->configFinal._id))).map(

          //Redirect to module informations
          data => Redirect(routes.ModuleManager.moreInformation(id)).withSession(request.session - "configForm" - "config" - "infoMesure")
        )
      )
    })
  }

  /**
   * Display the summary of the configuration
   * @param status Status of the response
   * @param module Module associat to the configuration
   * @param config The configuration
   * @param infoMesure Mesure informations
   * @param errors Errors messages of the configuration
   * @param request HTTPRequest received
   * @return The summary of the configuration
   */
  def display_validation(status:Results.Status,module:Module,config:ConfigurationForm,infoMesure:List[(BSONObjectID,InfoMesureForm)],errors:List[String])(implicit request:Request[AnyContent]):Future[Result]={
    //Find sensors with type associat
    findListSensor(infoMesure.foldLeft(List[BSONObjectID]()){
      (list,couple)=>list :+ couple._1
    }).map(data => data match {

      //Display the summary of the configuration
      case (typeSensor,espece, typeMesure, sensor) => status(views.html.configuration.validation(module,config,typeSensor,sensor,espece,infoMesure,errors))
    })
  }

  /**
   * Find the list of sensors, type sensors and type mesure for sensors associat to the module
   * @param listSensors List of sensors associat to the module
   * @return The list of sensors, type sensors and type mesure
   */
  def findListSensor(listSensors:List[BSONObjectID]):Future[(List[TypeSensor],List[Espece],List[TypeMesure],List[Sensor])]={
    //Get the list of type sensors for sensors selected available
    val selector=Json.obj("delete"->false,"_id" -> Json.obj("$in" -> JsArray(listSensors.mapConserve(id=>BSONObjectIDFormat.writes(id)).asInstanceOf[List[JsValue]].toSeq)))
    val getTypeSensors = sensorsDao.fold(selector, Json.obj(), HashSet[BSONObjectID]())((set, sensors) => set + sensors.types).flatMap(
      types => typeSensorsDao.findAll(Json.obj("delete"->false,"_id" -> Json.obj("$in" -> JsArray(types.toList.mapConserve(p => BSONObjectIDFormat.writes(p)).asInstanceOf[List[JsValue]].toSeq))))
    )

    //Find sensors
    sensorsDao.findAll(selector).flatMap(
      sensors =>
        getTypeSensors.flatMap(
          typeSensors =>
            especeDao.findAll(Json.obj("_id"->Json.obj("$in"->typeSensors.foldRight(List[BSONObjectID]()){
              (types,list)=>types.espece:::list
            }))).flatMap(
              espece=>
                typeMesureDao.findAll(Json.obj("_id"->Json.obj("$in"->espece.foldRight(List[BSONObjectID]()){
                  (esp,list)=>esp.mesure::list
                }))).map(
                  typeMesure=>(typeSensors,espece,typeMesure,sensors)
                )
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
  def verifyModuleAndSensorFound(idModule:String,idSensor:String)(f:(Module,Sensor)=>Future[Result])(implicit request:Request[AnyContent]):Future[Result]={
    //Verify if module exists
    moduleManager.doIfModuleFound(BSONObjectID(idModule)) {

      //Find the sensor
      module=>sensorsDao.findOne(Json.obj("delete"->false,"_id"->BSONObjectID(idSensor))).flatMap(

        data=>data match{

            //If the sensor not found, redirect to the sensor list
         case None => future{Redirect(routes.ConfigurationManager.listSensors(idModule))}

            //If the sensor found, execute parameter function
         case Some(sensor)=>f(module,sensor)
       }
      )
    }{
      //Redirect to the list of modules
      _ =>future{Redirect(routes.ModuleManager.inventary())}
    }
  }

  /**
   * Find the configuration in session
   * @param request HTTPRequest received
   * @return The configuration in session or the default configuration
   */
  def getConfiguration(implicit request:Request[AnyContent]):ConfigurationForm=
  //Find the configuration in session
  request.session.get("config") match{

      //If configuration is not found, return default configuration
    case None=>ConfigurationForm(port="",types="",numberOfValue=0)

      //If configuration is found return it
    case Some(config)=>formatsForm.reads(Json.parse(config)).getOrElse(ConfigurationForm(port="",types="",numberOfValue=0))
  }

  /**
   * Find mesure informations in session
   * @param request HTTPRequest received
   * @return Mesure informations in session or an empty list
   */
  def getInfoMesure(implicit request:Request[AnyContent]):List[(BSONObjectID,InfoMesureForm)]=
  //Find mesure informations
  request.session.get("infoMesure") match{

      //If mesure informations are not found, return an empty list
    case None=>List()

      //If mesure informations are found, return their
    case Some(data)=>Json.parse(data).as[List[JsObject]].foldLeft(List[(BSONObjectID,InfoMesureForm)]()){
      (list,obj)=>list :+ (BSONObjectID((obj\"sensor").as[String]),(obj\"info").as[InfoMesureForm])
    }
  }

  /**
   * Verify if configuration is valid
   * @param config The configuration
   * @param errors List of errors
   * @return A list of errors
   */
  def verifyConfiguration(config: ConfigurationForm,errors:List[String]):List[String]={
    //If the port or the type of configuration is not defined
    if(config.port.isEmpty || config.types.isEmpty){

      //return a new list of errors
      errors :+ Messages("inventary.configuration.error.portOrTypeEmpty")
    }else{

      //Return the list of errors
      errors
    }
  }

  /**
   * Verify if mesure informations are valid
   * @param infos List of mesure informations
   * @param errors List of errors
   * @return A list of errors
   */
  def verifyInfoMesure(infos:List[(BSONObjectID,InfoMesureForm)],errors:List[String]):List[String]={
    //If the list of mesure informations is empty
    if(infos.isEmpty){

      //return a new list of errors
      errors :+ Messages("inventary.configuration.error.noSensor")
    }else{

      //return the list of errors
      errors
    }
  }
}

@Api(value = "/configuration", description = "Operations for modules configuration")
object ConfigurationManager extends ConfigurationManagerLike
