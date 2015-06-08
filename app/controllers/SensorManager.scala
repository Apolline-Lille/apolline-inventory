package controllers

import java.text.SimpleDateFormat
import java.util.Date

import com.wordnik.swagger.annotations._
import models._
import play.api.data.Form
import play.api.data.Forms._
import play.api.i18n.Messages
import play.api.libs.json.{JsObject, Json}
import play.api.mvc._
import play.modules.reactivemongo.json.BSONFormats.BSONObjectIDFormat
import reactivemongo.bson
import reactivemongo.bson.{BSONObjectID, BSONDocument}
import reactivemongo.core.commands.LastError

import scala.concurrent._
import scala.concurrent.duration.Duration

import play.api.libs.concurrent.Execution.Implicits.defaultContext

import scala.util.{Failure, Success}

/**
 * This class represent all information get when the user submit a form for insert or update a sensor
 * @param id Sensor id
 * @param acquisition Acquistion date of the sensor
 * @param expiration Expiration date of the sensor
 * @param firstUse First use date of the sensor
 * @param hs Flag indicate if the sensor is out of order
 * @param commentaire Comment for the sensor
 * @param send Value on the button used for send the form
 */
case class SensorForm(
  id:String,
  acquisition:Date,
  expiration:Option[Date],
  firstUse:Option[Date],
  hs:Boolean,
  commentaire:Option[String],
  send:String
) extends SensorInfo

/**
 * This object is a controller for manage all sensors
 */
trait SensorManagerLike extends Controller{

  /************* Property *********************/

  /**
   * DAO for sensors type
   */
  val typeSensorDao:TypeSensorDao=TypeSensorDaoObj

  /**
   * DAO for signal
   */
  val typeMesureDao:TypeMesureDao=TypeMesureDaoObj

  /**
   * DAO for sensors
   */
  val sensorDao:SensorDao=SensorDaoObj

  /**
   * Manager for sensors type
   */
  val typeSensorManager:TypeSensorManagerLike=TypeSensorManager

  /**
   * Value contains the configuration of the form
   */
  lazy val form=Form[SensorForm](
    mapping(
      "id"->nonEmptyText,
      "acquisition"->date,
      "expiration"->optional(date),
      "firstUse"->optional(date),
      "hs"->boolean,
      "commentaire"->optional(text),
      "send"->nonEmptyText
    )(SensorForm.apply)(SensorForm.unapply)
  )

  /****************** Route methods ***********/

  /**
   * This method is call when the user is on the page /inventary/sensors/:id. It list sensors available for a particular type
   * @return Return Ok Action when the user is on the page /inventary/sensors/:id with the list of sensors
   *         Return Redirect Action when the user is not log in
   *         Return Internal Server Error Action when have mongoDB error
   */
  @ApiOperation(
    nickname = "inventary",
    value = "Get the html page for list sensors",
    notes = "Get the html page for list sensors",
    httpMethod = "GET")
  @ApiResponses(Array(
    new ApiResponse(code=303,message="Move resource to the login page at /login if the user is not log"),
    new ApiResponse(code=500,message="Have a mongoDB error")
  ))
  @ApiImplicitParams(Array(
    new ApiImplicitParam(value = "Id of the sensor type for list sensors associated",required=true,name="id", dataType = "String", paramType = "path")
  ))
  def inventary(id:String,sort:String="id",sens:Int=1)=Action.async{
    implicit request =>
      //Verify if user is connect
      UserManager.doIfconnectAsync(request) {
        getInventarySensor(Json.obj("delete"->false,"types"->BSONObjectID(id)),Json.obj(sort->sens),BSONObjectID(id),Redirect(routes.TypeSensorManager.inventary())){
          (typeSensor,typeMesure,listSensor,sensorUsed)=>Ok(views.html.sensors.listSensor(typeSensor,typeMesure,listSensor,sensorUsed,sort,sens))
        }
      }
  }

  /**
   * This method is call when the user is on the page /inventary/sensors/:id/sensor. It display a form for add new sensor
   * @return Return Ok Action when the user is on the page /inventary/sensors/:id/sensor with the form for add new sensor
   *         Return Redirect Action when the user is not log in
   */
  @ApiOperation(
    nickname = "inventary/sensor/insert",
    value = "Get the html page a form for add new sensor",
    notes = "Get the html page a form for add new sensor",
    httpMethod = "GET")
  @ApiResponses(Array(
    new ApiResponse(code=303,message="Move resource to the login page at /login if the user is not log")
  ))
  @ApiImplicitParams(Array(
    new ApiImplicitParam(value = "Id of the sensor type for list sensors associated",required=true,name="id", dataType = "String", paramType = "path")
  ))
  def sensorPage(id:String)=Action.async{
    implicit request =>
    //Verify if user is connect
    UserManager.doIfconnectAsync(request) {
      //Verify if sensor type found
      typeSensorManager.doIfTypeSensorFound(BSONObjectID(id)) {_=>
        //Print an empty form for add new sensor
        future{Ok(views.html.sensors.formSensor(form, id, routes.SensorManager.sensorInsert(id)))}
      }{_=>
        //Print an empty form with error type not found
        future{BadRequest(views.html.sensors.formSensor(form.withGlobalError(Messages("inventary.typeSensor.error.typeNotExist")), id, routes.SensorManager.sensorInsert(id)))}
      }
    }
  }

  /**
   * This method is call when the user is on the page /inventary/sensors/:id/:id2. It display a form for update a sensor
   * @return Return Ok Action when the user is on the page /inventary/sensors/:id/:id2 with the form for update a sensor
   *         Return Redirect Action when the user is not log in or if sensor not found
   *         Return Internal Server Error Action when have mongoDB error
   */
  @ApiOperation(
    nickname = "inventary/sensor/update",
    value = "Get the html page a form for update a sensor",
    notes = "Get the html page a form for update a sensor",
    httpMethod = "GET")
  @ApiResponses(Array(
    new ApiResponse(code=303,message="<ul><li>Move resource to the login page at /login if the user is not log</li><li>Move resource to the sensors inventary at /inventary/sensors/:id if sensor not found</li></ul>"),
    new ApiResponse(code=500,message="Have a mongoDB error")
  ))
  @ApiImplicitParams(Array(
    new ApiImplicitParam(value = "Id of the sensor type",required=true,name="id", dataType = "String", paramType = "path"),
    new ApiImplicitParam(value = "Id of the sensor",required=true,name="id2", dataType = "String", paramType = "path")
  ))
  def sensorUpdatePage(id:String,id2:String)=Action.async{
    implicit request =>
      //If user is connect print a form with prefilled data
      printFormWithData(id,id2,routes.SensorManager.sensorUpdate(id,id2)){
      sensor=>
        //Data prefilled into the form
        SensorForm(
          sensor.id,
          sensor.acquisition,
          sensor.expiration,
          sensor.firstUse,
          sensor.hs,
          sensor.commentaire,
          ""
        )
    }
  }

  /**
   * This method is call when the user is on the page /inventary/sensors/:id/:id2/clone. It display a prefilled form for insert a new sensor
   * @return Return Ok Action when the user is on the page /inventary/sensors/:id/:id2/clone with the prefilled form for insert a new sensor
   *         Return Redirect Action when the user is not log in or if sensor not found
   *         Return Internal Server Error Action when have mongoDB error
   */
  @ApiOperation(
    nickname = "inventary/sensor/clone",
    value = "Get the html page a prefilled form for insert a new sensor",
    notes = "Get the html page a prefilled form for insert a new sensor",
    httpMethod = "GET")
  @ApiResponses(Array(
    new ApiResponse(code=303,message="<ul><li>Move resource to the login page at /login if the user is not log</li><li>Move resource to the sensors inventary at /inventary/sensors/:id if sensor not found</li></ul>"),
    new ApiResponse(code=500,message="Have a mongoDB error")
  ))
  @ApiImplicitParams(Array(
    new ApiImplicitParam(value = "Id of the sensor type",required=true,name="id", dataType = "String", paramType = "path"),
    new ApiImplicitParam(value = "Id of the sensor",required=true,name="id2", dataType = "String", paramType = "path")
  ))
  def sensorClonePage(id:String,id2:String)=Action.async{
    implicit request =>
      //If user is connect print a form with prefilled data
      printFormWithData(id,id2,routes.SensorManager.sensorInsert(id)){
      sensor=>
        //Data prefilled into the form
        SensorForm(
          "",
          sensor.acquisition,
          sensor.expiration,
          sensor.firstUse,
          sensor.hs,
          sensor.commentaire,
          ""
        )
    }
  }

  /**
   * This method is call when the user submit a form for insert new sensor
   * @return Return Ok Action when the user sensor was insert and return prefilled form for insert a new sensor
   *         Return Redirect Action when the user is not log in or if sensor was insert
   *         Return Bad request Action if the form was submit with data error
   *         Return Internal Server Error Action when have mongoDB error
   */
  @ApiOperation(
    nickname = "inventary/sensor/insert",
    value = "Insert a new sensor",
    notes = "Insert a new sensor to the mongoDB database",
    httpMethod = "POST")
  @ApiResponses(Array(
    new ApiResponse(code=303,message="<ul><li>Move resource to the login page at /login if the user is not log</li><li>Move resource to the sensors inventary at /inventary/sensors/:id if sensor was insert</li></ul>"),
    new ApiResponse(code=400,message="Fields required or not valid"),
    new ApiResponse(code=500,message="Have a mongoDB error")
  ))
  @ApiImplicitParams(Array(
    new ApiImplicitParam(value = "Id of the sensor type",required=true,name="id", dataType = "String", paramType = "path"),
    new ApiImplicitParam(value = "Sensor id",required=true,name="id", dataType = "String", paramType = "form"),
    new ApiImplicitParam(value = "Acquisition date of the sensor",required=true,name="acquisition", dataType = "Date", paramType = "form"),
    new ApiImplicitParam(value = "Expiration date of the sensor",name="expiration", dataType = "Date", paramType = "form"),
    new ApiImplicitParam(value = "First use date of the sensor",name="firstUse", dataType = "Date", paramType = "form"),
    new ApiImplicitParam(value = "Flag indicate if the sensor is out of order",name="hs",dataType = "Boolean", paramType = "form"),
    new ApiImplicitParam(value = "Comment for the sensor",required=true,name="commentaire",dataType = "String", paramType = "form"),
    new ApiImplicitParam(value = "Value on the button used for send the form",required=true,name="send",defaultValue="Envoyer et continuer",dataType="String",paramType="form")
  ))
  def sensorInsert(id:String)=Action.async {
    implicit request =>
      val msg=Messages("inventary.sensor.error.sensorExist")+" <input type=\"submit\" class=\"btn btn-danger\" name=\"send\" value=\""+Messages("global.reactiver")+"\"/> <input type=\"submit\" class=\"btn btn-danger\" name=\"send\" value=\""+Messages("global.ignorer")+"\"/>"

      //Verify if the user is connect and if data received are valid
      submitForm(msg,id,routes.SensorManager.sensorInsert(id)){

        //Filter for verify if sensor exists
        sensorData=>Json.obj("id" -> sensorData.id, "types" -> BSONObjectID(id))

      }{
        sensorData=>
          if(!sensorData.send.equals("Réactiver")) {
            insertSensor(id,sensorData)
          }else{
            sensorDao.findOne(Json.obj("id" -> sensorData.id, "types" -> BSONObjectID(id))).flatMap(
              sensorOpt=>sensorOpt match{
                case Some(sensor)=>update(id,sensor._id.stringify,sensor,false)
                case _=>future{Redirect(routes.SensorManager.inventary(id))}
              }
            ).recover({case _=>InternalServerError("error")})
          }
      }
  }

  /**
   * This method is call when the user submit a form for update a sensor
   * @return Return Redirect Action when the user is not log in or if sensor was update
   *         Return Bad request Action if the form was submit with data error
   *         Return Internal Server Error Action when have mongoDB error
   */
  @ApiOperation(
    nickname = "inventary/sensor/update",
    value = "Update a sensor",
    notes = "Update a sensor to the mongoDB database",
    httpMethod = "POST")
  @ApiResponses(Array(
    new ApiResponse(code=303,message="<ul><li>Move resource to the login page at /login if the user is not log</li><li>Move resource to the sensors inventary at /inventary/sensors/:id if sensor was update</li></ul>"),
    new ApiResponse(code=400,message="Fields required or not valid"),
    new ApiResponse(code=500,message="Have a mongoDB error")
  ))
  @ApiImplicitParams(Array(
    new ApiImplicitParam(value = "Id of the sensor type",required=true,name="id", dataType = "String", paramType = "path"),
    new ApiImplicitParam(value = "Id of the sensor",required=true,name="id2", dataType = "String", paramType = "path"),
    new ApiImplicitParam(value = "Sensor id",required=true,name="id", dataType = "String", paramType = "form"),
    new ApiImplicitParam(value = "Acquisition date of the sensor",required=true,name="acquisition", dataType = "Date", paramType = "form"),
    new ApiImplicitParam(value = "Expiration date of the sensor",name="expiration", dataType = "Date", paramType = "form"),
    new ApiImplicitParam(value = "First use date of the sensor",name="firstUse", dataType = "Date", paramType = "form"),
    new ApiImplicitParam(value = "Flag indicate if the sensor is out of order",name="hs",dataType = "Boolean", paramType = "form"),
    new ApiImplicitParam(value = "Comment for the sensor",required=true,name="commentaire",dataType = "String", paramType = "form"),
    new ApiImplicitParam(value = "Value on the button used for send the form",required=true,name="send",defaultValue="Envoyer",dataType="String",paramType="form")
  ))
  def sensorUpdate(idType:String,id:String)=Action.async{
    implicit request =>
      val msg=Messages("inventary.sensorSensor.error.sensorExist")+" <input type=\"submit\" class=\"btn btn-danger\" value=\""+Messages("global.ignorer")+"\"/>"

      //Verify if the user is connect and if data received are valid
      submitForm(msg,idType,routes.SensorManager.sensorUpdate(idType,id)){

        //Filter for verify if sensor exists
        sensorData=>Json.obj("_id"->Json.obj("$ne"->BSONObjectID(id)),"id" -> sensorData.id, "types" -> BSONObjectID(idType))

      }{
        //Update the sensor
        sensorData=>update(idType,id,sensorData,false)
      }
  }

  /**
   * This method is call when the user delete a sensor
   * @param idType Sensor type id
   * @param id Sensor id
   * @return Return Redirect Action when the user is not log in or sensor is delete
   *         Return Internal Server Error Action when have mongoDB error
   */
  @ApiOperation(
    nickname = "inventary/sensors/:id/:id2/delete",
    value = "Delete a sensor",
    notes = "Delete a sensor to the mongoDB database",
    httpMethod = "GET")
  @ApiResponses(Array(
    new ApiResponse(code=303,message="<ul><li>Move resource to the login page at /login if the user is not log</li><li>Move resource to the sensor inventary page at /inventary/sensors/:id when sensor is delete"),
    new ApiResponse(code=500,message="Have a mongoDB error")
  ))
  @ApiImplicitParams(Array(
    new ApiImplicitParam(value = "Id of the sensor type",required=true,name="id", dataType = "String", paramType = "path"),
    new ApiImplicitParam(value = "Id of the sensor",required=true,name="id2", dataType = "String", paramType = "path")
  ))
  def delete(idType:String,id:String)=Action.async {
    implicit request=>

      //Verify if user is connect
      UserManager.doIfconnectAsync(request) {
        //Verify if sensor type found
        typeSensorManager.doIfTypeSensorFound(BSONObjectID(idType)) { _ =>
          //Find the sensor
          sensorDao.findOne(Json.obj("_id" -> BSONObjectID(id))).flatMap(

            data => data match {

              //If sensor not found redirect to the sensor inventary
              case None => future {
                Redirect(routes.SensorManager.inventary(idType))
              }

              //If sensor found
              case Some(sensorData) => {
                //Update the sensor and set the delete column to true
                update(idType, id, sensorData, true)
              }
            }).recover({
            //Send Internal Server Error if have mongoDB error
            case e => InternalServerError("error")
          })
        }{
          _=> future{Redirect(routes.TypeSensorManager.inventary())}
        }
      }
  }

  /****************  Methods  ***********************/

  /**
   * List sensors get depending on the query
   * @param selector Query for get sensors
   * @param sort List of column and her direction used for sort sensors
   * @param r Page display when type sensors or typeMesure not found
   * @param f Function for print the list of cards
   * @return
   */
  def getInventarySensor(selector:JsObject,sort:JsObject,id:BSONObjectID,r:Result)(f:(TypeSensor,TypeMesure,List[Sensor],List[(BSONObjectID,Int)])=>Result)={
    //Find sensors
    val future_sensors=sensorDao.findAll(selector,sort)

    //Find type sensors
    typeSensorDao.findById(id).flatMap(typeSensorOpt=>
      typeSensorOpt match {

        //If the sensor type not found, Redirect to the inventary
        case None => future{r}

        //If the sensor type found, applied function for get signal
        case Some(typeSensor) =>
          //Find type mesure
          typeMesureDao.findById(typeSensor.mesure).flatMap(typeMesureOpt=>
            typeMesureOpt match {

              //If signal not found, redirect to the inventary
              case None => future{r}

              //If signal found, apply function for get list of sensors
              case Some(typeMesure) =>
                future_sensors.flatMap(listSensor=>
                  sensorDao.countUsedSensors(List(typeSensor)).map(
                    countSensorUsed=>
                      //Print the list of sensors
                      f(typeSensor,typeMesure,listSensor,countSensorUsed)
                  )
                ).recover({
                  //Send Internal Server Error if have mongoDB error
                  case e => InternalServerError("error")
                })
            }
          ).recover({
            //Send Internal Server Error if have mongoDB error
            case e => InternalServerError("error")
          })
      }
    ).recover({
      //Send Internal Server Error if have mongoDB error
      case e => InternalServerError("error")
    })
  }

  /**
   * Print a form with prefilled data
   * @param id Sensor type id
   * @param id2 Sensor id
   * @param r Route call when user submit the form
   * @param f Function for get prefilled information
   * @param request
   * @return Return OK page with the prefilled form
   *         Return Redirect to the sensor inventary if sensor not found or to the login page if user is not connect
   *         Return Internal Server Error if have mongoDB error
   */
  def printFormWithData(id:String,id2:String,r:Call)(f:Sensor=>SensorForm)(implicit request:Request[AnyContent]): Future[Result] ={
    //Verify if user is connect
    UserManager.doIfconnectAsync(request) {
      //Verify if sensor type found
      typeSensorManager.doIfTypeSensorFound(BSONObjectID(id)) {_=>
        //Find the sensor
        sensorDao.findById(BSONObjectID(id2)).map(
          sensorOpt => sensorOpt match {

            //If the sensor not found redirect to the sensor inventary
            case None => Redirect(routes.SensorManager.inventary(id))

            //If the sensor found
            case Some(sensor) => {
              //print the prefilled form with sensor information
              val sensorData = f(sensor)
              Ok(views.html.sensors.formSensor(form.fill(sensorData), id, r))
            }

          }
        ).recover({
          //Send Internal Server Error if have mongoDB error
          case _ => InternalServerError("error")
        })
      }{_=>
        future{BadRequest(views.html.sensors.formSensor(form.withGlobalError(Messages("inventary.typeSensor.error.typeNotExist")), id, r))}
      }
    }
  }

  /**
   * Verify if the user is connect and if data received are valid then apply function dedicated
   * @param id Sensor type id
   * @param routeSubmit Route use for submit the form
   * @param verif Function use for get sensor selector
   * @param f Function dedicated
   * @param request
   * @return Return Bad request Action if the form is not valid
   *         Return Redirect if dedicated function is a success
   *         Return Internal server error if have mongoDB error
   */
  def submitForm(errorMessage:String,id:String,routeSubmit:Call)(verif:SensorForm=>JsObject)(f:SensorForm=>Future[Result])(implicit request: Request[AnyContent]):Future[Result]={
    //Verify if user is connect
    UserManager.doIfconnectAsync(request) {
      //Verify if sensor type found
      typeSensorManager.doIfTypeSensorFound(BSONObjectID(id)) {_=>
        form.bindFromRequest.fold(

          //If form contains errors
          formWithErrors => {
            //the form is redisplay with error descriptions
            future {
              BadRequest(views.html.sensors.formSensor(formWithErrors, id, routeSubmit))
            }
          },

          // Else if form no contains errors
          sensorData => {

            //Verify if dates are consistent
            val formWithVerif = verifyErrorAcquisitionAfterFirstUse(sensorData, form)

            //If dates ares consistent
            if (formWithVerif.equals(form)) {
              actionWhenFormValid(errorMessage,id,routeSubmit,sensorData,verif,f)

            } else {
              //If dates aren't consistent, return bad request with prefilled form
              future {
                BadRequest(views.html.sensors.formSensor(formWithVerif.fill(sensorData), id, routeSubmit))
              }
            }
          }
        )
      } {_=>
        future {
          BadRequest(views.html.sensors.formSensor(form.withGlobalError(Messages("inventary.typeSensor.error.typeNotExist")), id, routeSubmit))
        }
      }
    }
  }

  /**
   * This method insert a sensor into the databse
   * @param id Sensor type id
   * @param sensorData Data received from the form
   * @param request
   * @return
   */
  def insertSensor(id:String,sensorData:SensorForm)(implicit request: Request[AnyContent])={
    //Insert the sensor into the mongoDB database
    sensorDao.insert(
      Sensor(
        id = sensorData.id,
        types = BSONObjectID(id),
        acquisition = sensorData.acquisition,
        expiration = sensorData.expiration,
        firstUse = sensorData.firstUse,
        hs = sensorData.hs,
        commentaire = sensorData.commentaire
      )
    ).map(e =>
      //When the sensor was insert
      sensorData.send match {

        //If use click on the button "Envoyer et continuer"
        case "Envoyer et continuer" => {
          //Prepare prefilled data
          val sensorForm = SensorForm(
            "",
            sensorData.acquisition,
            sensorData.expiration,
            sensorData.firstUse,
            sensorData.hs,
            sensorData.commentaire,
            ""
          )
          //Print the form with prefilled data
          Ok(views.html.sensors.formSensor(form.fill(sensorForm), id, routes.SensorManager.sensorInsert(id)))
        }

        //If user click on an other button redirect her to the sensor inventary for the current sensor type
        case _ => Redirect(routes.SensorManager.inventary(id))
      }
      ).recover({
      //Send Internal Server Error if have mongoDB error
      case e => InternalServerError("error")
    })
  }

  /**
   * This method verify if the sensor exists before insert/update/reactivat the sensor
   * @param errorMessage Error message print if the sensor exist
   * @param id Sensor type id
   * @param r Route used when submit a form
   * @param sensorData Data received from the form
   * @param verif A method return a JSON Object for get sensor to verify if the sensor exist
   * @param f A method for insert/update/reactivat the sensor
   * @param request
   * @return
   */
  def actionWhenFormValid(errorMessage:String,id:String,r:Call,sensorData:SensorForm,verif:SensorForm=>JsObject,f:SensorForm=>Future[Result])(implicit request:Request[AnyContent])={
    //Find the sensor
    sensorDao.findAll(verif(sensorData)).flatMap(
      sensor => {

        //If sensor not found, execute dedicated function
        if(sensor.size==0 || List("Réactiver","Ignorer").contains(sensorData.send)) {
          f(sensorData)
        }else if(sensor.filter(p => !(p.delete) ).size>0){
          future{BadRequest(views.html.sensors.formSensor(form.withGlobalError(Messages("inventary.sensor.error.sensorExist")).fill(sensorData),id, r))}
        }
        else{
          future{BadRequest(views.html.sensors.formSensor(form.withGlobalError(errorMessage).fill(sensorData),id, r))}
        }
      }
    ).recover({
      //Send Internal Server Error if have mongoDB error
      case e => InternalServerError("error")
    })
  }

  /**
   * Verify if acquisition date is before first use date
   * @param sensorData Data received from the form
   * @param form The current form
   * @return Return the form after verification
   */
  def verifyErrorAcquisitionAfterFirstUse(sensorData:SensorForm,form:Form[SensorForm]):Form[SensorForm]={
    //Verify if acquistion date is after expiration date
    val nform=verifyErrorAcquisitionAfterExpiration(sensorData,form)

    //If first use date is defined and acquisition date is after
    if(sensorData.firstUse.nonEmpty && sensorData.acquisition.after(sensorData.firstUse.get)){
      //Return form with an error
      nform.withError("firstUse",Messages("inventary.sensor.error.firstUseBeforeAcquisition"))
    }else{
      nform
    }
  }

  /**
   * Verify if acquisition date is before expiration date
   * @param sensorData Data received from the form
   * @param form The current form
   * @return Return the form after verification
   */
  def verifyErrorAcquisitionAfterExpiration(sensorData:SensorForm,form:Form[SensorForm]):Form[SensorForm]={

    //If expiration date is defined and acquisition date is after
    if(sensorData.expiration.nonEmpty && sensorData.acquisition.after(sensorData.expiration.get)){
      //Return form with an error
      form.withError("expiration",Messages("inventary.sensor.error.expirationBeforeAcquisition"))
    }else{
      form
    }
  }

  /**
   * Update a sensor
   * @param idType Sensor type Id
   * @param id Sensor id
   * @param sensorData Sensor information received from the formf4C8gP2q
   * @param delete Flag for says if sensors was delete
   * @return
   */
  def update(idType:String,id:String,sensorData:SensorInfo,delete:Boolean): Future[Result] ={
    //Update the sensor
    sensorDao.updateById(
      BSONObjectID(id),

      //Create sensor information
      Sensor(
        _id=BSONObjectID(id),
        id = sensorData.id,
        types = BSONObjectID(idType),
        acquisition = sensorData.acquisition,
        expiration = sensorData.expiration,
        firstUse = sensorData.firstUse,
        hs = sensorData.hs,
        commentaire = sensorData.commentaire,
        delete=delete
      )
    ).map(e=>
      //If sensor was update, redirect to the sensor inventary
      Redirect(routes.SensorManager.inventary(idType))
    ).recover({
      //Send Internal Server Error if have mongoDB error
      case e => InternalServerError("error")
    })
  }

  def format(d:Date):String={
    val format=new SimpleDateFormat("dd/MM/YYYY")
    format.format(d)
  }
}

@Api(value = "/sensors", description = "Operations for sensors")
object SensorManager extends SensorManagerLike{
  this:SensorManagerLike =>
}
