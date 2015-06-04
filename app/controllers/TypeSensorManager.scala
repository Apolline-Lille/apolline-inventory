package controllers

import com.wordnik.swagger.annotations._
import models._
import play.api.i18n.Messages
import play.api.libs.json.{JsObject, Json}
import play.api.mvc._
import play.api.data._
import play.api.data.Forms._
import play.modules.reactivemongo.json.BSONFormats
import reactivemongo.bson.{BSONObjectID, BSONDocument}

import scala.concurrent._
import scala.concurrent.duration.Duration

import play.api.libs.concurrent.Execution.Implicits.defaultContext


/**
 * This class represent all information get when the user submit a form for insert or update a sensor type
 * @param model Name of the sensor model
 * @param types Name of the sensor type
 * @param espece List of specie analyze by the sensor
 * @param mesure Name of the signal received by the sensor
 * @param nbSignaux Number of signal received by the sensor
 * @param fabricant Builder of the sensor
 * @param unite Unity for the signal
 */
case class TypeSensorForm(
   model:String,
   types:String,
   espece:List[String],
   mesure:String,
   nbSignaux:Int,
   fabricant:String,
   unite:String,
   send:Option[String]=None
 )

/**
 * This trait is a controller for manage sensors type
 */
trait TypeSensorManagerLike extends Controller{

  /************* Property *********************/

  /**
   * Value contains the configuration of the form
   */
  lazy val form=Form[TypeSensorForm](
    mapping(
      "model"->nonEmptyText,
      "types"->nonEmptyText,
      "espece"->list(text),
      "mesure"->nonEmptyText,
      "nbSignaux"->number(min=1),
      "fabricant"->nonEmptyText,
      "unite"->nonEmptyText,
      "send"->optional(text)
    )(TypeSensorForm.apply)(TypeSensorForm.unapply)
  )

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

  /****************** Route methods ***********/

  /**
   * This method is call when the user is on the page /inventary/sensors. It list sensors type available
   * @return Return Ok Action when the user is on the page /inventary/sensors with the list of sensors type
   *         Return Redirect Action when the user is not log in
   *         Return Internal Server Error Action when have mongoDB error
   */
  @ApiOperation(
    nickname = "inventary",
    value = "Get the html page for list sensors type",
    notes = "Get the html page for list sensors type",
    httpMethod = "GET")
  @ApiResponses(Array(
    new ApiResponse(code=303,message="Move resource to the login page at /login if the user is not log"),
    new ApiResponse(code=500,message="Have a mongoDB error")
  ))
  @ApiImplicitParams(Array(
    new ApiImplicitParam(value = "Sensor type name for filter all sensors type",name="sort", dataType = "String", paramType = "query")
  ))
  def inventary(sort:String="",filtreSto:String="")=Action.async{
    implicit request =>
      //Verify if user is connect
      UserManager.doIfconnectAsync(request) {
        getInventaryTypeSensor(Json.obj("delete"->false),sort){
          (typeSensor,typeMesure,stock,nomType)=>
          //Display the HTML page
          Ok(views.html.sensors.listTypeSensor(filtreSto,sort,filtreStock(filtreSto),typeSensor,typeMesure,stock,nomType))
        }
      }
  }

  /**
   * This method is call when the user is on the page /inventary/sensors/type. It display a form for add new sensor type
   * @return Return Ok Action when the user is on the page /inventary/sensors/type with the form
   *         Return Redirect Action when the user is not log in
   */
  @ApiOperation(
    nickname = "inventary/sensors",
    value = "Get the html page for insert a new sensor type",
    notes = "Get the html page for insert a new sensor type",
    httpMethod = "GET")
  @ApiResponses(Array(
    new ApiResponse(code=303,message="Move resource to the login page at /login if the user is not log")
  ))
  def typePage=Action.async{
    implicit request =>
      //Verify if user is connect
      UserManager.doIfconnectAsync(request) {

        //Display the form for insert new sensor type
        printForm(Results.Ok,form,routes.TypeSensorManager.typeInsert())
      }
  }

  /**
   * This method is call when the user is on the page /inventary/sensors/:id/update. It display a form for update sensor type
   * @param id Sensor type id
   * @return Return Ok Action when the user is on the page /inventary/sensors/:id/update with the form
   *         Return Redirect Action when the user is not log in or sensor type information not found
   *         Return Internal Server Error Action when have mongoDB error
   */
  @ApiOperation(
    nickname = "inventary/sensors/:id/update",
    value = "Get the html page for update sensor type",
    notes = "Get the html page for update sensor type",
    httpMethod = "GET")
  @ApiResponses(Array(
    new ApiResponse(code=303,message="<ul><li>Move resource to the login page at /login if the user is not log</li><li>Move resource to the sensor inventary page at /inventary/sensors when sensor type not found"),
    new ApiResponse(code=500,message="Have a mongoDB error")
  ))
  @ApiImplicitParams(Array(
    new ApiImplicitParam(value = "Id of the sensor type",required=true,name="id", dataType = "String", paramType = "path")
  ))
  def typeUpdatePage(id:String)=Action.async{
    implicit request =>
      //Verify if user is connect
      UserManager.doIfconnectAsync(request) {

        //Find the sensor type
        typeSensorDao.findById(BSONObjectID(id)).flatMap(
          typeSensorOpt=> typeSensorOpt match{

            //Sensor type not found redirect to the sensor inventary
            case None=>future{Redirect(routes.TypeSensorManager.inventary())}

            //Sensor type found
            case Some(typeSensor)=>{

              //Find signal associated to the sensor type
              typeMesureDao.findById(typeSensor.mesure).flatMap(
              typeMesureOpt=>typeMesureOpt match {

                //Signal not found redirect to the sensor inventary
                case None => future{Redirect(routes.TypeSensorManager.inventary())}

                //Signal found
                case Some(typeMesure) => {

                  //Prepare data for prefilled the form
                  val typeSensorData = TypeSensorForm(
                    typeSensor.modele,
                    typeSensor.nomType,
                    typeSensor.espece,
                    typeMesure.nom,
                    typeSensor.nbSignaux,
                    typeSensor.fabricant,
                    typeMesure.unite
                  )

                  //Display the form for update sensor type
                  printForm(Results.Ok,form.fill(typeSensorData),routes.TypeSensorManager.typeUpdate(id))
                }
              }
              ).recover({
                //Send an Internal Server Error for mongoDB error
                case e=>InternalServerError("error")
              })
            }
          }
        ).recover({
          //Send an Internal Server Error for mongoDB error
          case e=>InternalServerError("error")
        })
      }
  }

  /**
   * This method is call when the user submit a form for insert a new sensor type
   * @return Return Bad Request Action if the form was submit with data error
   *         Return Redirect Action when the user is not log in or sensor type is insert
   *         Return Internal Server Error Action when have mongoDB error
   */
  @ApiOperation(
    nickname = "inventary/sensor/type",
    value = "Insert a new sensor type",
    notes = "Insert a new sensor type to the mongoDB database",
    httpMethod = "POST")
  @ApiResponses(Array(
    new ApiResponse(code=303,message="<ul><li>Move resource to the login page at /login if the user is not log</li><li>Move resource to the sensor inventary page at /inventary/sensors when sensor type is insert"),
    new ApiResponse(code=400,message="Fields required or not valid"),
    new ApiResponse(code=500,message="Have a mongoDB error")
  ))
  @ApiImplicitParams(Array(
    new ApiImplicitParam (value = "Name of the sensor model",required=true,name="model", dataType = "String", paramType = "form"),
    new ApiImplicitParam (value = "Name of the sensor type",required=true,name="types", dataType = "String", paramType = "form"),
    new ApiImplicitParam (value = "Builder of the sensor",required=true,name="fabricant", dataType = "String", paramType = "form"),
    new ApiImplicitParam (value = "List of specie analyze by the sensor",required=true,allowMultiple=true,name="especes", dataType = "String", paramType = "form"),
    new ApiImplicitParam (value = "Number of signal received by the sensor",required=true,name="nbSignaux", dataType = "Integer", paramType = "form"),
    new ApiImplicitParam (value = "Name of the signal received by the sensor",required=true,name="mesure", dataType = "String", paramType = "form"),
    new ApiImplicitParam (value = "Unity for the signal",required=true,name="unite", dataType = "String", paramType = "form")
  ))
  def typeInsert=Action.async{
    implicit request=>
      val msg=Messages("inventary.typeSensor.error.typeExist")+" <input type=\"submit\" class=\"btn btn-danger\" name=\"send\" value=\""+Messages("global.reactiver")+"\"/> <input type=\"submit\" class=\"btn btn-danger\" name=\"send\" value=\""+Messages("global.ignorer")+"\"/>"
      //Verify if the user is connect and if data received are valid
      submitForm(msg,routes.TypeSensorManager.typeInsert()) {
        typeData => Json.obj("modele" -> typeData.model, "fabricant" -> typeData.fabricant)
      }{(typeData,especes,mesure)=>{
        if(typeData.send.isEmpty || typeData.send.equals(Some("Ignorer"))){
          //Insert sensor type
          typeSensorDao.insert(TypeSensor(
            nomType=typeData.types,
            modele=typeData.model,
            mesure=mesure._id,
            fabricant=typeData.fabricant,
            nbSignaux=typeData.nbSignaux,
            espece=especes
          )).map(
              //Redirect to the inventary if sensor type was insert
              e => Redirect(routes.TypeSensorManager.inventary())
            ).recover({
            //Send Internal Server Error if have mongoDB error
            case e => InternalServerError("error")
          })
        }
        else{
          updateWithDeleteColumn(Json.obj("modele" -> typeData.model, "fabricant" -> typeData.fabricant),false)
        }
      }
      }
  }

  /**
   * This method is call when the user submit a form for update a sensor type
   * @param id Sensor type id
   * @return Return Bad Request Action if the form was submit with data error
   *         Return Redirect Action when the user is not log in or sensor type is update
   *         Return Internal Server Error Action when have mongoDB error
   */
  @ApiOperation(
    nickname = "inventary/sensors/:id/update",
    value = "Update a sensor type",
    notes = "Update a sensor type to the mongoDB database",
    httpMethod = "POST")
  @ApiResponses(Array(
    new ApiResponse(code=303,message="<ul><li>Move resource to the login page at /login if the user is not log</li><li>Move resource to the sensor inventary page at /inventary/sensors when sensor type is update"),
    new ApiResponse(code=400,message="Fields required or not valid"),
    new ApiResponse(code=500,message="Have a mongoDB error")
  ))
  @ApiImplicitParams(Array(
    new ApiImplicitParam(value = "Id of the sensor type",required=true,name="id", dataType = "String", paramType = "path"),
    new ApiImplicitParam (value = "Name of the sensor model",required=true,name="model", dataType = "String", paramType = "form"),
    new ApiImplicitParam (value = "Name of the sensor type",required=true,name="types", dataType = "String", paramType = "form"),
    new ApiImplicitParam (value = "Builder of the sensor",required=true,name="fabricant", dataType = "String", paramType = "form"),
    new ApiImplicitParam (value = "List of specie analyze by the sensor",required=true,allowMultiple=true,name="especes", dataType = "String", paramType = "form"),
    new ApiImplicitParam (value = "Number of signal received by the sensor",required=true,name="nbSignaux", dataType = "Integer", paramType = "form"),
    new ApiImplicitParam (value = "Name of the signal received by the sensor",required=true,name="mesure", dataType = "String", paramType = "form"),
    new ApiImplicitParam (value = "Unity for the signal",required=true,name="unite", dataType = "String", paramType = "form")
  ))
  def typeUpdate(id:String)=Action.async{
    implicit request=>
      val msg=Messages("inventary.typeSensor.error.typeExist")+" <input type=\"submit\" class=\"btn btn-danger\" value=\""+Messages("global.ignorer")+"\"/>"

      //Verify if the user is connect and if data received are valid
      submitForm(msg,routes.TypeSensorManager.typeUpdate(id)){
        typeData => Json.obj("_id"->Json.obj("$ne"->BSONFormats.BSONObjectIDFormat.writes(BSONObjectID(id))),"modele" -> typeData.model, "fabricant" -> typeData.fabricant)
      }{(typeData,especes,mesure)=>{
        if(typeData.send.isEmpty || typeData.send.equals(Some("Ignorer"))) {
          //Update sensor type
          typeSensorDao.updateById(BSONObjectID(id),
            TypeSensor(
              _id = BSONObjectID(id),
              nomType = typeData.types,
              modele = typeData.model,
              mesure = mesure._id,
              fabricant = typeData.fabricant,
              nbSignaux = typeData.nbSignaux,
              espece = especes
            )).map(
              //Redirect to the inventary if sensor type was update
              e => Redirect(routes.TypeSensorManager.inventary())
            ).recover({
            //Send Internal Server Error if have mongoDB error
            case e => InternalServerError("error")
          })
        }else{
          printForm(Results.BadRequest, form.withGlobalError(msg).fill(typeData), routes.TypeSensorManager.typeUpdate(id))
        }
      }
      }
  }

  /**
   * This method is call when the user delete a sensor type
   * @param id Sensor type id
   * @return Return Redirect Action when the user is not log in or sensor type is delete
   *         Return Internal Server Error Action when have mongoDB error
   */
  @ApiOperation(
    nickname = "inventary/sensors/:id/delete",
    value = "Delete a sensor type",
    notes = "Delete a sensor type to the mongoDB database",
    httpMethod = "GET")
  @ApiResponses(Array(
    new ApiResponse(code=303,message="<ul><li>Move resource to the login page at /login if the user is not log</li><li>Move resource to the sensor inventary page at /inventary/sensors when sensor type is delete"),
    new ApiResponse(code=500,message="Have a mongoDB error")
  ))
  @ApiImplicitParams(Array(
    new ApiImplicitParam(value = "Id of the sensor type",required=true,name="id", dataType = "String", paramType = "path")
  ))
  def delete(id:String)=Action.async{
    implicit request=>
      //Verify if user is connect
      UserManager.doIfconnectAsync(request) {

        val idFormat=BSONFormats.BSONObjectIDFormat.writes(BSONObjectID(id))
        //find the sensor
        sensorDao.findOne(Json.obj("delete"->false,"types"->idFormat)).flatMap(
          data => data match{

            case None => updateWithDeleteColumn(Json.obj("_id"->idFormat),true)

            //Sensor found redirect to the sensors inventary
            case _ =>future{Redirect(routes.TypeSensorManager.inventary())}

          }
        ).recover({
          //Send Internal Server Error if have mongoDB error
          case e => InternalServerError("error")
        })
      }
  }

  /****************  Methods  ***********************/

  /**
   * List type sensors get depending on the query
   * @param selector Query for get type sensors
   * @param filtreType Name of a particular type found
   * @param f Function for print the list of type sensors
   * @return
   */
  def getInventaryTypeSensor(selector:JsObject,filtreType:String)(f:(List[TypeSensor],List[TypeMesure],List[BSONDocument],List[BSONDocument])=>Result)={
    val selectorAll=if(filtreType.isEmpty){selector}else{selector ++ Json.obj("nomType"->filtreType)}

    //Find sensors quantity for all type
    val future_stock=sensorDao.countByType()
    //Find all signal
    val future_mesure=typeMesureDao.findAll()
    //Find all sensors type name for the filter
    val future_nomType=typeSensorDao.findAllType(BSONFormats.toBSON(selector).get.asInstanceOf[BSONDocument])

    //Get value defined on future
    typeSensorDao.findAll(selectorAll).flatMap(typeSensor=>
      future_mesure.flatMap(typeMesure=>
        future_stock.flatMap(stock=>
          future_nomType.map(nomType=>

            //Print the list of sensors type
            f(typeSensor,typeMesure,stock.toList,nomType.toList)

          ).recover({case e=>InternalServerError("error")})
        ).recover({case e=>InternalServerError("error")})
      ).recover({case e=>InternalServerError("error")})
    ).recover({case e=>InternalServerError("error")})
  }

  /**
   * Verify if the user is connect and if data received are valid then apply function dedicated
   * @param r Route use for submit the form
   * @param f Function dedicated
   * @param request
   * @return Return Bad request Action if the form is not valid
   *         Return Redirect if dedicated function is a success
   *         Return Internal server error if have mongoDB error
   */
  def submitForm(errorMessage:String,r:Call)(verif:TypeSensorForm=>JsObject)(f:(TypeSensorForm,List[String],TypeMesure)=>Future[Result])(implicit request: Request[AnyContent]):Future[Result]={
    //Verify if user is connect
    UserManager.doIfconnectAsync(request) {
      form.bindFromRequest.fold(

        //If form contains errors
        formWithErrors => {
          val especes = formWithErrors.data.getOrElse("espece[]",List[String]()).asInstanceOf[List[String]].filter(p => p.length > 0)

          //If don't have valid specie
          especes match {
            //print form with prefilled data and a bad request
            case List() => printForm(Results.BadRequest,formWithErrors.withError("espece",Messages("global.error.required")),r)
            //the form is redisplay with error descriptions
            case _ => printForm(Results.BadRequest, formWithErrors, r)
          }
        },

        // Else if form no contains errors
        typeData => {
          //Get valid specie
          val especes = typeData.espece.filter(p => p.length > 0)

          especes match {
            //print form with prefilled data and a bad request
            case List() => printForm(Results.BadRequest,form.withError("espece",Messages("global.error.required")).fill(typeData),r)
            case _ => actionWhenFormValid (errorMessage,r,typeData,especes,verif,f)
          }
        }
      )
    }
  }

  /**
   * Verify if sensor type found and execute a function if sensor type found or not
   * @param id Sensor type id
   * @param found Function executed if sensor type found
   * @param notFound Function executed if sensor type not found
   * @return Return the result of executed function
   */
  def doIfTypeSensorFound(id:BSONObjectID)(found:Unit=>Future[Result])(notFound:Unit=>Future[Result]):Future[Result]={
    //Find the sensor type
    typeSensorDao.findOne(Json.obj("_id"->BSONFormats.BSONObjectIDFormat.writes(id),"delete"->false)).flatMap(
      typeSensorOpt => typeSensorOpt match{

          //If the sensor type not found execute function not found
        case None => notFound()

          //If the sensor type found execute function found
        case Some(typeSensor) => found()
      }
    ).recover({
      //Send Internal Server Error if have mongoDB error
      case _=> InternalServerError("error")
    })
  }

  /**
   * This method verify if the sensor exists before insert/update/reactivat the sensor
   * @param errorMessage Error message print if the sensor exist
   * @param r Route used when submit a form
   * @param typeData Data received from the form
   * @param especes List of valid specie in the form
   * @param verif A method return a JSON Object for get sensor to verify if the sensor exist
   * @param f A method for insert/update/reactivat the sensor
   * @param request
   * @return
   */
  def actionWhenFormValid(errorMessage:String,r:Call,typeData:TypeSensorForm,especes:List[String],verif:TypeSensorForm=>JsObject,f:(TypeSensorForm,List[String],TypeMesure)=>Future[Result])(implicit request:Request[AnyContent])={
    //Find the sensor type
    typeSensorDao.findAll(verif(typeData)).flatMap(
      types=>{
        //If sensor type not found
        if(types.size==0 || List(Some("Réactiver"),Some("Ignorer")).contains(typeData.send)){

          //Find the signal
          typeMesureDao.findOne(Json.obj("nom" -> typeData.mesure)).flatMap(
            mesure => mesure match {

              //If signal not found, insert the signal and applied dedicated function
              case None => applyFunctionWithInsertSensor(typeData, especes, f)
              //If signal found, applied dedicated function
              case _ => f(typeData, especes, mesure.get)
            }
          ).recover({
            //Send Internal Server Error if have mongoDB error
            case e => InternalServerError("error")
          })
        }
        else if(types.filter(p => !(p.delete) ).size>0){
          printForm(Results.BadRequest, form.withGlobalError(Messages("inventary.typeSensor.error.typeExist")).fill(typeData), r)
        }
        else{
          printForm(Results.BadRequest, form.withGlobalError(errorMessage).fill(typeData), r)
        }
      }
    ).recover({
      //Send Internal Server Error if have mongoDB error
      case e => InternalServerError("error")
    })
  }

  /**
   * This method insert signal into the database and after execute dedicated function
   * @param typeData Data received from the form
   * @param especes List of valid specie
   * @param f Dedicated function
   * @return Return Redirect Action if the dedicated function success
   *         Return Internal server error if have mongoDB error
   */
  def applyFunctionWithInsertSensor(typeData:TypeSensorForm,especes:List[String],f:(TypeSensorForm,List[String],TypeMesure)=>Future[Result]): Future[Result] ={
    val mesure_Insert = TypeMesure(nom = typeData.mesure, unite = typeData.unite)

    //Insert the signal into the database
    typeMesureDao.insert(mesure_Insert).flatMap(

      //If signal was insert, execute dedicated function
      e => f(typeData,especes, mesure_Insert)

    ).recover({

      //Send Internal Server Error if have mongoDB error
      case _ => InternalServerError("error")
    })
  }

  /**
   * Print the form with datalist
   * @param status Status for print the form
   * @param form Form configuration
   * @param r Route call when submit the form
   * @return
   */
  def printForm(status:Results.Status,form:Form[TypeSensorForm],r:Call)(implicit request:Request[AnyContent]): Future[Result] ={
    getListData().map(listData=>
      status(
        views.html.sensors.formType(
          form,
          listData.get("modele").get,
          listData.get("fabricant").get,
          listData.get("espece").get,
          listData.get("type").get,
          listData.get("mesure").get,
          listData.get("unite").get,
          r
        )
      )
    )
  }

  /**
   * This method update just the delete column of a sensor type
   * @param selector JsObject for select the sensor type
   * @param delete Value of the delete column
   * @return
   */
  def updateWithDeleteColumn(selector:JsObject,delete:Boolean):Future[Result]={
    //Find the sensor type
    typeSensorDao.findOne(selector).flatMap(
      data=>data match{
        case Some(typeSensorData)=>{

          //Update the sensor type and set the delete column to true
          typeSensorDao.updateById(
            typeSensorData._id,
            typeSensorData.copy(delete=delete)
          ).map(
              //Redirect to the sensors inventary after delete sensors type
              e => Redirect(routes.TypeSensorManager.inventary())
            ).recover({
            //Send Internal Server Error if have mongoDB error
            case e => InternalServerError("error")
          })
        }
        case _ => future{Redirect(routes.TypeSensorManager.inventary())}
      }
    ).recover({
      //Send Internal Server Error if have mongoDB error
      case e => InternalServerError("error")
    })
  }

  def filtreStock(filtre:String)(v:Int)=filtre match{
    case "yes" => v>0
    case "no" => v==0
    case _ => v>=0
  }

  /**
   * Get data for insert to the datalist
   * @return
   */
  def getListData():Future[Map[String, List[BSONDocument]]]={
    val future_modele = typeSensorDao.findListModele("modele")
    val future_fabricant = typeSensorDao.findListFabricant("fabricant")
    val future_espece = typeSensorDao.findListEspece("espece")
    val future_type = typeSensorDao.findListType("nomType")
    val future_mesure = typeMesureDao.findListMesure("nom")
    val future_unite = typeMesureDao.findListUnite("unite")
    future_modele.flatMap(modele=>
      future_fabricant.flatMap(fabricant=>
        future_espece.flatMap(espece=>
          future_type.flatMap(typesData=>
            future_mesure.flatMap(mesure=>
              future_unite.map(unite=>
                Map(
                  "modele"->modele.toList,
                  "fabricant"->fabricant.toList,
                  "espece"->espece.toList,
                  "type"->typesData.toList,
                  "mesure"->mesure.toList,
                  "unite"->unite.toList
                )
              )
            )
          )
        )
      )
    )
  }
}


@Api(value = "/typeSensor", description = "Operations for sensors type")
object TypeSensorManager extends TypeSensorManagerLike{
  this:TypeSensorManagerLike =>

}
