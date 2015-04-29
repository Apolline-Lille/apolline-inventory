package controllers

import java.lang.annotation.Annotation

import com.wordnik.swagger.annotations._
import models._
import play.api.libs.json.Json
import play.api.mvc._
import play.api.data._
import play.api.data.Forms._
import play.modules.reactivemongo.json.BSONFormats
import reactivemongo.bson.{BSONObjectID, BSONDocument}
import reactivemongo.core.commands.LastError

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
   unite:String
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
      "unite"->nonEmptyText
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
  def inventary(sort:String=null)=Action.async{
    request =>
      //Verify if user is connect
      UserManager.doIfconnectAsync(request) {
        //create a selector for filter sensors type
        val selector=if(sort==null){Json.obj("delete"->false)}else{Json.obj("delete"->false,"nomType"->sort)}

        //Find sensors quantity for all type
        val future_stock=sensorDao.countByType()
        //Find all sensors type with the selector
        val future_type=typeSensorDao.findAll(selector)
        //Find all signal
        val future_mesure=typeMesureDao.findAll()
        //Find all sensors type name for the filter
        val future_nomType=typeSensorDao.findAllType()

        //Get value defined on future
        future_type.flatMap(typeSensor=>
          future_mesure.flatMap(typeMesure=>
            future_stock.flatMap(stock=>
              future_nomType.map(nomType=>

                //Display the HTML page
                Ok(views.html.sensors.listTypeSensor(sort,typeSensor,typeMesure,List[String](),stock.toList,nomType.toList))

              ).recover({case e=>InternalServerError("error")})
            ).recover({case e=>InternalServerError("error")})
          ).recover({case e=>InternalServerError("error")})
        ).recover({case e=>InternalServerError("error")})
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
  def typePage=Action{
    request =>
      //Verify if user is connect
      UserManager.doIfconnect(request) {

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
    request =>
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
              typeMesureDao.findById(typeSensor.mesure).map(
              typeMesureOpt=>typeMesureOpt match {

                //Signal not found redirect to the sensor inventary
                case None => Redirect(routes.TypeSensorManager.inventary())

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
      //Verify if the user is connect and if data received are valid
      submitForm(routes.TypeSensorManager.typeInsert()){(typeData,especes,mesure)=>{

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

      //Verify if the user is connect and if data received are valid
      submitForm(routes.TypeSensorManager.typeUpdate(id)){(typeData,especes,mesure)=>{

        //Update sensor type
        typeSensorDao.updateById(BSONObjectID(id),
          TypeSensor(
          _id=BSONObjectID(id),
          nomType=typeData.types,
          modele=typeData.model,
          mesure=mesure._id,
          fabricant=typeData.fabricant,
          nbSignaux=typeData.nbSignaux,
          espece=especes
        )).map(
          //Redirect to the inventary if sensor type was update
          e => Redirect(routes.TypeSensorManager.inventary())
        ).recover({
          //Send Internal Server Error if have mongoDB error
          case e => InternalServerError("error")
        })
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

        //find the sensor type
        typeSensorDao.findOne(Json.obj("_id"->BSONFormats.BSONObjectIDFormat.writes(BSONObjectID(id)))).flatMap(
          data => data match{

            //Sensor type not found redirect to the sensors inventary
            case None =>future{Redirect(routes.TypeSensorManager.inventary())}

            //Sensor type found
            case _ => {

              //Update the sensor type and set the delete column to true
              val typeSensorData = data.get
              typeSensorDao.updateById(
                BSONObjectID(id),
                TypeSensor(
                  _id = BSONObjectID(id),
                  nomType = typeSensorData.nomType,
                  modele = typeSensorData.modele,
                  mesure = typeSensorData.mesure,
                  fabricant = typeSensorData.fabricant,
                  nbSignaux = typeSensorData.nbSignaux,
                  espece = typeSensorData.espece,
                  delete = true
                )
              ).map(
                  //Redirect to the sensors inventary after delete sensors type
                  e => Redirect(routes.TypeSensorManager.inventary())
              ).recover({
                //Send Internal Server Error if have mongoDB error
                case e => InternalServerError("error")
              })

            }
          }
        ).recover({
          //Send Internal Server Error if have mongoDB error
          case e => InternalServerError("error")
        })
      }
  }

  /****************  Methods  ***********************/

  /**
   * Verify if the user is connect and if data received are valid then apply function dedicated
   * @param r Route use for submit the form
   * @param f Function dedicated
   * @param request
   * @return Return Bad request Action if the form is not valid
   *         Return Redirect if dedicated function is a success
   *         Return Internal server error if have mongoDB error
   */
  def submitForm(r:Call)(f:(TypeSensorForm,List[String],TypeMesure)=>Future[Result])(implicit request: Request[AnyContent]):Future[Result]={
    //Verify if user is connect
    UserManager.doIfconnectAsync(request) {
      form.bindFromRequest.fold(

        //If form contains errors
        formWithErrors => {

          //the form is redisplay with error descriptions
          future{printForm(Results.BadRequest,formWithErrors,r)}
        },

        // Else if form no contains errors
        typeData => {
          //Get valid specie
          val especes = typeData.espece.filter(p => p.length > 0)

          //If don't have valid specie
          if (especes.size == 0) {
            //print form with prefilled data and a bad request
            future{printForm(Results.BadRequest,form.withError("espece", "This field is required").fill(typeData),r)}
          } else {

            //Find the sensor type
            typeSensorDao.findOne(Json.obj("modele" -> typeData.model, "fabricant" -> typeData.fabricant)).flatMap(
              e=> e match {

                //If sensor type not found
                case None => {

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
                //print form with prefilled data and a bad request
                case _ => future{printForm(Results.BadRequest, form.withGlobalError("Ce type de capteur existe déjà").fill(typeData), r)}
              }
            ).recover({
              //Send Internal Server Error if have mongoDB error
              case e => InternalServerError("error")
            })
          }
        }
      )
    }
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
  def printForm(status:Results.Status,form:Form[TypeSensorForm],r:Call): Result ={
    val listData = getListData()
    status(views.html.sensors.formType(form, listData.get("modele").get, listData.get("fabricant").get, listData.get("espece").get, listData.get("type").get, r))
  }

  /**
   * Get data for insert to the datalist
   * @return
   */
  def getListData():Map[String, List[BSONDocument]]={
    val future_modele = typeSensorDao.findListModele("modele")
    val future_fabricant = typeSensorDao.findListFabricant("fabricant")
    val future_espece = typeSensorDao.findListEspece("espece")
    val future_type = typeSensorDao.findListType("nomType")
    val modele = Await.result(future_modele, Duration.Inf)
    val fabricant = Await.result(future_fabricant, Duration.Inf)
    val espece = Await.result(future_espece, Duration.Inf)
    val typesData = Await.result(future_type, Duration.Inf)
    Map("modele"->modele.toList,"fabricant"->fabricant.toList,"espece"->espece.toList,"type"->typesData.toList)
  }
}


@Api(value = "/typeSensor", description = "Operations for sensors type")
object TypeSensorManager extends TypeSensorManagerLike{
  this:TypeSensorManagerLike =>

}
