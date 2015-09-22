package controllers

import com.wordnik.swagger.annotations._
import models._
import play.api.data.format.Formats._
import play.api.i18n.Messages
import play.api.libs.json._
import play.api.mvc._
import play.api.data._
import play.api.data.Forms._
import play.modules.reactivemongo.json.BSONFormats
import play.modules.reactivemongo.json.BSONFormats.BSONObjectIDFormat
import reactivemongo.bson.{BSONObjectID, BSONDocument}
import reactivemongo.core.commands._

import scala.concurrent._
import scala.concurrent.duration.Duration

import play.api.libs.concurrent.Execution.Implicits.defaultContext


/**
 * This class represent all information get when the user submit a form for insert or update a sensor type
 * @param model Name of the sensor model
 * @param types Name of the sensor type
 * @param nbSignaux Number of signal received by the sensor
 * @param fabricant Builder of the sensor
 */
case class TypeSensorForm(
   model:String,
   types:String,
   nbSignaux:Int,
   fabricant:String,
   send:Option[String]=None
 )


/**
 * This class represent all information about a specie get when the user submit a form for insert or update a sensor type
 * @param espece Name of the specie
 * @param mesure Name of the mesure
 * @param unite Unity of the mesure
 * @param min Min value received
 * @param max Max value received
 */
case class EspeceForm(
   espece:String,
   mesure:String,
   unite:String,
   min:Float,
   max:Float
)

/**
 * This trait is a controller for manage sensors type
 */
trait TypeSensorManagerLike extends Controller{

  implicit val typeFormFormat:Format[TypeSensorForm]=Json.format[TypeSensorForm]

  implicit val especeFormat:Format[EspeceForm]=Json.format[EspeceForm]

  /************* Property *********************/

  /**
   * Value contains the configuration of the form
   */
  lazy val form=Form[TypeSensorForm](
    mapping(
      "model"->nonEmptyText,
      "types"->nonEmptyText,
      "nbSignaux"->number(min=1),
      "fabricant"->nonEmptyText,
      "send"->optional(text)
    )(TypeSensorForm.apply)(TypeSensorForm.unapply)
  )

  lazy val formEspece=Form[EspeceForm](
    mapping(
      "espece"->nonEmptyText,
      "mesure"->nonEmptyText,
      "unite"->nonEmptyText,
      "min"->of[Float],
      "max"->of[Float]
    )(EspeceForm.apply)(EspeceForm.unapply) verifying(Messages("inventary.typeSensor.error.minGreaterMax"), fields => fields match {
      case data => data.min<data.max
    })
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

  /**
   * DAO for modules
   */
  val moduleDao:ModuleDao=ModuleDaoObj

  /**
   * DAO for species
   */
  val especeDao:EspeceDao=EspeceDaoObj

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
  def inventary(types:String="",modele:String="",filtreSto:String="")=Action.async{
    implicit request =>
      //Verify if user is connect
      UserManager.doIfconnectAsync(request) {
        getInventaryTypeSensor(Json.obj("delete"->false),types,modele){
          (typeSensor,typeMesure,espece,stock,stockUsed,nomType)=>
              //Display the HTML page
              Ok(views.html.sensors.listTypeSensor(filtreSto, types, modele, filtreStock(filtreSto), typeSensor, typeMesure,espece, stock, stockUsed, nomType))
        }
      }
  }

  /**
   * This method is call when the user is on the page /inventary/sensors/type. It display a form for add sensor type information
   * @return Return Ok Action when the user is on the page /inventary/sensors/type with the form
   *         Return Redirect Action when the user is not log in
   */
  @ApiOperation(
    nickname = "inventary/sensors",
    value = "Get the html page for insert a sensor type",
    notes = "Get the html page for insert a sensor type",
    httpMethod = "GET")
  @ApiResponses(Array(
    new ApiResponse(code=303,message="Move resource to the login page at /login if the user is not log")
  ))
  def typePage=Action.async{
    implicit request =>
      //Verify if user is connect
      UserManager.doIfconnectAsync(request) {

        //Display the form for insert new sensor type
        printForm(Results.Ok,form,routes.TypeSensorManager.saveTypeInfo(),request.session + ("typeForm"->"insert") - "typeInfo" - "typeEspece")
      }
  }

  /**
   * This method is call when the user is on the page /inventary/sensors/type. It save sensor type informations on session
   * @return Redirect when the user is not log in or when sensor type informations are saved on session
   *         Bad request if the form is submit with error
   */
  @ApiOperation(
    nickname = "inventary/sensors/type",
    value = "Save sensor type informations on session",
    notes = "Save sensor type informations on session",
    httpMethod = "POST")
  @ApiResponses(Array(
    new ApiResponse(code=303,message="<ul><li>Move resource to the login page at /login if the user is not log</li><li>Move resource to the form for add specie at /inventary/sensors/type/species</li></ul>"),
    new ApiResponse(code=400,message="The form was submit with error")
  ))
  def saveTypeInfo=Action.async{
    implicit request =>
      //Verify if user is connect
      UserManager.doIfconnectAsync(request) {

        form.bindFromRequest.fold(

          //If form contains errors
          formWithErrors => printForm(Results.BadRequest,formWithErrors,routes.TypeSensorManager.saveTypeInfo(),request.session),

          //Save sensor type information on session and redirect to the form for add specie
          data=>future{Redirect(routes.TypeSensorManager.addEspecePage()).withSession(request.session + ("typeInfo"->Json.stringify(typeFormFormat.writes(data))))}
        )
      }
  }

  /**
   * This method is call when the user is on the page /inventary/sensors/type/specie. It display a form for add a specie associat a sensor type
   * @return Return Ok Action when the user is on the page /inventary/sensors/type/specie with the form
   *         Return Redirect Action when the user is not log in
   */
  @ApiOperation(
    nickname = "inventary/sensors/type/specie",
    value = "Get the html page for insert a specie",
    notes = "Get the html page for insert a specie",
    httpMethod = "POST")
  @ApiResponses(Array(
    new ApiResponse(code=303,message="Move resource to the login page at /login if the user is not log")
  ))
  def addEspecePage=Action.async{
    implicit request=>
      //Verify if user is connect
      UserManager.doIfconnectAsync(request) {

        //Find the list of specie
        especeDao.findListEspece().flatMap(
          espece=>
            //Find the list of mesure name
            typeMesureDao.findListMesure("nom").flatMap(
              mesures=>
                //Find the list of unity
                typeMesureDao.findListUnite("unite").map(
                  unite=>

                    //Print the form for add a specie
                    Ok(views.html.sensors.formEspece(formEspece,espece.toList,mesures.toList,unite.toList,routes.TypeSensorManager.saveEspece()))
                )
            )
        )
      }
  }

  /**
   * This method is call when the user is on the page /inventary/sensors/type/specie. It save specie on session
   * @return Redirect when the user is not log in or when specie are saved on session
   *         Bad request if the form is submit with error
   */
  @ApiOperation(
    nickname = "inventary/sensors/type/specie",
    value = "Save specie on session",
    notes = "Save specie on session",
    httpMethod = "POST")
  @ApiResponses(Array(
    new ApiResponse(code=303,message="<ul><li>Move resource to the login page at /login if the user is not log</li><li>Move resource to /inventary/sensors/type/validate for validate the sensor type</li></ul>"),
    new ApiResponse(code=400,message="The form was submit with error")
  ))
  def saveEspece=Action.async{
    implicit request =>
      //Verify if user is connect
      UserManager.doIfconnectAsync(request) {

        formEspece.bindFromRequest.fold(

          //If form contains errors, find the list of specie
          formWithErrors =>especeDao.findListEspece().flatMap(
            espece=>
              //Find the list of mesure name
              typeMesureDao.findListMesure("nom").flatMap(
                mesures=>
                  //Find the list of unity
                  typeMesureDao.findListUnite("unite").map(
                    unite=>
                      //Send bad request with a prefilled form and errors
                      BadRequest(views.html.sensors.formEspece(formWithErrors,espece.toList,mesures.toList,unite.toList,routes.TypeSensorManager.saveEspece()))
                  )
              )
          ),

          //Redirect to the page for validate species
          data=>future{Redirect(routes.TypeSensorManager.validationPage()).withSession(request.session + ("typeEspece"->Json.stringify(especesToJson(data::getEspeceOnSession))))}
        )
      }
  }

  /**
   * This method is call when the user is on the page /inventary/sensors/type/validate. It display a resume of the sensor type
   * @return Return Ok Action when the user is on the page /inventary/sensors/type/validate with a resume of the sensor type
   *         Return Redirect Action when the user is not log in
   */
  @ApiOperation(
    nickname = "inventary/sensors/type/validate",
    value = "Get the html page for display a resume of the sensor type",
    notes = "Get the html page for display a resume of the sensor type",
    httpMethod = "POST")
  @ApiResponses(Array(
    new ApiResponse(code=303,message="Move resource to the login page at /login if the user is not log")
  ))
  def validationPage=Action.async{
    implicit request=>
      //Verify if user is connect
      UserManager.doIfconnectAsync(request) {
        //Display the resume of the sensor type
        future{Ok(views.html.sensors.validation(getTypeOnSession,getEspeceOnSession,""))}
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
    new ApiResponse(code=303,message="<ul><li>Move resource to the login page at /login if the user is not log</li><li>Move resource to the sensor inventary page at /inventary/sensors when sensor type not found</li></ul>"),
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

                  //Prepare data for prefilled the form
                  val typeSensorData = TypeSensorForm(
                    typeSensor.modele,
                    typeSensor.nomType,
                    typeSensor.nbSignaux,
                    typeSensor.fabricant
                  )

                  getEspeceForm(typeSensor.espece).flatMap(
                    espece=>
                      //Display the form for update sensor type
                      printForm(Results.Ok,form.fill(typeSensorData),routes.TypeSensorManager.saveTypeInfo(),request.session + ("typeForm"->"update") + ("typeInfo"->Json.stringify(typeFormFormat.writes(typeSensorData))) + ("typeEspece"->Json.stringify(especesToJson(espece))) + ("typeId"->id))
                  )
            }
          }
        ).recover({
          //Send an Internal Server Error for mongoDB error
          case e=>InternalServerError("error")
        })
      }
  }


  /**
   * This method is call when the user is on the page /inventary/sensors/type/update. It display a prefilled form for update sensor type information
   * @return Return Ok Action when the user is on the page /inventary/sensors/type/update with the prefilled form
   *         Return Redirect Action when the user is not log in
   */
  @ApiOperation(
    nickname = "inventary/sensors/type/update",
    value = "Get the html page for update a sensor type information",
    notes = "Get the html page for update a sensor type information",
    httpMethod = "GET")
  @ApiResponses(Array(
    new ApiResponse(code=303,message="Move resource to the login page at /login if the user is not log")
  ))
  def updateInfo=Action.async{
    implicit request =>
      //Verify if user is connect
      UserManager.doIfconnectAsync(request) {
        getTypeOnSession match {
          case None =>
            //Display the form for update sensor type
            printForm(Results.Ok, form, routes.TypeSensorManager.saveTypeInfo(), request.session)
          case Some(typeData)=>
            //Display the form for update sensor type
            printForm(Results.Ok, form.fill(typeData), routes.TypeSensorManager.saveTypeInfo(), request.session)
        }
      }
  }

  /**
   * This method is call when the user is on the page /inventary/sensors/type/validate. It save sensor type on mongoDB
   * @return Redirect when the user is not log in or when sensor type is saved on mongoDB
   *         Bad request if the sensor type is not valid
   */
  @ApiOperation(
    nickname = "inventary/sensors/type/validate",
    value = "Save the sensor type on mongoDB",
    notes = "Save the sensor type on mongoDB",
    httpMethod = "POST")
  @ApiResponses(Array(
    new ApiResponse(code=303,message="<ul><li>Move resource to the login page at /login if the user is not log</li><li>Move resource to the list of sensors at /inventary/sensors"),
    new ApiResponse(code=400,message="The sensor type is not valid")
  ))
  def validate=Action.async{
    implicit request=>
      request.session.get("typeForm") match{
        case Some("update")=>
          request.session.get("typeId") match{
            case None=>future{Redirect(routes.TypeSensorManager.inventary())}
            case Some(id)=>typeUpdate(id)
          }
        case _=>typeInsert
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

        val idFormat=BSONObjectID(id)
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

  /**
   * Delete a specie when insert or update a sensor type
   * @param id Index of the specie
   * @return
   */
  @ApiOperation(
    nickname = "inventary/sensors/type/validate",
    value = "Delete a specie when insert or update a sensor type",
    notes = "Delete a specie when insert or update a sensor type",
    httpMethod = "GET")
  @ApiResponses(Array(
    new ApiResponse(code=303,message="<ul><li>Move resource to the login page at /login if the user is not log</li><li>Move resource /inventary/sensors/type/validate when after delete a specie"),
    new ApiResponse(code=500,message="Have a mongoDB error")
  ))
  @ApiImplicitParams(Array(
    new ApiImplicitParam(value = "Index of the specie",required=true,name="id", dataType = "Int", paramType = "path")
  ))
  def deleteEspece(id:Int)=Action.async{
    implicit request=>
      //Verify if user is connect
      UserManager.doIfconnectAsync(request) {

        //Find specie saved on session
        val s=getEspeceOnSession

        //Remove a specie
        val newList=if(s.size<id) s else s.take(id)++s.drop(id+1)

        //Redirect to the page for for validate the sensor type
        future{Redirect(routes.TypeSensorManager.validationPage).withSession(request.session + ("typeEspece"->Json.stringify(especesToJson(newList))))}
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
  def getInventaryTypeSensor(selector:JsObject,filtreType:String,filtreModele:String)(f:(List[TypeSensor],List[TypeMesure],List[Espece],List[BSONDocument],List[(BSONObjectID,Int)],List[BSONDocument])=>Result)={
    val selectorAll=(filtreType,filtreModele) match{
      case ("","")=>selector
      case (_,"")=>selector ++ Json.obj("nomType"->filtreType)
      case ("",_)=>selector ++ Json.obj("modele"->Json.obj("$regex"->(".*"+filtreModele+".*")))
      case _ =>selector ++ Json.obj("nomType"->filtreType,"modele"->Json.obj("$regex"->(".*"+filtreModele+".*")))
    }

    //Find sensors quantity for all type
    val future_stock=sensorDao.countByType()
    //Find all signal
    val future_mesure=typeMesureDao.findAll()
    //Find all sensors type name for the filter
    val future_nomType=BSONFormats.toBSON(selector) match{
      case JsSuccess(v,_)=>typeSensorDao.findAllType(v.asInstanceOf[BSONDocument])
      case _=>future{Stream[BSONDocument]()}
    }

    //Get value defined on future
    typeSensorDao.findAll(selectorAll).flatMap(typeSensor=>
      especeDao.findAll(Json.obj("_id"->Json.obj("$in"->typeSensor.foldRight(List[BSONObjectID]()){(types,list)=>types.espece:::list}))).flatMap(
        espece=>
          future_mesure.flatMap(typeMesure=>
            future_stock.flatMap(stock=>
              future_nomType.flatMap(nomType=>
                sensorDao.countUsedSensors(typeSensor).map(
                  stockUsed=>
                    //Print the list of sensors type
                    f(typeSensor,typeMesure,espece,stock.toList,stockUsed,nomType.toList)
                ).recover({case e=>InternalServerError("error")})
              ).recover({case e=>InternalServerError("error")})
            ).recover({case e=>InternalServerError("error")})
          ).recover({case e=>InternalServerError("error")})
      )
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
  def submitForm(errorMessage:String,r:Call)(verif:TypeSensorForm=>JsObject)(f:(TypeSensorForm,List[BSONObjectID])=>Future[Result])(implicit request: Request[AnyContent]):Future[Result]={
    //Verify if user is connect
    UserManager.doIfconnectAsync(request) {

      //Find specie on session
      getEspeceOnSession match {

        //Send bad request if not have specie
        case Nil=>future{BadRequest(views.html.sensors.validation(getTypeOnSession,List(),Messages("inventary.typeSensor.error.especeNotFound")))}

        //apply function for insert or update sensor type
        case especes=>actionWhenFormValid(errorMessage,r,getTypeOnSession,especes,verif,f)
      }
    }
  }

  /**
   * Insert the sensor type on mongoDB
   * @param request Request received
   * @return
   */
  def typeInsert(implicit request:Request[AnyContent])={
    val msg=Messages("inventary.typeSensor.error.typeExist")+" <input type=\"submit\" class=\"btn btn-danger\" name=\"send\" value=\""+Messages("global.reactiver")+"\"/> <input type=\"submit\" class=\"btn btn-danger\" name=\"send\" value=\""+Messages("global.ignorer")+"\"/>"
    //Verify if the user is connect and if data received are valid
    submitForm(msg,routes.TypeSensorManager.validate()) {
      typeData => Json.obj("modele" -> typeData.model, "fabricant" -> typeData.fabricant)
    }{(typeData,especes)=>{
      if(typeData.send.isEmpty || typeData.send.equals(Some("Ignorer"))){
        //Insert sensor type
        typeSensorDao.insert(TypeSensor(
          nomType=typeData.types,
          modele=typeData.model,
          fabricant=typeData.fabricant,
          espece=especes,
          nbSignaux=typeData.nbSignaux
        )).map(
            //Redirect to the inventary if sensor type was insert
            e =>Redirect(routes.TypeSensorManager.inventary())
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
   * Update the sensor type on mongoDB
   * @param request Request received
   * @return
   */
  def typeUpdate(id:String)(implicit request:Request[AnyContent])={
    val msg=Messages("inventary.typeSensor.error.typeExist")+" <input type=\"submit\" class=\"btn btn-danger\" name=\"send\" value=\""+Messages("global.ignorer")+"\"/>"

    //Verify if the user is connect and if data received are valid
    submitForm(msg,routes.TypeSensorManager.validate()){
      typeData => Json.obj("_id"->Json.obj("$ne"->BSONObjectID(id)),"modele" -> typeData.model, "fabricant" -> typeData.fabricant)
    }{(typeData,especes)=>{
      if(typeData.send.isEmpty || typeData.send.equals(Some("Ignorer"))) {
        //Update sensor type
        typeSensorDao.updateById(BSONObjectID(id),
          TypeSensor(
            _id = BSONObjectID(id),
            nomType = typeData.types,
            modele = typeData.model,
            fabricant = typeData.fabricant,
            espece=especes,
            nbSignaux = typeData.nbSignaux
          )).map(
            //Redirect to the inventary if sensor type was update
            e => Redirect(routes.TypeSensorManager.inventary())
          ).recover({
          //Send Internal Server Error if have mongoDB error
          case e => InternalServerError("error")
        })
      }else{
        future{Results.BadRequest(views.html.sensors.validation(getTypeOnSession,getEspeceOnSession,msg))}
      }
    }
    }
  }

  /**
   * Verify if sensor type found and execute a function if sensor type found or not
   * @param id Sensor type id
   * @param found Function executed if sensor type found
   * @param notFound Function executed if sensor type not found
   * @return Return the result of executed function
   */
  def doIfTypeSensorFound(id:BSONObjectID)(found:TypeSensor=>Future[Result])(notFound:Unit=>Future[Result]):Future[Result]={
    //Find the sensor type
    typeSensorDao.findOne(Json.obj("_id"->id,"delete"->false)).flatMap(
      typeSensorOpt => typeSensorOpt match{

          //If the sensor type not found execute function not found
        case None => notFound()

          //If the sensor type found execute function found
        case Some(typeSensor) => found(typeSensor)
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
   * @param typeOpt Data received from the form
   * @param especes List of valid specie in the form
   * @param verif A method return a JSON Object for get sensor to verify if the sensor exist
   * @param f A method for insert/update/reactivat the sensor
   * @param request
   * @return
   */
  def actionWhenFormValid(errorMessage:String,r:Call,typeOpt:Option[TypeSensorForm],especes:List[EspeceForm],verif:TypeSensorForm=>JsObject,f:(TypeSensorForm,List[BSONObjectID])=>Future[Result])(implicit request:Request[AnyContent]):Future[Result]=typeOpt match{
    case None=>future{BadRequest(views.html.sensors.validation(typeOpt,especes,Messages("inventary.typeSensor.error.typeInfo")))}
    case Some(typeData)=>
      //Find the sensor type
      typeSensorDao.findAll(verif(typeData)).flatMap(
        types=>(typeData.send,types) match{
          //If type not found or click on reactivat or ignore button
          case (Some("Réactiver"),_) | (Some("Ignorer"),_) | (_,Nil) =>
            Future.sequence(insertEspeces(especes)).flatMap(
              espece => f(typeData, espece)
            )

            //If type exist and delete
          case _ if types.filter(p => !(p.delete) ).size>0 =>future{BadRequest(views.html.sensors.validation(typeOpt,especes,Messages("inventary.typeSensor.error.typeExist")))}

            //If type exist and not delete
          case _ => future{BadRequest(views.html.sensors.validation(typeOpt,especes,errorMessage))}
        }
      ).recover({
        //Send Internal Server Error if have mongoDB error
        case e => Results.InternalServerError("error")
      })
  }

  /**
   * Insert species and mesure type on mongoDB
   * @param espece List of species submit
   * @return The list of BSONObjectID associat to species
   */
  def insertEspeces(espece:List[EspeceForm]):List[Future[BSONObjectID]]=espece match{
      //Return an empty list if not have specie
    case Nil=>List()

    case h::t=>
      //Find the list of mesure
      typeMesureDao.findOne(Json.obj("nom"->h.mesure,"unite"->h.unite)).flatMap(
        mesureOpt=>mesureOpt match{

            //If not have mesure
          case None=>{
            //Create mesure type
            val mesure=TypeMesure(nom=h.mesure,unite=h.unite)

            //Insert mesure type on mongoDB
            typeMesureDao.insert(mesure).flatMap(

              //Insert specie
              data=>insertEspece(h,mesure._id)
            )
          }

            //If have mesure, insert specie
          case Some(mesure)=>insertEspece(h,mesure._id)
        }
      )::insertEspeces(t)
  }

  /**
   * Insert specie on mongoDB
   * @param espece A Specie
   * @param mesure Mesure type BSONObjectID
   * @return Return the BSONObjectID of specie
   */
  def insertEspece(espece:EspeceForm,mesure:BSONObjectID):Future[BSONObjectID]=
    //Find specie
    especeDao.findOne(Json.obj("espece"->espece.espece,"mesure"->mesure,"min"->espece.min,"max"->espece.max)).flatMap(

      especeOpt=>especeOpt match {

        //If specie not exist
        case None=>{

          //Create specie
          val esp=Espece(espece=espece.espece,mesure=mesure,min=espece.min,max=espece.max)

          //Insert specie on mongoDB
          especeDao.insert(esp).map(

            //Return specie BSONObjectID
            data=>esp._id
          )
        }

        //If specie exist, return BSONObjectID
        case Some(esp)=>future{esp._id}
      }
  )

  /**
   * Print the form with datalist
   * @param status Status for print the form
   * @param form Form configuration
   * @param r Route call when submit the form
   * @return
   */
  def printForm(status:Results.Status,form:Form[TypeSensorForm],r:Call,session:Session)(implicit request:Request[AnyContent]): Future[Result] ={
    getListData().map(listData=>
      status(
        views.html.sensors.formType(
          form,
          listData.get("modele").get,
          listData.get("fabricant").get,
          listData.get("type").get,
          r
        )
      ).withSession(session)
    )
  }

  /**
   * Find species saved on session
   * @param request Request received
   * @return List of species saved on session
   */
  def getEspeceOnSession(implicit request:Request[AnyContent]):List[EspeceForm]=request.session.get("typeEspece") match {

    //If the session not exist, return an empty list
    case None=>List()

    //If the session exist, return the list of species saved on session
    case Some(str)=>Json.parse(str).as[List[EspeceForm]]
  }

  /**
   * Find sensor type saved on session
   * @param request Request received
   * @return Sensor type saved on session
   */
  def getTypeOnSession(implicit request:Request[AnyContent]):Option[TypeSensorForm]=request.session.get("typeInfo") match{

    //If the session not exist, return None
    case None=>None

    //If the session exist, return the sensor type
    case Some(info)=>Some(Json.parse(info).as[TypeSensorForm])
  }

  /**
   * Parse the list of species to JSON
   * @param especes List of specie
   * @return Return Json represent a list of species
   */
  def especesToJson(especes:List[EspeceForm]):JsArray=JsArray(especes.foldRight(List[JsValue]()){
    (esp,list)=>especeFormat.writes(esp)::list
  })

  /**
   * Find species associat to a sensor type for update a sensor type
   * @param id Sensor type id
   * @return Return species for update a sensor type
   */
  def getEspeceForm(id:List[BSONObjectID]):Future[List[EspeceForm]]=
    //Find the list of species
    especeDao.findAll(Json.obj("_id"->Json.obj("$in"->id))).flatMap(
      espece=>

        //Find the list of mesure
        typeMesureDao.findAll(Json.obj("_id"->Json.obj("$in"->espece.foldRight(List[BSONObjectID]()){
          (esp,list)=>esp.mesure::list
        }))).map(
          mesure=>

            //for each species create specie for update a sensor type
            espece.foldRight(List[EspeceForm]()){
              (esp,list)=>mesure.find(p=>p._id.equals(esp.mesure)) match{
                case None=>list
                case Some(m)=>EspeceForm(esp.espece,m.nom,m.unite,esp.min,esp.max)::list
              }
            }
        )
    )

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
    val future_type = typeSensorDao.findListType("nomType")
    future_modele.flatMap(modele=>
      future_fabricant.flatMap(fabricant=>
        future_type.map(typesData=>
          Map(
            "modele"->modele.toList,
            "fabricant"->fabricant.toList,
            "type"->typesData.toList
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
