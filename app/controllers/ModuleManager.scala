package controllers

import java.text.SimpleDateFormat
import java.util.Date

import com.wordnik.swagger.annotations._
import models._
import play.api.Play
import play.api.data.Form
import play.api.data.Forms._
import play.api.i18n.Messages
import play.api.libs.json.{JsObject, JsValue, JsArray, Json}
import play.api.mvc._
import play.modules.reactivemongo.json.BSONFormats.BSONObjectIDFormat
import reactivemongo.bson.BSONObjectID
import scala.collection.immutable.HashSet
import scala.concurrent._

import play.api.Play.current
import play.api.libs.concurrent.Execution.Implicits.defaultContext

/**
 * This class is used when user select cards or sensors for associat it to a module
 * @param elements List of select elements
 */
case class SelectInfo(elements:List[String],send:String)

/**
 * This class is used when user submit a form with general information for a module
 * @param types Type of module
 * @param id Module id
 * @param dateAssemblage Date when the module is assembly
 * @param commentaire A comment for the module
 */
case class ModuleForm(types:String,id:String,dateAssemblage:Date,commentaire:Option[String])

/**
 * This trait is a controller for manage module
 */
trait ModuleManagerLike extends Controller {

  /** *********** Property *********************/

  /**
   * Manager for type cards
   */
  val typeCardsManager:TypeCardsManagerLike = TypeCardsManager

  /**
   * Manager for cards
   */
  val cardsManager:CardsManagerLike = CardsManager

  /**
   * Manager for type sensors
   */
  val typeSensorManager:TypeSensorManagerLike = TypeSensorManager

  /**
   * Manager for sensors
   */
  val sensorsManager:SensorManagerLike=SensorManager

  /**
   * DAO for module
   */
  val moduleDao: ModuleDao = ModuleDaoObj

  /**
   * DAO for cards
   */
  val cardsDao: CardsDao = CardsDaoObj

  /**
   * DAO for type cards
   */
  val typeCardsDao: TypeCardsDao = TypeCardsDaoObj

  /**
   * DAO for sensors
   */
  val sensorsDao: SensorDao = SensorDaoObj

  /**
   * DAO for type sensors
   */
  val typeSensorsDao: TypeSensorDao = TypeSensorDaoObj

  val firmwareDao:FirmwareDao = FirmwareDaoObj

  val typeMesureDao:TypeMesureDao = TypeMesureDaoObj

  val config=Play.configuration

  val selectElement=Form[SelectInfo](
    mapping(
      "elements"->list(text),
      "send"->nonEmptyText
    )(SelectInfo.apply)(SelectInfo.unapply)
  )

  val form=Form[ModuleForm](
    mapping(
      "types"->nonEmptyText,
      "id"->nonEmptyText,
      "dateAssemblage"->date,
      "commentaire"->optional(text)
    )(ModuleForm.apply)(ModuleForm.unapply)
  )

  /** **************** Route methods ***********/

  /**
   * This method is call when the user is on the page /inventary/modules. It list module available
   * @return Return Ok Action when the user is on the page /inventary/modules with the list of module
   *         Return Redirect Action when the user is not log in
   *         Return Internal Server Error Action when have mongoDB error
   */
  @ApiOperation(
    nickname = "inventary",
    value = "Get the html page for list module",
    notes = "Get the html page for list module",
    httpMethod = "GET")
  @ApiResponses(Array(
    new ApiResponse(code = 303, message = "Move resource to the login page at /login if the user is not log"),
    new ApiResponse(code = 500, message = "Have a mongoDB error")
  ))
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name="filtre",value="Value used for filter modules depending on their type",dataType="String",paramType="query")
  ))
  def inventary(filtre:String="") = Action.async {
    implicit request =>
      //Verify if user is connect
      UserManager.doIfconnectAsync(request) {
        val selector=if(filtre.isEmpty) Json.obj("delete"->false) else { Json.obj("delete"->false,"types"->filtre) }
        moduleDao.findAll(selector).flatMap(
          modules=> {
            moduleDao.findListType().map(
              listType=>
                //Print the list of module
                Ok(views.html.module.listModule(modules,listType.toList,filtre))
            )
          }
        )
      }
  }

  /**
   * This method is call when the user is on the page /inventary/modules/:id. It print module information
   * @return Return Ok Action when the user is on the page /inventary/modules/ with module information
   *         Return Redirect Action when the user is not log in or if module not found
   */
  @ApiOperation(
    nickname = "inventary/modules/:id",
    value = "Get the html page for show module information",
    notes = "Get the html page for show module information",
    httpMethod = "GET")
  @ApiResponses(Array(
    new ApiResponse(code = 303, message = "<ul><li>Move resource to the login page at /login if the user is not log</li><li>Move resource to module inventary at /inventary/modules/:id if module not found")
  ))
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name="id",value="id of the module which get more information",dataType="String",paramType="path",required=true)
  ))
  def moreInformation(id:String) = Action.async {
    implicit request=>
      //Verify if user is connect
      UserManager.doIfconnectAsync(request){

        //Find the module
        moduleDao.findOne(Json.obj("delete"->false,"_id"->BSONObjectID(id))).flatMap(
          data => data match{

              //If module not found redirect
            case None =>future{Redirect(routes.ModuleManager.inventary())}

            //If module found
            case Some(module)=>{

              //Find sensors information
              findSensorsInfo(module).flatMap(
                dataSensor => dataSensor match {

                  case (typeSensors,typeMesure,sensors)=>

                    //Find cards information
                    findCardsInfo (module).map (
                      data => data match {
                        case (typesCards, cards,firmware) =>

                          val previous=previousPage
                          //Print module information
                          Ok(views.html.module.moreInfo(module,typesCards,cards,firmware,typeSensors,typeMesure,sensors,previous)).withSession(request.session + ("previous"->previous))
                      }
                    )
                }
              )
            }
          }
        )
      }
  }

  /**
   * This method is call when the user is on the page /inventary/modules/form. It print a form for add information of the module
   * @return Return Ok Action when the user is on the page /inventary/modules/form with the form
   *         Return Redirect Action when the user is not log in
   *         Return Internal Server Error Action when have mongoDB error
   */
  @ApiOperation(
    nickname = "inventary/modules/form",
    value = "Get the html form page for add a new module",
    notes = "Get the html form page for add a new module",
    httpMethod = "GET")
  @ApiResponses(Array(
    new ApiResponse(code = 303, message = "Move resource to the login page at /login if the user is not log"),
    new ApiResponse(code = 500, message = "Have a mongoDB error")
  ))
  def formGeneralAdd()=Action.async{
    implicit request=>
      //Verify if user is connect
      UserManager.doIfconnectAsync(request) {

        //Create new session with an empty module and flag  indicat it's a form for insert module
        val session=request.session + ("moduleForm" -> "insert") + ("module" -> Module.toStrings(Module(id="",types="",dateAssemblage=new Date,cartes=List(),capteurs=List(),commentaire=None)))

        //Print the form
        printForm(form.bind(Map("dateAssemblage"->new SimpleDateFormat("YYYY-MM-dd").format(new Date))).discardingErrors,Results.Ok,session)
      }
  }

  /**
   * This method is call when the user is on the page /inventary/modules/form/:id. It print a form for add information of the module
   * @return Return Ok Action when the user is on the page /inventary/modules/form/:id with the form
   *         Return Redirect Action when the user is not log in
   *         Return Internal Server Error Action when have mongoDB error
   */
  @ApiOperation(
    nickname = "inventary/modules/form/:id",
    value = "Get the html form page for update a module",
    notes = "Get the html form page for update a module",
    httpMethod = "GET")
  @ApiResponses(Array(
    new ApiResponse(code = 303, message = "Move resource to the login page at /login if the user is not log"),
    new ApiResponse(code = 500, message = "Have a mongoDB error")
  ))
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name="id",value="Id of the module to update",dataType="String",paramType="path",required=true)
  ))
  def formGeneralUpdate(id:String)=Action.async{
    implicit request=>
      //Verify if user is connect
      UserManager.doIfconnectAsync(request) {

        moduleDao.findOne(Json.obj("delete"->false,"_id"->BSONObjectID(id))).flatMap(
          data =>data match {
            case None => future{Redirect(routes.ModuleManager.inventary())}
            case Some(module) => {
              //Create new session with an empty module and flag  indicat it's a form for insert module
              val session = request.session + ("moduleForm" -> "update") + ("module" -> Module.toStrings(module))
              val formData=ModuleForm(module.types,module.id,module.dateAssemblage,module.commentaire)

              //Print the form
              printForm(form.fill(formData), Results.Ok, session)
            }
          }
        )
      }
  }

  /**
   * This method is call when the user is on the page /inventary/modules/form/update. It print a form for add information of the module
   * @return Return Ok Action when the user is on the page /inventary/modules/form/update with the form
   *         Return Redirect Action when the user is not log in
   *         Return Internal Server Error Action when have mongoDB error
   */
  @ApiOperation(
    nickname = "inventary/modules/form/update",
    value = "Get the html form page for update input information",
    notes = "Get the html form page for update input information",
    httpMethod = "GET")
  @ApiResponses(Array(
    new ApiResponse(code = 303, message = "Move resource to the login page at /login if the user is not log"),
    new ApiResponse(code = 500, message = "Have a mongoDB error")
  ))
  def formUpdate()=Action.async{
    implicit request=>
      //Verify if user is connect
      UserManager.doIfconnectAsync(request) {

        val module=Module.toModule(request.session.get("module"))
        val formData=ModuleForm(module.types,module.id,module.dateAssemblage,module.commentaire)

        //Print the form
        printForm(form.fill(formData), Results.Ok, request.session)
      }
  }

  /**
   * This method is call when the user is on the page /inventary/modules/form/typeCards. It display a page for list cards type if they have cards not used.
   * @return Return Ok Action when the user is on the page /inventary/modules/form/typeCards with a page for list cards type if they have cards not used
   *         Return Redirect Action when the user is not log in
   */
  @ApiOperation(
    nickname = "inventary/modules/form/typeCards",
    value = "Get the html page for display a page for list cards type if they have cards not used",
    notes = "Get the html page for display a page for list cards type if they have cards not used",
    httpMethod = "GET")
  @ApiResponses(Array(
    new ApiResponse(code=303,message="Move resource to the login page at /login if the user is not log"),
    new ApiResponse(code=500,message="Send 500 internal error if mongoDB error")
  ))
  @ApiImplicitParams(Array(
    new ApiImplicitParam(value = "Name of type cards used for filter type cards",name="filtre", dataType = "String", paramType = "query")
  ))
  def formTypeCards(filtreType:String="") = Action.async{
    implicit request =>
      //Verify if user is connect
      UserManager.doIfconnectAsync(request) {
        val module=Module.toModule(request.session.get("module"))
        moduleDao.fold(Json.obj("delete"->false,"_id"->Json.obj("$ne"->module._id)),Json.obj(),module.cartes)((list,mod) => list ++ mod.cartes).flatMap(
          listCardsUsed=> {
            val listCards=listCardsUsed.mapConserve(p=>BSONObjectIDFormat.writes(p)).asInstanceOf[List[JsValue]]
            //Find type cards id of type cards have cards in stock
            cardsDao.fold(Json.obj("delete" -> false,"_id"->Json.obj("$nin"->JsArray(listCards.toSeq))), Json.obj(), HashSet[JsValue]())((list, card) => list + BSONObjectIDFormat.writes(card.types)).flatMap(
              idType => {

                //Find the list of type cards
                val selector = Json.obj("delete" -> false, "_id" -> Json.obj("$in" -> JsArray(idType.toSeq)))
                typeCardsManager.getInventaryTypeCards(selector, filtreType) {
                  (types, filtre, count,countUsed) => Ok(views.html.module.listTypeCards(filtreType, types, count,countUsed, filtre))
                }

              }
            )
          }
        )
      }
  }

  /**
   * This method is call when the user is on the page /inventary/modules/form/cards/:id. It display a page for list cards not used for add associate it to a module.
   * @return Return Ok Action when the user is on the page /inventary/modules/form/cards/:id with a page for list cards not used for add associate it to a module
   *         Return Redirect Action when the user is not log in
   */
  @ApiOperation(
    nickname = "inventary/modules/form/cards/:id",
    value = "Get the html page for display a page for list cards not used for add associate it to a module",
    notes = "Get the html page for display a page for list cards not used for add associate it to a module",
    httpMethod = "GET")
  @ApiResponses(Array(
    new ApiResponse(code=303,message="Move resource to the login page at /login if the user is not log or to /inventary/modules/form/typeCards"),
    new ApiResponse(code=500,message="Send 500 internal error if mongoDB error")
  ))
  @ApiImplicitParams(Array(
    new ApiImplicitParam(value = "Cards type id",name="id", dataType = "String", paramType = "path"),
    new ApiImplicitParam(value = "Name of column cards used for sort cards",name="sort", dataType = "String", paramType = "query"),
    new ApiImplicitParam(value = "Flag indicate if the sort is ascending or descending",name="sens", dataType = "Int", paramType = "query")
  ))
  def formCards(id:String,sort:String="id",sens:Int=1)=Action.async{
    implicit request=>
      //Verify if user is connect
      UserManager.doIfconnectAsync(request) {
        val module=Module.toModule(request.session.get("module"))
        moduleDao.fold(Json.obj("delete"->false,"_id"->Json.obj("$ne"->module._id)),Json.obj(),module.cartes)((list,card) => list ++ card.cartes).flatMap(
          listCardsUsed=> {
            val listCards = listCardsUsed.mapConserve(p => BSONObjectIDFormat.writes(p)).asInstanceOf[List[JsValue]]
            //Find the list of cards
            cardsManager.getInventaryCards(Json.obj("delete" -> false, "types" -> BSONObjectID(id),"_id"->Json.obj("$nin"->listCards)), Json.obj(sort -> sens), BSONObjectID(id), Redirect(routes.ModuleManager.formTypeCards())) {
              (typeCards, listCards, firmware,cardsUsed) => Ok(views.html.module.listCards(selectElement, typeCards, listCards, firmware,cardsUsed, sort, sens))
            }
          }
        )
      }
  }

  /**
   * This method is call when the user is on the page /inventary/modules/form/typeSensors. It display a page for list sensors type if they have sensors not used.
   * @return Return Ok Action when the user is on the page /inventary/modules/form/typeSensors with a page for list sensors type if they have sensors not used
   *         Return Redirect Action when the user is not log in
   */
  @ApiOperation(
    nickname = "inventary/modules/form/typeSensors",
    value = "Get the html page for display a page for list sensors type if they have sensors not used",
    notes = "Get the html page for display a page for list sensors type if they have sensors not used",
    httpMethod = "GET")
  @ApiResponses(Array(
    new ApiResponse(code=303,message="Move resource to the login page at /login if the user is not log"),
    new ApiResponse(code=500,message="Send 500 internal error if mongoDB error")
  ))
  @ApiImplicitParams(Array(
    new ApiImplicitParam(value = "Name of type cards used for filter type sensors",name="filtre", dataType = "String", paramType = "query")
  ))
  def formTypeSensors(filtreType:String="")=Action.async{
    implicit request=>
      //Verify if user is connect
      UserManager.doIfconnectAsync(request) {
        val module=Module.toModule(request.session.get("module"))
        moduleDao.fold(Json.obj("delete"->false,"_id"->Json.obj("$ne"->module._id)),Json.obj(),module.capteurs)((list,card) => list ++ card.capteurs).flatMap(
          listSensorsUsed=> {
            val listSensors = listSensorsUsed.mapConserve(p => BSONObjectIDFormat.writes(p)).asInstanceOf[List[JsValue]]
            //Find type sensors id of type cards have sensors in stock
            sensorsDao.fold(Json.obj("delete" -> false,"_id"->Json.obj("$nin"->JsArray(listSensors.toSeq))), Json.obj(), HashSet[JsValue]())((list, card) => list + BSONObjectIDFormat.writes(card.types)).flatMap(
              idType =>

                //Find the list of type sensors
                typeSensorManager.getInventaryTypeSensor(Json.obj("delete" -> false, "_id" -> Json.obj("$in" -> JsArray(idType.toSeq))), filtreType) {
                  (typeSensor, typeMesure, stock,stockUsed, nomType) => Ok(views.html.module.listTypeSensors(filtreType, typeSensor, typeMesure, stock,stockUsed, nomType))
                }

            ).recover({ case e => InternalServerError("error") })
          }
        )
      }
  }

  /**
   * This method is call when the user is on the page /inventary/modules/form/sensors/:id. It display a page for list sensors not used for add associate it to a module.
   * @return Return Ok Action when the user is on the page /inventary/modules/form/sensors/:id with a page for list sensors not used for add associate it to a module
   *         Return Redirect Action when the user is not log in
   */
  @ApiOperation(
    nickname = "inventary/modules/form/sensors/:id",
    value = "Get the html page for display a page for list sensors not used for add associate it to a module",
    notes = "Get the html page for display a page for list sensors not used for add associate it to a module",
    httpMethod = "GET")
  @ApiResponses(Array(
    new ApiResponse(code=303,message="Move resource to the login page at /login if the user is not log or to /inventary/modules/form/typeSensors"),
    new ApiResponse(code=500,message="Send 500 internal error if mongoDB error")
  ))
  @ApiImplicitParams(Array(
    new ApiImplicitParam(value = "Sensors type id",name="id", dataType = "String", paramType = "path"),
    new ApiImplicitParam(value = "Name of column sensors used for sort sensors",name="sort", dataType = "String", paramType = "query"),
    new ApiImplicitParam(value = "Flag indicate if the sort is ascending or descending",name="sens", dataType = "Int", paramType = "query")
  ))
  def formSensors(id:String,sort:String="id",sens:Int=1)=Action.async{
    implicit request=>
      //Verify if user is connect
      UserManager.doIfconnectAsync(request) {
        val module=Module.toModule(request.session.get("module"))
        moduleDao.fold(Json.obj("delete"->false,"_id"->Json.obj("$ne"->module._id)),Json.obj(),module.capteurs)((list,card) => list ++ card.capteurs).flatMap(
          listSensorsUsed=> {
            val listSensors = listSensorsUsed.mapConserve(p => BSONObjectIDFormat.writes(p)).asInstanceOf[List[JsValue]]
            //Find the list of sensors
            sensorsManager.getInventarySensor(Json.obj("delete" -> false, "types" -> BSONObjectID(id), "_id" -> Json.obj("$nin" -> JsArray(listSensors))), Json.obj(sort -> sens), BSONObjectID(id), Redirect(routes.ModuleManager.formTypeSensors())) {
              (typeSensor, typeMesure, listSensors,sensorsUsed) => Ok(views.html.module.listSensors(selectElement, typeSensor, typeMesure, listSensors,sensorsUsed, sort, sens))
            }
          }
        )
      }
  }

  /**
   * This method is called when user is on the page /inventary/modules/form/cards/:id. It associat selected cards to a module
   * @param idType Cards type id
   * @return Redirect if user not log in, after associat cards or if cards type not found
   *         Bad request if form was send with error
   */
  @ApiOperation(
    nickname = "inventary/modules/form/cards/:id",
    value = "Associat cards to module",
    notes = "Associat cards to module",
    httpMethod = "POST")
  @ApiResponses(Array(
    new ApiResponse(code=303,message="<ul><li>Move resource to the login page at /login if the user is not log</li><li>Move resource to the cards inventary in the module form at /inventary/modules/form/cards/:id if cards associat to module or if cards type not found</li></ul>"),
    new ApiResponse(code=400,message="Fields required or not valid")
  ))
  @ApiImplicitParams(Array(
    new ApiImplicitParam(value = "Id of the card type",required=true,name="idType", dataType = "String", paramType = "path"),
    new ApiImplicitParam(value = "Id of cards selected",name="elements", dataType = "List[String]", paramType = "form")
  ))
  def addCards(idType:String)=Action.async{
    implicit request=>
      //Verify if user is connect
      UserManager.doIfconnectAsync(request) {
        //Verify if card type found
        typeCardsManager.doIfTypeCardsFound(BSONObjectID(idType)) {_=>
          selectElement.bindFromRequest.fold(

            //If form contains errors
            formWithErrors => {
              //Find the list of cards
              cardsManager.getInventaryCards(Json.obj("delete"->false,"types"->BSONObjectID(idType)),Json.obj("id"->1),BSONObjectID(idType),Redirect(routes.ModuleManager.formTypeCards())){
                (typeCards,listCards,firmware,cardsUsed)=>BadRequest(views.html.module.listCards(formWithErrors,typeCards,listCards,firmware,cardsUsed,"id",1))
              }
            },

            // Else if form no contains errors
            data =>

              //If have cards selected
              if(data.elements.nonEmpty){

                //Associat cards to module
                addCardsToModule(data)
              }else{
                if(data.send.equals(Messages("inventary.module.nextState"))){
                  future{Redirect(routes.ModuleManager.formTypeSensors())}
                }
                else {
                  future{Redirect(routes.ModuleManager.formTypeCards())}
                }
              })
        }{_=>
          future{Redirect(routes.ModuleManager.formTypeCards())}
        }
      }
  }

  /**
   * This method is called when user is on the page /inventary/modules/form/sensors/:id. It associat selected sensors to a module
   * @param idType Sensors type id
   * @return Redirect if user not log in, after associat sensors or if sensors type not found
   *         Bad request if form was send with error
   */
  @ApiOperation(
    nickname = "inventary/modules/form/sensors/:id",
    value = "Associat sensors to module",
    notes = "Associat sensors to module",
    httpMethod = "POST")
  @ApiResponses(Array(
    new ApiResponse(code=303,message="<ul><li>Move resource to the login page at /login if the user is not log</li><li>Move resource to the sensors inventary in the module form at /inventary/modules/form/sensors/:id if sensors associat to module or if sensors type not found</li></ul>"),
    new ApiResponse(code=400,message="Fields required or not valid")
  ))
  @ApiImplicitParams(Array(
    new ApiImplicitParam(value = "Id of the sensor type",required=true,name="idType", dataType = "String", paramType = "path"),
    new ApiImplicitParam(value = "Id of sensors selected",name="elements", dataType = "List[String]", paramType = "form")
  ))
  def addSensors(idType:String)=Action.async{
    implicit request=>
      //Verify if user is connect
      UserManager.doIfconnectAsync(request) {
        //Verify if sensor type found
        typeSensorManager.doIfTypeSensorFound(BSONObjectID(idType)) {_=>
          selectElement.bindFromRequest.fold(

            //If form contains errors
            formWithErrors => {
              //Find the list of cards
              sensorsManager.getInventarySensor(Json.obj("delete"->false,"types"->BSONObjectID(idType)),Json.obj("id"->1),BSONObjectID(idType),Redirect(routes.ModuleManager.formTypeCards())){
                (typeSensor,typeMesure,listSensors,sensorUsed)=>BadRequest(views.html.module.listSensors(selectElement,typeSensor,typeMesure,listSensors,sensorUsed,"id",1))
              }
            },

            // Else if form no contains errors
            data =>

              //If have cards selected
              if(data.elements.nonEmpty){

                //Associat cards to module
                addSensorsToModule(data)
              }else{
                if(data.send.equals(Messages("inventary.module.nextState"))){
                  future{Redirect(routes.ModuleManager.validate())}
                }
                else {
                  future{Redirect(routes.ModuleManager.formTypeSensors())}
                }
              })
        }{_=>
          future{Redirect(routes.ModuleManager.formTypeSensors())}
        }
      }
  }

  /**
   * This method is called when user is on the page /inventary/modules/form. It add module information
   * @return Redirect if user not log in, after add module information
   *         Bad request if form was send with error
   */
  @ApiOperation(
    nickname = "inventary/modules/form",
    value = "Add module information",
    notes = "Add module information",
    httpMethod = "POST")
  @ApiResponses(Array(
    new ApiResponse(code=303,message="<ul><li>Move resource to the login page at /login if the user is not log</li><li>Move resource to the form for module information at /inventary/modules/form/typeCards if module information are valid</li></ul>"),
    new ApiResponse(code=400,message="Fields required or not valid")
  ))
  @ApiImplicitParams(Array(
    new ApiImplicitParam(value = "Module type",name="types", dataType = "String", paramType = "form"),
    new ApiImplicitParam(value = "Id of module",name="id", dataType = "String", paramType = "form"),
    new ApiImplicitParam(value = "Assembly date",name="dateAssemblage", dataType = "Date", paramType = "form"),
    new ApiImplicitParam(value = "Commentary for the module",name="commentaire", dataType = "String", paramType = "form")
  ))
  def addInformation()=Action.async{
    implicit request=>
      //Verify if user is connect
      UserManager.doIfconnectAsync(request) {
        form.bindFromRequest.fold(

          //If form contains errors
          formWithErrors => {
            printForm(formWithErrors,Results.BadRequest,request.session)
          },

          // Else if form no contains errors
          data => {
            //Find module object in session
            val mod = Module.toModule(request.session.get("module"))
            val selector=request.session.get("moduleForm") match{
              case Some("update") => Json.obj("_id"->Json.obj("$ne"->mod._id),"delete" -> false, "types" -> data.types, "id" -> data.id)
              case _=>Json.obj("delete" -> false, "types" -> data.types, "id" -> data.id)
            }

            //Find module
            moduleDao.findOne(selector).flatMap(

              moduleOpt => moduleOpt match {

                //If module not found
                case None => {
                  //Add information to the module
                  val newMod = mod.copy(id = data.id, types = data.types, dateAssemblage = data.dateAssemblage, commentaire = data.commentaire)

                  //Redirect to the next step
                  future {Redirect(routes.ModuleManager.formTypeCards()).withSession(request.session + ("module" -> Module.toStrings(newMod)))}
                }

                //If module found print form with error
                case _ => printForm(form.fill(data).withGlobalError(Messages("inventary.module.error.moduleExist")), Results.BadRequest, request.session)
              }

            ).recover({ case _ => InternalServerError("error") })
          }
        )
      }
  }

  /**
   * This method is called when user is on the page /inventary/modules/form/validate. It print all module information before insert it into the database
   * @return Ok with all module information
   *         Redirect if user not log in or if module information or not valid
   *         Bad request if the number of cards or sensors selected is not the same of the available number
   */
  @ApiOperation(
    nickname = "inventary/modules/form/validate",
    value = "Print all module information",
    notes = "Print all module information",
    httpMethod = "GET")
  @ApiResponses(Array(
    new ApiResponse(code=303,message="<ul><li>Move resource to the login page at /login if the user is not log</li><li>Move resource to /inventary/modules/form/typeCards if module not have cards</li><li>Move resource to /inventary/modules/form if module information not valid</li></ul>"),
    new ApiResponse(code=400,message="Number of cards or sensors selected is not the same of the available number")
  ))
  def validate()=Action.async{
    implicit request =>
      //Verify if user is connect
      UserManager.doIfconnectAsync(request) {
        //Find module object in session
        val mod = Module.toModule(request.session.get("module"))

        //Find the list of sensors used
        moduleDao.fold(selector=Json.obj("_id"->Json.obj("$ne"->mod._id),"delete"->false),state=List[BSONObjectID]())((list,mod)=>list ++ mod.capteurs).flatMap(
          listSensorsUsed=> {
            //Find sensors information for selected sensors
            findSensorsInfo(mod, listSensorsUsed).flatMap(
              dataSensor => dataSensor match {
                case (typeSensors, typeMesure, sensors) =>

                  //Find the list of cards used
                  moduleDao.fold(selector=Json.obj("_id"->Json.obj("$ne"->mod._id),"delete"->false),state=List[BSONObjectID]())((list,mod)=>list ++ mod.cartes).flatMap(
                    listCardsUsed=> {
                      //Find cards information for selected cards
                      findCardsInfo(mod,listCardsUsed).flatMap(
                        data => data match {
                          case (typesCards, cards, firmware) => verifyData(mod, cards, sensors) {
                            //Print the summary with an error message
                            (mod, error) => future {BadRequest(views.html.module.validateForm(mod, typesCards, cards, firmware, typeSensors, typeMesure, sensors, error))}
                          } {
                            //Print the summary
                            mod => future {Ok(views.html.module.validateForm(mod, typesCards, cards, firmware, typeSensors, typeMesure, sensors, ""))}
                          }
                        }
                      )
                    }
                  )
              }
            )
          }
        )
      }
  }

  /**
   * This method is called when user submit form on the page /inventary/modules/form/validate. It add module information on the database
   * @return Redirect if user not log in or if module information are not valid
   *         Redirect after insert module
   */
  @ApiOperation(
    nickname = "inventary/modules/form/validate",
    value = "Add module information on the database",
    notes = "Add module information on the database",
    httpMethod = "POST")
  @ApiResponses(Array(
    new ApiResponse(code=303,message="<ul><li>Move resource to the login page at /login if the user is not log</li><li>Move resource to /inventary/modules/form/typeCards if module not have cards</li><li>Move resource to /inventary/modules/form if module information not valid</li><li>Move resource to /inventary/modules after insert module information</li></ul>")
  ))
  def insertModule()=Action.async{
    implicit request =>
      //Verify if user is connect
      UserManager.doIfconnectAsync(request) {
        request.session.get("moduleForm") match{

          //If insert form
          case Some("insert") =>submitForm(
            mod =>
            //Insert the module to mongoDB
            moduleDao.insert(mod).map(
              data=>
                //Redirect to the inventary
                Redirect(routes.ModuleManager.inventary()).withSession(request.session - "module" - "moduleForm")
            ).recover({case _=>InternalServerError("error")})
          )
          case Some("update") => submitForm(
            mod =>
              //update the module to mongoDB
              moduleDao.updateById(mod._id,mod).map(
                data=>
                  //Redirect to the inventary
                  Redirect(routes.ModuleManager.inventary()).withSession(request.session - "module" - "moduleForm")
              ).recover({case _=>InternalServerError("error")})
          )
          case _=>future{Redirect(routes.ModuleManager.validate())}
        }
      }
  }

  /**
   * This method is called when user submit form on the page /inventary/modules/:id/delete. It delete a module
   * @return Redirect if user not log in
   *         Redirect if module not found
   *         Redirect after delete module
   */
  @ApiOperation(
    nickname = "inventary/modules/:id/delete",
    value = "Delete a module",
    notes = "Delete a module",
    httpMethod = "GET")
  @ApiResponses(Array(
    new ApiResponse(code=303,message="<ul><li>Move resource to the login page at /login if the user is not log</li><li>Move resource to /inventary/modules if module not found</li><li>Move resource to /inventary/modules after delete module<li>Move resource to /inventary/modules/form if module information not valid</li><li>Move resource to /inventary/modules after insert module information</li></ul>")
  ))
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name="id",value="Module id",dataType="String",required=true,paramType="path")
  ))
  def deleteModule(id:String)=Action.async{
    implicit request=>
      //Verify if user is connect
      UserManager.doIfconnectAsync(request) {

        //Find the module
        moduleDao.findOne(Json.obj("delete"->false,"_id"->BSONObjectID(id))).flatMap(
          data=>data match{

              //If module not found, redirect
            case None => future{Redirect(routes.ModuleManager.inventary())}

              //If module found, change delete flag to true then redirect
            case Some(module) => moduleDao.updateById(module._id,module.copy(delete=true)).map(
              dataUpdate=>Redirect(routes.ModuleManager.inventary())
            )
          }
        )
      }
  }

  /**
   * This method is called when user submit form on the page /inventary/modules/form/cards/:id/delete. It delete a cards into a module
   * @return Redirect if user not log in
   *         Redirect after delete cards into a module
   */
  @ApiOperation(
    nickname = "inventary/modules/form/cards/:id/delete",
    value = "Delete a cards into a module",
    notes = "Delete a cards into a module",
    httpMethod = "GET")
  @ApiResponses(Array(
    new ApiResponse(code=303,message="<ul><li>Move resource to the login page at /login if the user is not log</li><li>Move resource to /inventary/modules/form/validate after delete cards into module</li></ul>")
  ))
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name="id",value="Cards id",dataType="String",required=true,paramType="path")
  ))
  def deleteCards(id:String)=Action{
    implicit request=>
      //Verify if user is connect
      UserManager.doIfconnect(request) {
        //Find module in the session
        val module=Module.toModule(request.session.get("module"))

        //Find cards associat to the module
        val cartes=module.cartes.diff(List(BSONObjectID(id)))

        //Create new module
        val newModule=module.copy(cartes=cartes)

        //redirect to validate page
        Redirect(routes.ModuleManager.validate()).withSession(request.session + ("module" -> Module.toStrings(newModule)))
      }
  }

  /**
   * This method is called when user submit form on the page /inventary/modules/form/sensors/:id/delete. It delete a sensor into a module
   * @return Redirect if user not log in
   *         Redirect after delete sensor into a module
   */
  @ApiOperation(
    nickname = "inventary/modules/form/sensors/:id/delete",
    value = "Delete a sensor into a module",
    notes = "Delete a sensor into a module",
    httpMethod = "GET")
  @ApiResponses(Array(
    new ApiResponse(code=303,message="<ul><li>Move resource to the login page at /login if the user is not log</li><li>Move resource to /inventary/modules/form/validate after delete sensor into module</li></ul>")
  ))
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name="id",value="Sensor id",dataType="String",required=true,paramType="path")
  ))
  def deleteSensors(id:String)=Action{
    implicit request=>
      //Verify if user is connect
      UserManager.doIfconnect(request) {
        //Find module in the session
        val module=Module.toModule(request.session.get("module"))

        //Find sensors associat to the module
        val sensors=module.capteurs.diff(List(BSONObjectID(id)))

        //Create new module
        val newModule=module.copy(capteurs=sensors)

        //Redirect to validate page
        Redirect(routes.ModuleManager.validate()).withSession(request.session + ("module" -> Module.toStrings(newModule)))
      }
  }

  /**
   * Find cards and sensors associat to module in session. Then verify if all module information are valid. After apply dedicated function
   * @param func Dedicated function
   * @param request Request received when submit the form
   * @return
   */
  def submitForm(func:Module=>Future[Result])(implicit request:Request[AnyContent])={
    //Find module object in session
    val mod = Module.toModule(request.session.get("module"))

    moduleDao.fold(selector=Json.obj("_id"->Json.obj("$ne"->mod._id),"delete"->false),state=List[BSONObjectID]())((list,mod)=>list ++ mod.capteurs).flatMap(
      listSensorsUsed=> {
        //Find the list of cards used
        moduleDao.fold(selector = Json.obj("_id" -> Json.obj("$ne" -> mod._id), "delete" -> false), state = List[BSONObjectID]())((list, mod) => list ++ mod.cartes).flatMap(
          listCardsUsed => {
            //Format list of cards and sensors for query
            val listCards = mod.cartes.diff(listCardsUsed).mapConserve(p => BSONObjectIDFormat.writes(p)).asInstanceOf[List[JsValue]]
            val listSensors = mod.capteurs.diff(listSensorsUsed).mapConserve(p => BSONObjectIDFormat.writes(p)).asInstanceOf[List[JsValue]]

            //Find cards associat to the module
            cardsDao.findAll(Json.obj("delete" -> false, "_id" -> Json.obj("$in" -> JsArray(listCards.toSeq)))).flatMap(
              cards =>

                //Find sensors associat to the module
                sensorsDao.findAll(Json.obj("delete" -> false, "_id" -> Json.obj("$in" -> JsArray(listSensors.toSeq)))).flatMap(
                  sensors => verifyData(mod, cards, sensors) {

                    //Redirect if module information have error
                    (mod, error) => future {
                      Redirect(routes.ModuleManager.validate())
                    }
                  } {
                    //Apply the function
                    mod => func(mod)
                  }
                )
            )
          }
        )
      }
    )
  }

  /**
   * This method verify if module information are valid.
   * @param mod Module with her information
   * @param cards List of available cards
   * @param sensors List of available sensors
   * @param error Function called if module information have error
   * @param success Function called if module information haven't error
   * @return Redirect if module id or module type is empty
   *         Redirect if module haven't got cards
   *         The result of error function if the number of cards or sensors is not correct
   *         The result of error function if module exist
   *         The result of success function if haven't error
   */
  def verifyData(mod:Module,cards:List[Cards],sensors:List[Sensor])(error:(Module,String)=>Future[Result])(success:Module=>Future[Result])={
    mod match{

      //If module id is empty
      case Module(_,"",_,_,_,_,_,_) =>future{Redirect(routes.ModuleManager.formUpdate())}

      //If module type is empty
      case Module(_,_,"",_,_,_,_,_) =>future{Redirect(routes.ModuleManager.formUpdate())}

      //If module haven't got cards
      case _ if cards.size==0 =>future{Redirect(routes.ModuleManager.formTypeCards())}

      //If the number of cards or sensors is not correct
      case _ if cards.size != mod.cartes.size || sensors.size != mod.capteurs.size =>{
        val cartes=cards.mapConserve(p=>p._id).asInstanceOf[List[BSONObjectID]]
        val capteurs=sensors.mapConserve(p=>p._id).asInstanceOf[List[BSONObjectID]]
        error(mod.copy(cartes=cartes,capteurs=capteurs),Messages("inventary.module.error.cardsOrSensorNotFound"))
      }

      case _ => moduleDao.findOne(Json.obj("id"->mod.id,"types"->mod.types,"_id"->Json.obj("$ne"->mod._id))).flatMap(
        data => data match{
          //If module not exist
          case None => success(mod)

          //If module exist
          case _ => error(mod,Messages("inventary.module.error.moduleExist"))
        }
      )
    }
  }

  /**
   * This method get all information for cards associat to a module
   * @param mod A module
   * @return Return list of type cards, cards information and firmware for cards associat to a module
   */
  def findCardsInfo(mod:Module,listCardsUsed:List[BSONObjectID]=List()):Future[(List[TypeCards],List[Cards],List[Firmware])]={
        //Find the list of cards selected without cards used
        val listCards=mod.cartes.diff(listCardsUsed).mapConserve(p=>BSONObjectIDFormat.writes(p)).asInstanceOf[List[JsValue]]

        //Get the list of type cards for cards selected available
        val getTypesCards=cardsDao.fold(Json.obj("_id"->Json.obj("$in"->JsArray(listCards.toSeq)),"delete"->false),Json.obj(),HashSet[BSONObjectID]())((set,cards)=>set + cards.types).flatMap(
          types=>typeCardsDao.findAll(Json.obj("delete"->false,"_id"->Json.obj("$in"->JsArray(types.toList.mapConserve(p=>BSONObjectIDFormat.writes(p)).asInstanceOf[List[JsValue]].toSeq))))
        )

        //Get the list of firmware
        val getFirm=firmwareDao.findAll()

        //Find cards available
        cardsDao.findAll(Json.obj("delete"->false,"_id"->Json.obj("$in"->JsArray(listCards.toSeq)))).flatMap(
          cards=>
            getTypesCards.flatMap(
              typesCards=>
                getFirm.map(

                  firm => (typesCards,cards,firm)

                )
            )
        )
  }

  /**
   * This method get all information for sensors associat to a module
   * @param mod A module
   * @return Return list of type sensors, type mesure and sensors information for sensors associat to a module
   */
  def findSensorsInfo(mod:Module,listSensorsUsed:List[BSONObjectID]=List()):Future[(List[TypeSensor],List[TypeMesure],List[Sensor])]={
    //Find the list of sensors selected without sensors used
    val listSensors = mod.capteurs.diff(listSensorsUsed).mapConserve(p => BSONObjectIDFormat.writes(p)).asInstanceOf[List[JsValue]]
    //Get the list of type sensors for sensors selected available
    val selector=Json.obj("delete"->false,"_id" -> Json.obj("$in" -> JsArray(listSensors.toSeq)))
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
   * This method associat cards to a module
   * @param data List of cards selected
   * @param request
   * @return
   */
  def addCardsToModule(data:SelectInfo)(implicit request:Request[AnyContent]):Future[Result]={
    //Transform list of string with id of selected cards to list of BSONObjectID
    val listId:List[Object]=data.elements.mapConserve(p=>BSONObjectIDFormat.writes(BSONObjectID(p)))

    //Find list of available cards
    cardsDao.fold(Json.obj("_id"->Json.obj("$in"->JsArray(listId.asInstanceOf[List[JsValue]].toSeq))),Json.obj(),List[BSONObjectID]())((list,mod)=>list :+ mod._id).map(
      availableId=> {

        //Get module
        val mod = Module.toModule(request.session.get("module"))

        //Create new module instance with selected cards add
        val newMod = mod.copy(cartes = mod.cartes ++ availableId)

        if(data.send.equals(Messages("inventary.module.nextState"))){
          Redirect(routes.ModuleManager.formTypeSensors()).withSession(request.session + ("module" -> Module.toStrings(newMod)))
        }
        else {
          //Redirect to list of cards type
          Redirect(routes.ModuleManager.formTypeCards()).withSession(request.session + ("module" -> Module.toStrings(newMod)))
        }
      }
    ).recover({case _ => InternalServerError("error")})
  }

  /**
   * This method associat sensors to a module
   * @param data List of sensors selected
   * @param request
   * @return
   */
  def addSensorsToModule(data:SelectInfo)(implicit request:Request[AnyContent]):Future[Result]={
    //Transform list of string with id of selected cards to list of BSONObjectID
    val listId:List[Object]=data.elements.mapConserve(p=>BSONObjectIDFormat.writes(BSONObjectID(p)))

    //Find list of available cards
    sensorsDao.fold(Json.obj("_id"->Json.obj("$in"->JsArray(listId.asInstanceOf[List[JsValue]].toSeq))),Json.obj(),List[BSONObjectID]())((list,mod)=>list :+ mod._id).map(
      availableId=> {

        //Get module
        val mod = Module.toModule(request.session.get("module"))

        //Create new module instance with selected sensors add
        val newMod = mod.copy(capteurs = mod.capteurs ++ availableId)

        if(data.send.equals(Messages("inventary.module.nextState"))){
          Redirect(routes.ModuleManager.validate()).withSession(request.session + ("module" -> Module.toStrings(newMod)))
        }
        else {
          //Redirect to list of sensors type
          Redirect(routes.ModuleManager.formTypeSensors()).withSession(request.session + ("module" -> Module.toStrings(newMod)))
        }
      }
    ).recover({case _ => InternalServerError("error")})
  }

  /**
   * This method print the form for insert module information
   * @param form Form configuration
   * @param status Status of the response
   * @param session Session used for the response
   * @param request Request received
   * @return
   */
  def printForm(form:Form[ModuleForm],status:Results.Status,session:Session)(implicit request:Request[AnyContent]): Future[Result] ={
    //Find the list of module type
    moduleDao.findListType().map(
      listType=>

        //Print the form
        status(views.html.module.formModule(form,listType.toList)).withSession(session)

    ).recover({case _ => InternalServerError("error")})
  }

  /**
   * Get the address of the previous page for the sensors inventary
   * @param request Request received
   * @return A string represent the address
   */
  def previousPage(implicit request:Request[AnyContent]): String ={
    //Find the hostname in the application.conf
    config.getString("hostname") match{

      //hostname found
      case Some(hostname)=>getPreviousPage(hostname,request.headers.get("Referer"))

      //hostname not found
      case None=>throw new Exception("hostname is not defined")
    }
  }

  /**
   * Get the address of the previous page for the module inventary
   * @param hostname The hostname
   * @param urlOrigin The url
   * @param request Request received
   * @return A String represent the address
   */
  def getPreviousPage(hostname:String,urlOrigin:Option[String])(implicit request:Request[AnyContent]): String=urlOrigin match{

    //url found and start with the hostname
    case Some(url) if url.startsWith(hostname)=> {

      //get the address without hostname
      val shortUrl=url.replace(hostname,"/")

      val pattern="/inventary/modules[^/]*".r

      shortUrl match{
        //if url match with the pattern, return the url
        case pattern() => url

        //Else return the address for the inventary type cards
        case _ => routes.ModuleManager.inventary().toString
      }
    }

    //Else return the address for the inventary type sensors
    case _=>routes.ModuleManager.inventary().toString
  }
}

@Api(value = "/module", description = "Operations for module")
object ModuleManager extends ModuleManagerLike
