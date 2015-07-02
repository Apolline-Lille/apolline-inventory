package controllers

import java.io.File
import java.lang.annotation.Annotation
import java.text.SimpleDateFormat
import java.util.{UUID, Date}

import com.wordnik.swagger.annotations._
import models._
import play.api.Play
import play.api.data.Form
import play.api.data.Forms._
import play.api.data.validation.Constraints._
import play.api.i18n.Messages
import play.api.libs.Files.TemporaryFile
import play.api.libs.json._
import play.api.mvc._
import reactivemongo.bson.BSONObjectID
import reactivemongo.core.commands.LastError
import reactivemongo.extensions.BSONFormats.BSONObjectIDFormat
import scala.collection.immutable.HashSet
import scala.concurrent._
import play.api.data.format.Formats._

import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.Play.current

import scala.concurrent.duration.Duration

case class ConditionForm(debut:Date,fin:Option[Date],commentaire:Option[String])

case class FormSelect(id:String)

case class LocalisationForm(nom:String,lat:Option[Float],lon:Option[Float],commentaire:Option[String])

/**
 * This object is a controller for manage all condition
 */
trait ConditionsManagerLike extends Controller{

  /************* Property *********************/

  /**
   * DAO for conditions
   */
  val conditionsDao:ConditionDao=ConditionDaoObj

  val moduleDao:ModuleDao=ModuleDaoObj

  val localisationDao:LocalisationDao=LocalisationDaoObj

  val campaignDao:CampagneDao=CampagneDaoObj

  val sensorDao:SensorDao=SensorDaoObj

  val cardDao:CardsDao=CardsDaoObj

  val moduleManager:ModuleManagerLike=ModuleManager

  val campaignManager:CampagneManagerLike=CampagneManager

  val app=Play.application

  val tempFileBuilder=new TemporaryFileBuilder

  val form=Form[ConditionForm](
    mapping(
      "debut"->date("dd/MM/yyyy HH:mm:ss"),
      "fin"->optional(date("dd/MM/yyyy HH:mm:ss")),
      "commentaire"->optional(text)
    )(ConditionForm.apply)(ConditionForm.unapply)
  )

  val formSelect=Form[FormSelect](
    mapping(
      "id"->nonEmptyText
    )(FormSelect.apply)(FormSelect.unapply)
  )

  val localisationForm=Form[LocalisationForm](
    mapping(
      "nom"->nonEmptyText,
      "lat"->optional(of(floatFormat)),
      "lon"->optional(of(floatFormat)),
      "commentaire"->optional(text)
    )(LocalisationForm.apply)(LocalisationForm.unapply)
  )

  /****************** Route methods ***********/

  /**
   * This method is call when the user is on the page /campaigns/campaign/:id. It list condtions for a campaign
   * @return Return Ok Action when the user is on the page /campaigns/campaign/:id with the list of conditions for a campaign
   *         Return Redirect Action when the user is not log in
   *         Return Internal Server Error Action when have mongoDB error
   */
  @ApiOperation(
    nickname = "conditions",
    value = "Get the html page for list conditions for a campaign",
    notes = "Get the html page for list conditions for a campaign",
    httpMethod = "GET")
  @ApiResponses(Array(
    new ApiResponse(code=303,message="Move resource to the login page at /login if the user is not log"),
    new ApiResponse(code=500,message="Have a mongoDB error")
  ))
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name="id",value="Campaign id",required=true,dataType="String",paramType="path")
  ))
  def listConditions(id:String,filtre:String="")=Action.async{
    implicit request =>
      //Verify if user is connect
      UserManager.doIfconnectAsync(request) {

        //Verify if campaign exist
        campaignManager.doIfCampaignFound(BSONObjectID(id)){

          //If campaign found
          campaign=> {

            //Find conditions id associat to the campaign
            val idCond=campaign.conditions.mapConserve(p => Json.toJson(p)).asInstanceOf[List[JsValue]]

            //Find conditions
            conditionsDao.findAll(createQueryConditionDate(filtre,idCond)).flatMap(
              conditions => {

                //Find module id associat to conditions
                val idMod = conditions.foldLeft(HashSet[JsValue]())((set, cond) => set + Json.toJson(cond.modules))

                //Find module
                moduleDao.findAll(Json.obj("_id" -> Json.obj("$in" -> JsArray(idMod.toSeq)))).flatMap(
                  modules =>

                    //Find localisations
                    findLocalisation(campaign.types,idCond).map(
                      localisations=>Ok(views.html.campaign.listCondition(campaign,conditions,modules,localisations,filtre,id))
                    )
                )
              }
            )
          }
        }{

          //If campaign not found
          _ => future{Redirect(routes.CampagneManager.listCampaign())}
        }
      }
  }

  def moreInformation(id1:String,id2:String)=Action.async{
    implicit request =>
      //Verify if user is connect
      UserManager.doIfconnectAsync(request) {

        //Verify if campaign exist
        campaignManager.doIfCampaignFound(BSONObjectID(id1)){

          //If campaign found
          campaign=> {

            //Find condition
            conditionsDao.findOne(Json.obj("_id"->BSONObjectID(id2))).flatMap(
              conditionOpt => conditionOpt match{
                case None =>future{Redirect(routes.ConditionsManager.listConditions(id1))}
                case Some(condition)=>
                  //Find module
                  moduleDao.findOne(Json.obj("_id" -> condition.modules,"delete"->false)).flatMap(
                  moduleOpt => moduleOpt match{
                    case None=>future{Redirect(routes.ConditionsManager.listConditions(id1))}
                    case Some(module)=>
                      localisationDao.findOne(Json.obj("condition"->BSONObjectID(id2))).map(
                        localisation => Ok(views.html.campaign.moreInformation(condition,localisation,module,id1))
                      )
                  }
                )
              }
            )
          }
        }{

          //If campaign not found
          _ => future{Redirect(routes.CampagneManager.listCampaign())}
        }
      }
  }

  /**
   * This method is call when the user is on the page /campaigns/campaign/:id/form. It display form for insert condition general information
   * @return Return Ok Action when the user is on the page /campaigns/campaign/:id/form with a form for insert condition general information
   *         Return Redirect Action when the user is not log in
   *         Return Internal Server Error Action when have mongoDB error
   */
  @ApiOperation(
    nickname = "conditions/form",
    value = "Get the html page with form for insert condition general information",
    notes = "Get the html page with form for insert condition general information",
    httpMethod = "GET")
  @ApiResponses(Array(
    new ApiResponse(code=303,message="Move resource to the login page at /login if the user is not log"),
    new ApiResponse(code=500,message="Have a mongoDB error")
  ))
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name="id",value="Campaign id",required=true,dataType="String",paramType="path")
  ))
  def formGeneral(id:String)=Action.async{
    implicit request =>
      //Verify if user is connect
      UserManager.doIfconnectAsync(request) {

        //Verify if campaign exist
        campaignManager.doIfCampaignFound(BSONObjectID(id)){

          //If campaign found
          campaign=> {
            val session=request.session + ("condition"->Json.stringify(Json.obj("form"->"insert")))
            future{Ok(views.html.campaign.formConditions(form.bind(Map("debut"->new SimpleDateFormat("dd/MM/yyyy HH:mm:ss").format(new Date))),id, printStateForm("infoCondition", campaign.types, id))).withSession(session)}
          }
        }{

          //If campaign not found
          _ => future{Redirect(routes.CampagneManager.listCampaign())}
        }
      }
  }

  /**
   * This method is call when the user is on the page /campaigns/campaign/:id/:id2/form. It display form for update a condition
   * @return Return Ok Action when the user is on the page /campaigns/campaign/:id/:id2/form with a form for update a condition
   *         Return Redirect Action when the user is not log in
   *         Return Internal Server Error Action when have mongoDB error
   */
  @ApiOperation(
    nickname = "conditions/form/update",
    value = "Get the html page with form for update a condition",
    notes = "Get the html page with form for update a condition",
    httpMethod = "GET")
  @ApiResponses(Array(
    new ApiResponse(code=303,message="Move resource to the login page at /login if the user is not log"),
    new ApiResponse(code=500,message="Have a mongoDB error")
  ))
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name="id",value="Campaign id",required=true,dataType="String",paramType="path"),
    new ApiImplicitParam(name="id2",value="Condition id",required=true,dataType="String",paramType="path")
  ))
  def formGeneralUpdate(id:String,id2:String)=Action.async{
    implicit request =>
      //Verify if user is connect
      UserManager.doIfconnectAsync(request) {

        //Verify if campaign exist
        campaignManager.doIfCampaignFound(BSONObjectID(id)){

          //If campaign found
          campaign=>

            //Find the condition
            conditionsDao.findById(BSONObjectID(id2)).flatMap(
            conditionOpt=>conditionOpt match{

                //If condition not found redirect to the list of conditions
              case None=>future{Redirect(routes.ConditionsManager.listConditions(id))}

                //If condition found, find the localisation
              case Some(condition)=>localisationDao.findOne(Json.obj("condition"->condition._id)).map(
                localisation=>{

                  //Prepare data condition for set to the session
                  val session=request.session + ("condition"->Json.stringify(Json.obj("form"->"update","id"->id2,"debut"->condition.dateDebut,"fin"->condition.dateFin,"commentaire"->condition.commentaire,"module"->condition.modules.stringify,"localisation"->localisation)))

                  //Prepare data for prefilled the form
                  val data=Map("debut"->new SimpleDateFormat("dd/MM/yyyy HH:mm:ss").format(condition.dateDebut),
                    "fin"->condition.dateFin.fold("")(d=>new SimpleDateFormat("dd/MM/yyyy HH:mm:ss").format(d)),
                    "commentaire"->condition.commentaire.getOrElse("")
                  )

                  //Display the form
                  Ok(views.html.campaign.formConditions(form.bind(data),id, printStateForm("infoCondition", campaign.types, id))).withSession(session)
                }
              )
            }
          )
        }{

          //If campaign not found
          _ => future{Redirect(routes.CampagneManager.listCampaign())}
        }
      }
  }

  /**
   * This method is call when the user is on the page /campaigns/campaign/:id/form/update. It display form for update input condition general information
   * @return Return Ok Action when the user is on the page /campaigns/campaign/:id/form/update with a form for update input condition general information
   *         Return Redirect Action when the user is not log in
   *         Return Internal Server Error Action when have mongoDB error
   */
  @ApiOperation(
    nickname = "conditions/form",
    value = "Get the html page with form for update input condition general information",
    notes = "Get the html page with form for update input condition general information",
    httpMethod = "GET")
  @ApiResponses(Array(
    new ApiResponse(code=303,message="Move resource to the login page at /login if the user is not log"),
    new ApiResponse(code=500,message="Have a mongoDB error")
  ))
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name="id",value="Campaign id",required=true,dataType="String",paramType="path")
  ))
  def formGeneralUpdateInput(id:String)=Action.async{
    implicit request =>
      //Verify if user is connect
      UserManager.doIfconnectAsync(request) {

        //Verify if campaign exist
        campaignManager.doIfCampaignFound(BSONObjectID(id)){

          //If campaign found
          campaign=> {

            //Find condition in session
            val cond=findCondition

            //Prepare data for prefilled the form
            val dataForm=ConditionForm((cond\"debut").asOpt[Date].getOrElse(new Date),(cond\"fin").asOpt[Date],(cond\"commentaire").asOpt[String])

            //Display the form
            future{Ok(views.html.campaign.formConditions(form.fill(dataForm),id, printStateForm("infoCondition", campaign.types, id)))}
          }
        }{

          //If campaign not found
          _ => future{Redirect(routes.CampagneManager.listCampaign())}
        }
      }
  }

  /**
   * This method is call when the user is on the page /campaigns/campaign/:id/form/module. It display the module list for associat one to a condition
   * @return Return Ok Action when the user is on the page /campaigns/campaign/:id/form/module with the module list for associat one to a condition
   *         Return Redirect Action when the user is not log in
   *         Return Internal Server Error Action when have mongoDB error
   */
  @ApiOperation(
    nickname = "conditions/form/module",
    value = "Get the html page with the module list for associat one to a condition",
    notes = "Get the html page with the module list for associat one to a condition",
    httpMethod = "GET")
  @ApiResponses(Array(
    new ApiResponse(code=303,message="Move resource to the login page at /login if the user is not log"),
    new ApiResponse(code=500,message="Have a mongoDB error")
  ))
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name="id",value="Campaign id",required=true,dataType="String",paramType="path"),
    new ApiImplicitParam(name="filtre",value="Module type for filter module",required=true,dataType="String",paramType="query")
  ))
  def formModule(id:String,filtre:String="",filtreId:String="")=Action.async{
    implicit request=>
      //Verify if user is connect
      UserManager.doIfconnectAsync(request) {

        //Verify if campaign exist
        campaignManager.doIfCampaignFound(BSONObjectID(id)){

          //If campaign found
          campaign=>

            //Display module inventary
            moduleManager.getInventaryModule(filtre,filtreId) {
              (modules, listType) => Ok(views.html.campaign.listModule(modules, listType, filtre,filtreId,id, printStateForm("module",campaign.types, id)))
            }
        }{
          //If campaign not found
          _ => future{Redirect(routes.CampagneManager.listCampaign())}
        }
      }
  }

  /**
   * This method is call when the user is on the page /campaigns/campaign/:id/form/localisation. It display a form for input localisation
   * @return Return Ok Action when the user is on the page /campaigns/campaign/:id/form/localisation with a form for input localisation
   *         Return Redirect Action when the user is not log in or if campaign not found or if campaign is not correct
   *         Return Internal Server Error Action when have mongoDB error
   */
  @ApiOperation(
    nickname = "conditions/form/localisation",
    value = "Get the html page with the form for input localisation",
    notes = "Get the html page with the form for input localisation",
    httpMethod = "GET")
  @ApiResponses(Array(
    new ApiResponse(code=303,message="<ul><li>Move resource to the login page at /login if the user is not log</li><li>Move resource to the conditions list if campaign not found</li><li>Move resource to the conditions list if campaigns type is not equal to 'Terrain'</li></ul>"),
    new ApiResponse(code=500,message="Have a mongoDB error")
  ))
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name="id",value="Campaign id",required=true,dataType="String",paramType="path")
  ))
  def formLocalisation(id:String)=Action.async{
    implicit request=>
      //Verify if user is connect
      UserManager.doIfconnectAsync(request) {

        //Verify if campaign exist
        campaignManager.doIfCampaignFound(BSONObjectID(id)){

          //If campaign found
          campaign=>

            campaign.types match {

              //If campaign type is equal to 'Terrain', display the form
              case "Terrain" =>{

                //Find condition
                val cond=findCondition

                //Prepare data for prefilled the form
                val formLoc=(cond\"localisation").asOpt[Localisation] match{
                  case None=>localisationForm
                  case Some(loc)=>localisationForm.fill(LocalisationForm(loc.nom,loc.lat,loc.lon,loc.commentaire))
                }

                //Display the form
                future{Ok(views.html.campaign.formLocalisation(formLoc,"",id,printStateForm("localisation",campaign.types, id)))}
              }

              //else redirect to conditions list
              case _ =>future{Redirect(routes.ConditionsManager.listConditions(id))}
            }
        }{
          //If campaign not found
          _ => future{Redirect(routes.CampagneManager.listCampaign())}
        }
      }
  }

  /**
   * This method is call when the user is on the page /campaigns/campaign/:id/form/validate. It display all information condition input
   * @return Return Ok Action when the user is on the page /campaigns/campaign/:id/form/validate with information condition input
   *         Return Redirect Action when the user is not log in or if campaign not found or if campaign is not correct
   *         Return bad request if condition information have an error
   *         Return Internal Server Error Action when have mongoDB error
   */
  @ApiOperation(
    nickname = "conditions/form/validate",
    value = "Get the html page with condition information",
    notes = "Get the html page with condition information",
    httpMethod = "GET")
  @ApiResponses(Array(
    new ApiResponse(code=303,message="<ul><li>Move resource to the login page at /login if the user is not log</li><li>Move resource to the conditions list if campaign not found</li></ul>"),
    new ApiResponse(code=400,message="condition information have an error"),
    new ApiResponse(code=500,message="Have a mongoDB error")
  ))
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name="id",value="Campaign id",required=true,dataType="String",paramType="path")
  ))
  def formValidate(id:String)=Action.async{
    implicit request=>
      //Verify if user is connect
      UserManager.doIfconnectAsync(request) {

        //Verify if campaign exist
        campaignManager.doIfCampaignFound(BSONObjectID(id)) {

          //If campaign found
          campaign =>
            //Verify condition information
            verifyAllData(campaign.types){

                  //If condition information not have error
              (cond,loc,module)=>future{Ok(views.html.campaign.validate(cond,loc,Some(module),"",id,app.path.toString,tempFileBuilder,printStateForm("validate", campaign.types, id)))}
            }{
                  //Id condition information have error
              (error,cond,loc,module)=>future{BadRequest(views.html.campaign.validate(cond,loc,module,error,id,app.path.toString,tempFileBuilder,printStateForm("validate", campaign.types, id)))}
            }
        }{
          //If campaign not found
          _ => future{Redirect(routes.CampagneManager.listCampaign())}
        }
      }
  }

  /**
   * This method is call when the user is on the page /campaigns/campaign/:id/form. It add condition information
   * @return Return Redirect Action when the user is not log in or if condition information are valid
   *         Return Bad request with the form if data received are not valid
   */
  @ApiOperation(
    nickname = "conditions/form",
    value = "Add condition information to the session",
    notes = "Add condition information to the session",
    httpMethod = "POST")
  @ApiResponses(Array(
    new ApiResponse(code=303,message="<ul><li>Move resource to the login page at /login if the user is not log</li><li>Move resource to the form for select module at /campaigns/campaign/:id/form/module</li></ul>"),
    new ApiResponse(code=400,message="Data received are not valid")
  ))
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name="id",value="Campaign id",required=true,dataType="String",paramType="path"),
    new ApiImplicitParam(name="debut",value="Date when begin the condition",required=true,dataType="String",paramType="form"),
    new ApiImplicitParam(name="fin",value="Date when finish the condition",dataType="String",paramType="form"),
    new ApiImplicitParam(name="commentaire",value="Comment about the condition",dataType="String",paramType="form")
  ))
  def addGeneralInformation(id:String)=Action.async{
    implicit request=>
      //Verify if user is connect
      UserManager.doIfconnectAsync(request) {
        //Verify if campaign exist
        campaignManager.doIfCampaignFound(BSONObjectID(id)) {

          //If campaign found
          campaign =>
            form.bindFromRequest.fold(
              //If form contains errors
              formWithErrors => {
                //the form is redisplay with error descriptions
                future{BadRequest(views.html.campaign.formConditions(formWithErrors,id, printStateForm("infoCondition", campaign.types, id)))}
              },
              infoData =>verifyGeneralData(infoData,id,campaign)
            )
        }{
          //If campaign not found
          _ => future{Redirect(routes.CampagneManager.listCampaign())}
        }
      }
  }

  /**
   * This method is call when the user is on the page /campaigns/campaign/:id/form/module. It associat module to a condition
   * @return Return Redirect Action when the user is not log in or if associat module to a condition
   *         Return Bad request with the form if data received are not valid or if module not found
   */
  @ApiOperation(
    nickname = "conditions/form/module",
    value = "Associat a module to a conditions",
    notes = "Associat a module to a conditions",
    httpMethod = "POST")
  @ApiResponses(Array(
    new ApiResponse(code=303,message="<ul><li>Move resource to the login page at /login if the user is not log</li><li>Move resource to the next form page</li></ul>"),
    new ApiResponse(code=400,message="<ul><li>Data received are not valid</li><li>Module not found</li></ul>")
  ))
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name="id",value="Campaign id",required=true,dataType="String",paramType="path"),
    new ApiImplicitParam(name="id",value="Module id",required=true,dataType="String",paramType="form")
  ))
  def addSelectedModule(id:String)=Action.async{
    implicit request=>
      //Verify if user is connect
      UserManager.doIfconnectAsync(request) {
        //Verify if campaign exist
        campaignManager.doIfCampaignFound(BSONObjectID(id)) {

          //If campaign found
          campaign =>

            formSelect.bindFromRequest.fold(
              //If form have error, display the modules list
              formWithErrors =>moduleManager.getInventaryModule("","") {
                (modules, listType) => BadRequest(views.html.campaign.listModule(modules, listType, "","",id, printStateForm("module",campaign.types, id)))
              },
              data =>
                //Find the module
                moduleDao.findOne(Json.obj("_id"->BSONObjectID(data.id),"delete"->false)).flatMap(
                  mod =>mod match{

                    //If module is found redirect to the next page
                    case Some(_)=>future{redirectToNextFormPage(id,campaign.types,findCondition + ("module"->Json.toJson(data.id)))}

                    //If module is not found, display the modules list
                    case None=>moduleManager.getInventaryModule("","") {
                      (modules, listType) => BadRequest(views.html.campaign.listModule(modules, listType, "","",id, printStateForm("module",campaign.types, id)))
                    }
                  }
                )
            )
        }{
          //If campaign not found
          _ => future{Redirect(routes.CampagneManager.listCampaign())}
        }
      }
  }

  /**
   * This method is call when the user is on the page /campaigns/campaign/:id/form/localisation. It add localisation to the ground condition
   * @return Return Redirect Action when the user is not log in or after insert localisation information
   *         Return Bad request with the form if data received are not valid
   */
  @ApiOperation(
    nickname = "conditions/form/localisation",
    value = "Add localisation for ground condition to the session",
    notes = "Add localisation for ground condition to the session",
    httpMethod = "POST")
  @ApiResponses(Array(
    new ApiResponse(code=303,message="<ul><li>Move resource to the login page at /login if the user is not log</li><li>Move resource to the validate page at /campaigns/campaign/:id/form/validate after add localisation to the session</li><li>Move resource to the list of condition at /campaigns/campaign/:id if campaign type is not ground</li></ul>"),
    new ApiResponse(code=400,message="Data received are not valid")
  ))
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name="id",value="Campaign id",required=true,dataType="String",paramType="path"),
    new ApiImplicitParam(name="nom",value="Localisation name",required=true,dataType="String",paramType = "form"),
    new ApiImplicitParam(name="lat",value="Latitude of the localisation",dataType="Float",paramType="form"),
    new ApiImplicitParam(name="lon",value="Longitude of the localisation",dataType="Float",paramType="form"),
    new ApiImplicitParam(name="commentaire",value="Comment for the localisation",dataType="String",paramType="form"),
    new ApiImplicitParam(name="photo[]",value="Picture describe the localisation",dataType="file",paramType="body")
  ))
  def addLocalisation(id:String)=Action.async(parse.multipartFormData) {
    implicit request =>
      //Verify if user is connect
      UserManager.doIfconnectAsync(request.asInstanceOf[Request[AnyContent]]) {
        //Verify if campaign exist
        campaignManager.doIfCampaignFound(BSONObjectID(id)) {

          //If campaign found
          campaign => campaign.types match {

            case "Terrain" => localisationForm.bindFromRequest.fold(
              //If form have error, display the form with error
              formWithErrors => future {BadRequest(views.html.campaign.formLocalisation(formWithErrors, "", id, printStateForm("localisation", campaign.types, id))(request.asInstanceOf[Request[AnyContent]]))},
              data => {
                val oldCond=findCondition(request.asInstanceOf[Request[AnyContent]])

                //Insert image into the temporary directory
                val img = insertImage(request.body.files.toList)

                //Create the localisation
                val loc = (oldCond\"localisation").asOpt[Localisation] match{
                  case None=>Localisation(nom = data.nom, lat = data.lat, lon = data.lon, commentaire = data.commentaire, photo = img, condition = BSONObjectID.generate)
                  case Some(locSession)=>Localisation(nom = data.nom, lat = data.lat, lon = data.lon, commentaire = data.commentaire, photo = img ::: locSession.photo, condition = BSONObjectID.generate)
                }

                val cond=oldCond + ("localisation" -> Json.toJson(loc))

                //Redirect to the next page
                future {Redirect(routes.ConditionsManager.formValidate(id)).withSession(request.asInstanceOf[Request[AnyContent]].session + ("condition" -> Json.stringify(cond)))}
              }
            )
            case _ => future {Redirect(routes.ConditionsManager.listConditions(id))}
          }
        } {
          //If campaign not found
          _ => future {Redirect(routes.CampagneManager.listCampaign())}
        }
      }
  }

  /**
   * This method is call when the user is on the page /campaigns/campaign/:id/form/validate. It verify condition information before insert the condition and the localisation
   * @return Return Redirect Action when the user is not log in or if campaign not found or after insert condition and localisation
   *         Return bad request if condition information have an error
   *         Return Internal Server Error Action when have mongoDB error
   */
  @ApiOperation(
    nickname = "conditions/form/validate",
    value = "Verify condition information before insert condition and localisation",
    notes = "Verify condition information before insert condition and localisation",
    httpMethod = "POST")
  @ApiResponses(Array(
    new ApiResponse(code=303,message="<ul><li>Move resource to the login page at /login if the user is not log</li><li>Move resource to the conditions list if campaign not found</li><li>Move resource to the conditions list after insert condition and localisation</li></ul>"),
    new ApiResponse(code=400,message="condition information have an error"),
    new ApiResponse(code=500,message="Have a mongoDB error")
  ))
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name="id",value="Campaign id",required=true,dataType="String",paramType="path")
  ))
  def validate(id:String)=Action.async{
    implicit request=>
      //Verify if user is connect
      UserManager.doIfconnectAsync(request) {

        //Verify if campaign exist
        campaignManager.doIfCampaignFound(BSONObjectID(id)) {

          //If campaign found
          campaign =>
            //Verify condition information
            verifyAllData(campaign.types){

              //If condition information not have error
              (cond,loc,module)=>{

                //Find the type of form
                (findCondition\"form").asOpt[String] match{

                    //Update the condition
                  case Some("update")=>updateCondition(cond,loc,id)

                    //Insert the condition
                  case _=>insertCondition(campaign,cond,loc)
                }
              }
            }{
              //Id condition information have error
              (error,cond,loc,module)=>future{BadRequest(views.html.campaign.validate(cond,loc,module,error,id,app.path.toString,tempFileBuilder,printStateForm("validate", campaign.types, id)))}
            }
        }{
          //If campaign not found
          _ => future{Redirect(routes.CampagneManager.listCampaign())}
        }
      }
  }

  /**
   * This method is call when the user is on the page /campaigns/campaign/:id/:id2/finish. It finish a condition
   * @return Return Redirect Action when the user is not log in or if campaign not found or after finish a condition
   *         Return Internal Server Error Action when have mongoDB error
   */
  @ApiOperation(
    nickname = "conditions/finish",
    value = "Finish a condition",
    notes = "Finish a condition",
    httpMethod = "GET")
  @ApiResponses(Array(
    new ApiResponse(code=303,message="<ul><li>Move resource to the login page at /login if the user is not log</li><li>Move resource to the conditions list if campaign not found</li><li>Move resource to the conditions list after finish a condition</li></ul>"),
    new ApiResponse(code=500,message="Have a mongoDB error")
  ))
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name="id",value="Campaign id",required=true,dataType="String",paramType="path"),
    new ApiImplicitParam(name="id2",value="Condition id",required=true,dataType="String",paramType="path")
  ))
  def finishCondition(id:String,id2:String)=Action.async{
    implicit request =>
      //Verify if user is connect
      UserManager.doIfconnectAsync(request) {

        //Verify if campaign exist
        campaignManager.doIfCampaignFound(BSONObjectID(id)) {

          //If campaign found
          campaign =>
            //Update end date of the condition
            conditionsDao.updateById(BSONObjectID(id2),Json.obj("$set"->Json.obj("dateFin"->new Date))).map(
              data=>Redirect(routes.ConditionsManager.listConditions(id))
            )
        }{
          //If campaign not found
          _ => future{Redirect(routes.CampagneManager.listCampaign())}
        }
      }
  }

  /**
   * This method is call when the user is on the page /campaigns/campaign/:id/:id2/picture/delete or /campaigns/campaign/:id/form/picture/delete. It suppress a picture for a localisation
   * @return Return Redirect Action when the user is not log in or if campaign not found or after suppress the picture
   *         Return Internal Server Error Action when have mongoDB error
   */
  @ApiOperation(
    nickname = "conditions/picture/delete",
    value = "Suppress a picture for a localisation",
    notes = "Suppress a picture for a localisation",
    httpMethod = "GET")
  @ApiResponses(Array(
    new ApiResponse(code=303,message="<ul><li>Move resource to the login page at /login if the user is not log</li><li>Move resource to the conditions list if campaign not found</li><li>Move resource after suppress a picture</li></ul>"),
    new ApiResponse(code=500,message="Have a mongoDB error")
  ))
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name="id",value="Campaign id",required=true,dataType="String",paramType="path"),
    new ApiImplicitParam(name="id2",value="Condition id",dataType="String",paramType="path"),
    new ApiImplicitParam(name="filename",value="Picture name",dataType="String",paramType="query")
  ))
  def removeImageCondition(id:String,id2:String,filename:String)=Action.async{
    implicit request =>
      //Verify if user is connect
      UserManager.doIfconnectAsync(request) {

        //Verify if campaign exist
        campaignManager.doIfCampaignFound(BSONObjectID(id)) {

          //If campaign found
          campaign => id2.isEmpty match{

              //Remove a temporary picture
            case true=>removeTemporaryImage(id,filename)

              //Remove a definitive picture
            case false=>removeDefinitiveImage(id,id2,filename)
          }
        }{

          //If campaign not found
          _ => future{Redirect(routes.CampagneManager.listCampaign())}
        }
      }
  }

  /**
   * This method insert a condition, and the associat localisation into the database and udpate le campaign for associat the localisation
   * @param campaign The Campaign to update
   * @param cond The condition to insert
   * @param loc The localisation to insert
   * @param request Request received
   * @return Return redirect to the conditions list
   */
  def insertCondition(campaign:Campagne,cond:Condition,loc:Option[Localisation])(implicit request:Request[AnyContent])={
    //Insert the condition
    conditionsDao.insert(cond).flatMap(
      data=> {

        //Update the first use date of sensor and card
        val updateCardSensor=updateFirstUseDate(cond.modules,cond.dateDebut)

        //Update the campaign
        campaignDao.updateById(campaign._id, campaign.copy(conditions = (campaign.conditions :+ cond._id))).flatMap(

          //Move picture and insert the localisation
          data => insertLocalisation(loc, cond).flatMap(
            data=>updateCardSensor.map(
              data => Redirect(routes.ConditionsManager.listConditions(campaign._id.stringify)).withSession(request.session - "condition")
            )
          )

        )
      }
    )
  }

  /**
   * This method update a condition, and the associat localisation into the database
   * @param cond The condition to update
   * @param loc The localisation to update
   * @param id Campaign id
   * @param request Request received
   * @return Return redirect to the conditions list
   */
  def updateCondition(cond:Condition,loc:Option[Localisation],id:String)(implicit request:Request[AnyContent])={

    //Find the condition id
    val condId=BSONObjectID((findCondition\"id").as[String])

    //Update the condition
    conditionsDao.updateById(condId,cond.copy(_id=condId)).flatMap(

      //Move image in the temporary folder to the definitive folder
      data=>updateLocalisation(loc).flatMap(

        //update the first use date of sensor and card
        data=>updateFirstUseDate(cond.modules,cond.dateDebut).map(
          data=>Redirect(routes.ConditionsManager.listConditions(id)).withSession(request.session - "condition")
        )
      )
    )
  }

  /**
   * This method update the first use date of sensors and cards if the first use date not exists or is greater
   * @param id Module id
   * @param date Begin date of the condition
   * @return Return future with the last error
   */
  def updateFirstUseDate(id:BSONObjectID,date:Date)={

    //Find the module
    moduleDao.findOne(Json.obj("_id"->id,"delete"->false)).flatMap(
      moduleOpt=>moduleOpt match {

          //If module not found return an error
        case None =>future{LastError(false,None,None,Some("Module not found"),None,0,false)}

          //If module found
        case Some(module) =>
          //update first use date of sensors
          sensorDao.update(
            selector=Json.obj("_id"->Json.obj("$in"->module.capteurs),"$or"->JsArray(Seq(Json.obj("firstUse"->Json.obj("$gt"->date)),Json.obj("firstUse"->Json.obj("$exists"->false))))),
            update=Json.obj("$set"->Json.obj("firstUse"->date)),
            multi=true
          ).flatMap(

            //update first use date of cards
            data=>cardDao.update(
              selector=Json.obj("_id"->Json.obj("$in"->module.cartes),"$or"->JsArray(Seq(Json.obj("firstUse"->Json.obj("$gt"->date)),Json.obj("firstUse"->Json.obj("$exists"->false))))),
              update=Json.obj("$set"->Json.obj("firstUse"->date)),
              multi=true
            )

          )
      }
    )
  }

  /**
   * This method remove a temporary picture
   * @param id Campaign id
   * @param filename Picture name
   * @param request Request received
   * @return Return redirect to validate form
   */
  def removeTemporaryImage(id:String,filename:String)(implicit request:Request[AnyContent])={
    //Find condition in session
    val cond=findCondition

    //Remove the picture
    val loc=removeImage("tmp/",filename,(cond\"localisation").as[Localisation])

    //Create new condition
    val newCond=cond ++ Json.obj("localisation"->Json.toJson(loc))

    //Redirect to validate form with new condition in session
    future{Redirect(routes.ConditionsManager.formValidate(id)).withSession(request.session + ("condition"->Json.stringify(newCond)))}
  }

  /**
   *  This method remove a definitive picture
   * @param id Campaign id
   * @param id2 Condition id
   * @param filename Picture name
   * @return Return redirect to condition information
   */
  def removeDefinitiveImage(id:String,id2:String,filename:String)={
    //Find the localisation
    localisationDao.findOne(Json.obj("condition"->BSONObjectID(id2))).flatMap(
      locOpt=>locOpt match{

          //If localisation not found redirect
        case None=>future{Redirect(routes.ConditionsManager.moreInformation(id,id2))}

          //If localisation found update the localisation and redirect
        case Some(loc)=>localisationDao.updateById(loc._id,removeImage("",filename,loc)).map(
          data=>Redirect(routes.ConditionsManager.moreInformation(id,id2))
        )
      }
    )
  }

  /**
   * This method remove physicaly a picture
   * @param path Path to the picture
   * @param filename Picture name
   * @param localisation Localisation associat to the picture
   * @return Return a localisation without the picture
   */
  def removeImage(path:String,filename:String,localisation: Localisation):Localisation={
    //Create reference to the picture
    val f=tempFileBuilder.createFile(app.path+"/public/images/campaign/"+path+filename)

    //Verify if the picture exist
    f.exists match{

        //If the picture exist, delete it
      case true=>f.delete match{

          //If the picture is delete, return localisation without the picture
        case true=>localisation.copy(photo=localisation.photo.filter(p=> !p.equals(filename)))

          //If picture is not delete, return the same localisation
        case false=>localisation
      }

        //If the picture not exist, return localisation without the picture
      case false =>localisation.copy(photo=localisation.photo.filter(p=> !p.equals(filename)))
    }
  }

  /**
   * This method move localisation picture and insert localisation into mongoDB if localisation was found
   * @param locOpt Localistion to insert
   * @return
   */
  def insertLocalisation(locOpt:Option[Localisation],cond:Condition): Future[LastError] =locOpt match{

      //If localisation not found
    case None=>future{LastError(true,None,None,None,None,0,false)}

      //If localisation found
    case Some(loc)=>{

      //Move picture
      moveImages(loc.photo)

      //Insert the localisation
      localisationDao.insert(loc.copy(condition=cond._id))
    }
  }

  /**
   * This method move localisation picture and update localisation into mongoDB if localisation was found
   * @param locOpt Localistion to update
   * @return
   */
  def updateLocalisation(locOpt:Option[Localisation])=locOpt match{
    //If localisation not found
    case None=>future{LastError(true,None,None,None,None,0,false)}

    //If localisation found
    case Some(loc)=>{

      //Move picture
      moveImages(loc.photo)

      //Insert the localisation
      localisationDao.updateById(loc._id,loc)
    }
  }

  /**
   * This method move localisation picture into campaign directory
   * @param imgs List of pictures
   */
  def moveImages(imgs:List[String]): Unit =imgs match{

      //If not have picture
    case Nil=>

      //If have picture
    case h::t=>{
      val f=tempFileBuilder.createFile(app.path+"/public/images/campaign/tmp/"+h)

      if(f.exists()) {
        //Get the picture
        val tempFile = tempFileBuilder.createTemporaryFile(f)

        //Move the picture
        tempFile.moveTo(new File(app.path + "/public/images/campaign/" + h))

        //Delete the old picture
        tempFile.clean
      }

      //Move other image
      moveImages(t)
    }
  }

  /**
   * This method find condition in session and parse the string into JsObject
   * @param request
   * @return
   */
  def findCondition(implicit request:Request[AnyContent]):JsObject=request.session.get("condition") match{
    case None=>Json.obj()
    case Some(str)=>Json.parse(str).asInstanceOf[JsObject]
  }

  /**
   * This method all condition information in the session and transform it to Condition and Localisation
   * @param request Request received
   * @return Return the Condition and Localisation create with session data
   */
  def findSessionData(implicit request:Request[AnyContent]):(Condition,Option[Localisation])={
    //Find data in session
    val jsCond=findCondition

    //Transform data to condition
    val cond=toCondition(jsCond)

    //Transform data to localisation
    val loc=toLocalisation(jsCond)

    //Return condition and localisation
    (cond,loc)
  }

  /**
   * This method transform data in session to condition
   * @param jsObj Data in session
   * @return The condition represent by session data
   */
  def toCondition(jsObj: JsObject):Condition={

    //Find the begin date
    val debut=findDateDebut(jsObj)

    //Find the end date
    val fin=findDateFin(jsObj)

    //Find module id
    val module=findModule(jsObj)

    //Create the condition
    Condition(dateDebut = debut,dateFin=fin,commentaire = (jsObj\"commentaire").as[Option[String]],modules=module)
  }

  /**
   * This method transform the begin date in session to a date
   * @param jsObj Data in session
   * @return The begin date
   */
  def findDateDebut(jsObj:JsObject):Date=(jsObj\"debut") match{
      //if begin date is undefined
    case _:JsUndefined => new Date

      //if begin date is defined
    case p =>{

      //Transform the date
      val date=new Date
      date.setTime(p.as[Long])

      //Return the date
      date
    }
  }

  /**
   * This method transform the end date in session to a date
   * @param jsObj Data in session
   * @return The end date
   */
  def findDateFin(jsObj:JsObject):Option[Date]=(jsObj\"fin") match{

        //If end date is equal to null
      case JsNull => None

        //If end date is undefined
      case _:JsUndefined => None

        //If end date is defined
      case p => p.as[Option[Long]] match {

          //If end date is equal to None
        case None => None

          //If end date is equal to some date
        case Some(time) => {

          //Transform the date
          val d = new Date
          d.setTime(time)

          //Return the date
          Some(d)
        }
      }
    }

  /**
   * This method transform the module id in session to an BSONObjectID
   * @param jsObj data in session
   * @return The module BSONObjectID
   */
  def findModule(jsObj:JsObject):BSONObjectID=jsObj\"module" match{
      //If module id is null
    case JsNull => BSONObjectID.generate

      //If module id is undefined
    case _:JsUndefined => BSONObjectID.generate

      //If module id is defined
    case id=>BSONObjectID(id.as[String])
  }

  /**
   * This method transform localisation in session to an Localisation
   * @param jsObj Data in session
   * @return The localisation
   */
  def toLocalisation(jsObj: JsObject):Option[Localisation]=jsObj\"localisation" match{

        //If localisation is null
      case JsNull => None

        //If localisation is undefined
      case _:JsUndefined=>None

        //If localisation is defined
      case p => Some(Localisation.localisationFormat.reads(p).get)
  }

  /**
   * This method verify all condition information
   * @param types Campaign type
   * @param correct Function call if datas are correct
   * @param notCorrect Function call if datas are not correct
   * @param request Request received
   * @return The Result return by function call
   */
  def verifyAllData(types:String)(correct:(Condition,Option[Localisation],Module)=>Future[Result])(notCorrect:(String,Condition,Option[Localisation],Option[Module])=>Future[Result])(implicit request:Request[AnyContent])={

    //Find condition and localisation in session
    val (cond,loc)=findSessionData

    //Find module associat to the condition
    moduleDao.findOne(Json.obj("delete"->false,"_id"->cond.modules)).flatMap(
      data=>data match {

          //If module not found
        case None => notCorrect(Messages("campaign.condition.error.notSelectModule"), cond, loc, None)

          //If module found
        case Some(module) => {

          //Verify condtion dates
          if (verifyDate(cond.dateDebut, cond.dateFin)) {

            //Verify localisation if is a ground campaign
            if (!types.equals("Terrain") || verifyLocalisation(loc)) {

              //Verify if module is not used in other condition at the same time
              if(verifyModuleAvailable(cond.modules,cond.dateDebut,cond.dateFin,(findCondition\"id").asOpt[String])) {
                correct(cond, loc, module)
              }
              else{
                notCorrect(Messages("campaign.condition.error.usedInSameTime"),cond,loc,Some(module))
              }

            }else{
              notCorrect(Messages("campaign.condition.error.notLocalisation"),cond,loc,Some(module))
            }
          }
          else{
            notCorrect(Messages("campaign.condition.error.beginAfterEnd"),cond,loc,Some(module))
          }
        }
      }
    )
  }

  /**
   * This method verify if the module is already used
   * @param id Module id
   * @param debut Begin date of the condition
   * @param fin End date of the condition
   * @return Return true if the module is not used else false
   */
  def verifyModuleAvailable(id:BSONObjectID,debut:Date,fin:Option[Date],idCondition:Option[String]):Boolean={
    //Create part of query for begin date
    val begin=createQueryIntersect("dateDebut",debut,fin)

    //Create part of query for end date
    val end=Json.obj("$or"->JsArray(Seq(createQueryIntersect("dateFin",debut,fin) + ("dateFin"->Json.obj("$exists"->false)))))

    val query=idCondition match{
      case None=>Json.obj("modules"->id,"$or"->JsArray(Seq(begin,end)))
      case Some(idCond)=>Json.obj("_id"->Json.obj("$ne"->BSONObjectID(idCond)),"modules"->id,"$or"->JsArray(Seq(begin,end)))
    }

    //Find condition if module is used
    val res=conditionsDao.findOne(query).map(
      data=>data match{

          //Condition not found
        case None => true

          //Condition found
        case _ => false
      }
    )

    //Return the result
    Await.result(res,Duration.Inf)
  }

  /**
   * This method create part of query for intersect between 2 date
   * @param name Name of the column
   * @param debut Begin date of the condition
   * @param fin End date of the condition
   * @return Part of query for the intersect
   */
  def createQueryIntersect(name:String,debut:Date,fin:Option[Date]):JsObject=fin match{
      //End date not found
    case None=>Json.obj(name->Json.obj("$gte"->debut))

      //End date found
    case Some(d)=>Json.obj(name->Json.obj("$gte"->debut,"$lte"->d))
  }

  /**
   * This method verify if the localisation is found and it name is not empty
   * @param locOpt Option with the localisation
   * @return
   */
  def verifyLocalisation(locOpt:Option[Localisation])=locOpt match{

      //Localisation not found
    case None => false

      //Localisation found verify if the name is not empty
    case Some(loc) => loc.nom.nonEmpty
  }

  /**
   * This method verify if date received are valid.
   * For that, this method verify if begin and end date are correct and if the begin date is before the end date
   * @param infoData Data received
   * @param error Function call if date aren't valid
   * @param valid Function call if date are valid
   * @return
   */
  def verifyDateValid(infoData:ConditionForm)(error:(Form[ConditionForm]=>Future[Result]))(valid:(Date,Option[Date])=>Future[Result])= {
    if (verifyDate(infoData.debut, infoData.fin)) {

      //Call valid function
      valid(infoData.debut, infoData.fin)
    }
    else{

      //Call error function
      error(form.fill(infoData).withGlobalError(Messages("campaign.condition.error.beginAfterEnd")))
    }
  }

  /**
   * Redirect to the next page after associat module to a condition
   * @param id Campaign id
   * @param types Campaign type
   * @param condition List of data in the condition
   * @param request Request received
   * @return
   */
  def redirectToNextFormPage(id:String,types:String,condition:JsObject)(implicit request:Request[AnyContent]):Result= {
    types match {
      case "Terrain" => Redirect(routes.ConditionsManager.formLocalisation(id)).withSession(request.session + ("condition" -> Json.stringify(condition)))
      case _ => Redirect(routes.ConditionsManager.listConditions(id)).withSession(request.session + ("condition" -> Json.stringify(condition)))
    }
  }

  /**
   * Verify data received and put data in session if their are correct
   * @param infoData Data received
   * @param id Campaign id
   * @param campaign The campaign
   * @param request Request received
   * @return
   */
  def verifyGeneralData(infoData:ConditionForm,id:String,campaign:Campagne)(implicit request:Request[AnyContent])={
    //Verify if date received are valid
    verifyDateValid(infoData){

      //If date aren't valid return bad request with the form
      formWithError=>future{BadRequest(views.html.campaign.formConditions(formWithError,id, printStateForm("infoCondition", campaign.types, id)))}
    }{
      //If date are valid
      (debut,fin)=> {
        //Put data in the session
        val cond = findCondition + ("debut" -> Json.toJson(infoData.debut)) + ("fin" -> Json.toJson(infoData.fin)) + ("commentaire" -> Json.toJson(infoData.commentaire))

        //Redirect to the module select
        future{Redirect(routes.ConditionsManager.formModule(id)).withSession(request.session + ("condition" -> Json.stringify(cond)))}
      }
    }
  }

  /**
   * Verify if the begin date is before the end date
   * @param debut Begin date
   * @param fin End date
   * @return Return true if the begin is before the end date
   */
  def verifyDate(debut:Date,fin:Option[Date])=fin match{
    //If not have end date
    case None=>true

    //If have end date
    case Some(d)=>debut.before(d)
  }

  /**
   * Display html code for navigate in the form for create a new conditions
   * @param activ Name of the link activ
   * @param types Condition type
   * @param id Id of the campaign
   * @return Html code for navigate in the form for create a new conditions
   */
  def printStateForm(activ:String,types:String,id:String)=views.html.campaign.stateForm(activ,types,id)

  /**
   * This method insert image in the folder public/images/campaign/tmp
   * @param list Images to insert in the folder
   * @return List of image name
   */
  def insertImage(list:List[MultipartFormData.FilePart[TemporaryFile]]):List[String]=list match{
    //If not have image
    case Nil=>List()

    //If have image
    case h::t=> {
      //Find the image extension
      val extension=h.filename.split("\\.").last

      //Create an uniq name
      val filename = UUID.randomUUID().toString+"."+extension

      //Find the location
      val r=app.path

      //Move the file
      h.ref.moveTo(new File((r+"/public/images/campaign/tmp/"+filename)))

      //Insert other image
      filename::insertImage(t)
    }
  }

  /**
   * This method find localisations associat to conditions for ground campaign
   * @param types Campaign type
   * @param idCond List of conditions id
   * @return Return the list of localisations associat to conditions
   */
  def findLocalisation(types:String,idCond:List[JsValue])=types match{

      //Find localisations if ground campaign
    case "Terrain"=>localisationDao.findAll(Json.obj("condition"->Json.obj("$in"->JsArray(idCond.toSeq))))

      //Return an empty list
    case _=>future{List[Localisation]()}
  }

  /**
   * This method create the mongoDB query for filter conditions
   * @param filtre Conditions filter
   * @param idCond List of conditions id
   * @return Return a mongoDB query
   */
  def createQueryConditionDate(filtre:String,idCond:List[JsValue]):JsObject=filtre match{
      //Query for conditions ongoing
      case "ongoing" =>Json.obj(
        "_id" -> Json.obj("$in" -> JsArray(idCond.toSeq)),
        "$or"->JsArray(Seq(
          Json.obj("dateFin"->Json.obj("$exists"->false)),
          Json.obj("dateDebut"->Json.obj("$lt"->new Date),"dateFin"->Json.obj("$gt"->new Date))
        ))
      )

      //Query for conditions finish
      case "finish" => Json.obj("_id" -> Json.obj("$in" -> JsArray(idCond.toSeq)),"dateFin"->Json.obj("$lt"->new Date))

      //Query for all conditions
      case _=>Json.obj("_id" -> Json.obj("$in" -> JsArray(idCond.toSeq)))
    }
}

@Api(value = "/conditions", description = "Operations for conditions")
object ConditionsManager extends ConditionsManagerLike