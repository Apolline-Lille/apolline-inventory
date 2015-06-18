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
import play.api.libs.json.{JsObject, Json}
import play.api.mvc._
import reactivemongo.bson.BSONObjectID
import reactivemongo.extensions.BSONFormats.BSONObjectIDFormat
import scala.concurrent._
import play.api.data.format.Formats._

import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.Play.current

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

  val moduleManager:ModuleManagerLike=ModuleManager

  val campaignManager:CampagneManagerLike=CampagneManager

  val app=Play.application

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
  def listConditions(id:String)=Action.async{
    implicit request =>
      //Verify if user is connect
      UserManager.doIfconnectAsync(request) {

        //Verify if campaign exist
        campaignManager.doIfCampaignFound(BSONObjectID(id)){

          //If campaign found
          campaign=>future{Ok(views.html.campaign.listCondition(campaign))}
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
            val session=request.session + ("condition"->Json.stringify(Json.obj()))
            future{Ok(views.html.campaign.formConditions(form.bind(Map("debut"->new SimpleDateFormat("dd/MM/yyyy HH:mm:ss").format(new Date))),id, printStateForm("infoCondition", campaign.types, id))).withSession(session)}
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
  def formModule(id:String,filtre:String)=Action.async{
    implicit request=>
      //Verify if user is connect
      UserManager.doIfconnectAsync(request) {

        //Verify if campaign exist
        campaignManager.doIfCampaignFound(BSONObjectID(id)){

          //If campaign found
          campaign=>

            //Display module inventary
            moduleManager.getInventaryModule(filtre) {
              (modules, listType) => Ok(views.html.campaign.listModule(modules, listType, filtre,id, printStateForm("module",campaign.types, id)))
            }
        }{
          //If campaign not found
          _ => future{Redirect(routes.CampagneManager.listCampaign())}
        }
      }
  }

  /**
   * This method is call when the user is on the page /campaigns/campaign/:id/form/localisation. It display the localisation list for associat to a condition
   * @return Return Ok Action when the user is on the page /campaigns/campaign/:id/form/localisation with the localisation list for associat to a condition
   *         Return Redirect Action when the user is not log in or if campaign not found or if campaign is not correct
   *         Return Internal Server Error Action when have mongoDB error
   */
  @ApiOperation(
    nickname = "conditions/form/localisation",
    value = "Get the html page with the localisation list for associat to a condition",
    notes = "Get the html page with the localisation list for associat to a condition",
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
              case "Terrain" =>future{Ok(views.html.campaign.formLocalisation(localisationForm,"",id,printStateForm("localisation",campaign.types, id)))}

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
              formWithErrors =>moduleManager.getInventaryModule("") {
                (modules, listType) => BadRequest(views.html.campaign.listModule(modules, listType, "",id, printStateForm("module",campaign.types, id)))
              },
              data =>
                //Find the module
                moduleDao.findOne(Json.obj("_id"->BSONObjectID(data.id),"delete"->false)).flatMap(
                  mod =>mod match{

                    //If module is found redirect to the next page
                    case Some(_)=>future{redirectToNextFormPage(id,campaign.types,findCondition + ("module"->Json.toJson(data.id)))}

                    //If module is not found, display the modules list
                    case None=>moduleManager.getInventaryModule("") {
                      (modules, listType) => BadRequest(views.html.campaign.listModule(modules, listType, "",id, printStateForm("module",campaign.types, id)))
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

                //Insert image into the temporary directory
                val img = insertImage(request.body.files.toList)

                //Create the localisation
                val loc = Localisation(nom = data.nom, lat = data.lat, lon = data.lon, commentaire = data.commentaire, photo = img, condition = BSONObjectID.generate)

                val cond=findCondition(request.asInstanceOf[Request[AnyContent]]) + ("localisation" -> Json.toJson(loc))
                //Redirect to the next page
                future {Redirect(routes.ConditionsManager.listConditions(id)).withSession(request.session + "condition" -> Json.stringify(cond))}
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
   * This method find condition in session and parse the string into JsObject
   * @param request
   * @return
   */
  def findCondition(implicit request:Request[AnyContent]):JsObject=request.session.get("condition") match{
    case None=>Json.obj()
    case Some(str)=>Json.parse(str).asInstanceOf[JsObject]
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
  def redirectToNextFormPage(id:String,types:String,condition:JsObject)(implicit request:Request[AnyContent]):Result=types match{
    case "Terrain" =>Redirect(routes.ConditionsManager.formLocalisation(id)).withSession(request.session + ("condition" -> Json.stringify(condition)))
    case _ =>Redirect(routes.ConditionsManager.listConditions(id)).withSession(request.session + ("condition" -> Json.stringify(condition)))
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
}

@Api(value = "/conditions", description = "Operations for conditions")
object ConditionsManager extends ConditionsManagerLike