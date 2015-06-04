package controllers

import com.wordnik.swagger.annotations._
import models._
import play.api.data.Form
import play.api.data.Forms._
import play.api.i18n.Messages
import play.api.libs.json.{JsObject, Json}
import play.api.mvc._
import reactivemongo.bson.BSONObjectID
import reactivemongo.extensions.BSONFormats.BSONObjectIDFormat
import scala.concurrent._

import play.api.libs.concurrent.Execution.Implicits.defaultContext

/**
 * This class is used when user submit form for campaign
 * @param nom Name of the campaign
 * @param send Value of the button press
 */
case class CampaignForm(nom:String,send:Option[String])

/**
 * This object is a controller for manage all campaign
 */
trait CampagneManagerLike extends Controller{

  /************* Property *********************/

  /**
   * DAO for campaign
   */
  val campaignDao:CampagneDao=CampagneDaoObj

  val form=Form[CampaignForm](
    mapping(
      "nom"->nonEmptyText,
      "send"->optional(text)
    )(CampaignForm.apply)(CampaignForm.unapply)
  )

  /****************** Route methods ***********/

  /**
   * This method is call when the user is on the page /campaigns. It list campaign
   * @return Return Ok Action when the user is on the page /campaigns with the list of campaign
   *         Return Redirect Action when the user is not log in
   *         Return Internal Server Error Action when have mongoDB error
   */
  @ApiOperation(
    nickname = "campaigns",
    value = "Get the html page for list campaign",
    notes = "Get the html page for list campaign",
    httpMethod = "GET")
  @ApiResponses(Array(
    new ApiResponse(code=303,message="Move resource to the login page at /login if the user is not log"),
    new ApiResponse(code=500,message="Have a mongoDB error")
  ))
  def listCampaign()=Action.async{
    implicit request =>
      //Verify if user is connect
      UserManager.doIfconnectAsync(request) {

        //Find campaigns
        campaignDao.findAll(Json.obj("delete"->false)).map(
          campaigns=>Ok(views.html.campaign.listCampaign(campaigns))
        )
      }
  }

  /**
   * This method is call when the user is on the page /campaigns/campaign. It display a form for add new campaign
   * @return Return Ok Action when the user is on the page /campaigns/campaign with a form for add new campaign
   *         Return Redirect Action when the user is not log in
   */
  @ApiOperation(
    nickname = "campaigns/campaign",
    value = "Get the html page with a form for add new campaign",
    notes = "Get the html page with a form for add new campaign",
    httpMethod = "GET")
  @ApiResponses(Array(
    new ApiResponse(code=303,message="Move resource to the login page at /login if the user is not log")
  ))
  def addCampaign()=Action{
    implicit request=>
      //Verify if user is connect
      UserManager.doIfconnect(request){
        //Print the form
        Ok(views.html.campaign.formCampaign(form,routes.CampagneManager.insertCampaign()))
      }
  }

  /**
   * This method is call when the user is on the page /campaigns/campaign/:id/update. It display a form for update a campaign
   * @return Return Ok Action when the user is on the page /campaigns/campaign/:id/update with a form for update campaign
   *         Return Redirect Action when the user is not log in
   *         Return Redirect if campaign not found
   *         Return Internal server error when have mongoDB error
   */
  @ApiOperation(
    nickname = "campaigns/campaign/:id/update",
    value = "Get the html page with a form for update a campaign",
    notes = "Get the html page with a form for update a campaign",
    httpMethod = "GET")
  @ApiResponses(Array(
    new ApiResponse(code=303,message="<ul><li>Move resource to the login page at /login if the user is not log</li><li>Move resource to the list of campaign at /campaigns</li></ul>"),
    new ApiResponse(code=500,message="Have a mongoDB error")
  ))
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name="id",value="Campaign id",required=true,dataType="String",paramType="path")
  ))
  def updateCampaignPage(id:String)=Action.async{
    implicit request=>
      //Verify if user is connect
      UserManager.doIfconnectAsync(request){

        //Find campaign
        campaignDao.findOne(Json.obj("_id"->BSONObjectID(id),"delete"->false)).map(
          data=>data match{

            //If campaign not found
            case None => Redirect(routes.CampagneManager.listCampaign())

            //If campaign found
            case Some(campaign) => {
              //Prepare form data
              val campForm=CampaignForm(campaign.nom,None)
              //Print the form
              Ok(views.html.campaign.formCampaign(form.fill(campForm),routes.CampagneManager.updateCampaign(id)))
            }
          }
        ).recover({case _ => InternalServerError("error")})
      }
  }

  /**
   * This method is call when the user is on the page /campaigns/campaign. It insert a new campaign into the database
   * @return Return Redirect Action when the user is not log in
   *         Return Redirect Action after insert or reactivat campaign
   *         Return Bad request if data received are not valid
   *         Return Internal Server Error Action when have mongoDB error
   */
  @ApiOperation(
    nickname = "campaigns/campaign/insert",
    value = "Insert a new campaign into the database",
    notes = "Insert a new campaign into the database",
    httpMethod = "POST")
  @ApiResponses(Array(
    new ApiResponse(code=303,message="<ul><li>Move resource to the login page at /login if the user is not log</li><li>Move resource to /campaigns after insert or reactivat campaign</li></ul>"),
    new ApiResponse(code=400,message="Data received are not valid"),
    new ApiResponse(code=500,message="Have a mongoDB error")
  ))
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name="nom",value="Name of the campaign",required=true,dataType="String",paramType="form")
  ))
  def insertCampaign()=Action.async{
    implicit request=>
      val msg=Messages("campaign.list.error.campaignExist")+" <input type=\"submit\" class=\"btn btn-danger\" name=\"send\" value=\""+Messages("global.reactiver")+"\"/> <input type=\"submit\" class=\"btn btn-danger\" name=\"send\" value=\""+Messages("global.ignorer")+"\"/>"

      //Verify data received
      submitForm(msg,routes.CampagneManager.insertCampaign()) {
        campaignData=>Json.obj("nom" -> campaignData.nom)
      }{
        //If data are valid
        campaignData=>

          (campaignData.send) match{
            //If user press on button reactivat
            case Some("Réactiver") => {

              //Create result if campaign not found
              val notFound=BadRequest(views.html.campaign.formCampaign(form.withGlobalError(Messages("campaign.list.campaignExist")),routes.CampagneManager.insertCampaign()))

              //Reactivat the campaign
              updateWithColumnDelete(Json.obj("nom"->campaignData.nom),false,notFound,Redirect(routes.CampagneManager.listCampaign()))
            }

            //If user press on a other insert the campaign
            case _=>campaignDao.insert(Campagne(nom = campaignData.nom, conditions = List())).map(
              data => Redirect(routes.CampagneManager.listCampaign())
            ).recover({ case _ => InternalServerError("error") })
          }
      }
  }

  /**
   * This method is call when the user is on the page /campaigns/campaign/:id/update. It update a campaign into the database
   * @return Return Redirect Action when the user is not log in
   *         Return Redirect Action after update campaign
   *         Return Bad request if data received are not valid
   *         Return Internal Server Error Action when have mongoDB error
   */
  @ApiOperation(
    nickname = "campaigns/campaign/update",
    value = "Update a campaign into the database",
    notes = "Update a campaign into the database",
    httpMethod = "POST")
  @ApiResponses(Array(
    new ApiResponse(code=303,message="<ul><li>Move resource to the login page at /login if the user is not log</li><li>Move resource to /campaigns after update campaign</li></ul>"),
    new ApiResponse(code=400,message="Data received are not valid"),
    new ApiResponse(code=500,message="Have a mongoDB error")
  ))
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name="id",value="Campaign id",required=true,dataType="String",paramType="path"),
    new ApiImplicitParam(name="nom",value="Name of the campaign",required=true,dataType="String",paramType="form")
  ))
  def updateCampaign(id:String)=Action.async{
    implicit request=>
      val msg=Messages("campaign.list.error.campaignExist")+" <input type=\"submit\" class=\"btn btn-danger\" name=\"send\" value=\""+Messages("global.ignorer")+"\"/>"

      //Verify data received
      submitForm(msg,routes.CampagneManager.updateCampaign(id)) {
        campaignData=>Json.obj("nom" -> campaignData.nom,"_id"->Json.obj("$ne"->BSONObjectID(id)))
      }{
        //If data are valid
        campaignData=>
          //Find campaign
          campaignDao.findOne(Json.obj("_id"->BSONObjectID(id),"delete"->false)).flatMap(
            campOpt=>campOpt match{
              //If campaign not found
              case None =>future{Redirect(routes.CampagneManager.listCampaign())}

              //If campaign found, update her
              case Some(camp)=>campaignDao.updateById(BSONObjectID(id),camp.copy(nom = campaignData.nom)).map(
                data => Redirect(routes.CampagneManager.listCampaign())
              ).recover({ case _ => InternalServerError("error") })
            }
          ).recover({case _ => InternalServerError("error")})
      }
  }

  /**
   * This method verify data received when user submit a form.
   * @param route Route used for submit the form
   * @param f Function used if data are valid
   * @param request
   * @return
   */
  def submitForm(errorMessage:String,route:Call)(verif:CampaignForm=>JsObject)(f:CampaignForm=>Future[Result])(implicit request:Request[AnyContent])={
    //Verify if user is connect
    UserManager.doIfconnectAsync(request){
      form.bindFromRequest.fold(

        //If form contains errors
        formWithErrors => {
          //the form is redisplay with error descriptions
          future{BadRequest(views.html.campaign.formCampaign(formWithErrors,route))}
        },

        // Else if form no contains errors
        campaignData => {

          //Find campaign
          campaignDao.findAll(verif(campaignData)).flatMap(
            data => (data,campaignData.send) match {

              //If campaign not found execute function
              case (Nil,_) => f(campaignData)

              //If campaign found but delete and user press button "Réactiver" or "Ignorer"
              case (_,Some("Réactiver")) | (_,Some("Ignorer")) if data.filter(p=> !p.delete).isEmpty => f(campaignData)

              //If campaign found but delete
              case (_,_) if data.filter(p=> !p.delete).isEmpty => future{BadRequest(views.html.campaign.formCampaign(form.withGlobalError(errorMessage), route))}

              //In other case
              case (_,_) => future{BadRequest(views.html.campaign.formCampaign(form.withGlobalError(Messages("campaign.list.error.campaignExist")), route))}
            }
          )
        }
      )
    }
  }

  /**
   * This method get a campaign and update her delete column
   * @param selector Query for select the campaign
   * @param delete Value to set on the delete column
   * @param routeNotFound Result used if campaign not found
   * @param route Result used if campaign found and update
   * @return
   */
  def updateWithColumnDelete(selector:JsObject,delete:Boolean,routeNotFound:Result,route:Result)={
    campaignDao.findOne(selector).flatMap(
      campaignData => campaignData match{
        case None => future{routeNotFound}
        case Some(campaign) => campaignDao.updateById(campaign._id,campaign.copy(delete=delete)).map(
          data => route
        ).recover({case _ => InternalServerError("error")})
      }
    ).recover({case _ => InternalServerError("error")})
  }
}

@Api(value = "/campaign", description = "Operations for campaign")
object CampagneManager extends CampagneManagerLike