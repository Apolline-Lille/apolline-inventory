package controllers

import java.lang.annotation.Annotation
import java.util.Date

import com.wordnik.swagger.annotations._
import models._
import play.api.data.Form
import play.api.data.Forms._
import play.api.data.validation.Constraints._
import play.api.i18n.Messages
import play.api.libs.json.{JsObject, Json}
import play.api.mvc._
import reactivemongo.bson.BSONObjectID
import reactivemongo.extensions.BSONFormats.BSONObjectIDFormat
import scala.concurrent._

import play.api.libs.concurrent.Execution.Implicits.defaultContext

case class ConditionForm(debut:String,fin:Option[String],commentaire:Option[String])

/**
 * This object is a controller for manage all condition
 */
trait ConditionsManagerLike extends Controller{

  /************* Property *********************/

  /**
   * DAO for conditions
   */
  val conditionsDao:ConditionDao=ConditionDaoObj

  val moduleManager:ModuleManagerLike=ModuleManager

  val campaignManager:CampagneManagerLike=CampagneManager

  val form=Form[ConditionForm](
    mapping(
      "debut"->nonEmptyText.verifying(pattern("\\d{2}/\\d{2}/\\d{4} \\d{2}:\\d{2}:\\d{2}".r)),
      "fin"->optional(text.verifying(pattern("\\d{2}/\\d{2}/\\d{4} \\d{2}:\\d{2}:\\d{2}".r))),
      "commentaire"->optional(text)
    )(ConditionForm.apply)(ConditionForm.unapply)
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
          campaign=>future{Ok(views.html.campaign.formConditions(form,printStateForm("infoCondition",campaign.types,id)))}
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
   * Display html code for navigate in the form for create a new conditions
   * @param activ Name of the link activ
   * @param types Condition type
   * @param id Id of the campaign
   * @return Html code for navigate in the form for create a new conditions
   */
  def printStateForm(activ:String,types:String,id:String)=views.html.campaign.stateForm(activ,types,id)
}

@Api(value = "/conditions", description = "Operations for conditions")
object ConditionsManager extends ConditionsManagerLike