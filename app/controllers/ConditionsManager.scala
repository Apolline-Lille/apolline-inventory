package controllers

import java.lang.annotation.Annotation

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
 * This object is a controller for manage all condition
 */
trait ConditionsManagerLike extends Controller{

  /************* Property *********************/

  /**
   * DAO for conditions
   */
  val conditionsDao:ConditionDao=ConditionDaoObj

  val campaignDao:CampagneDao=CampagneDaoObj

  /****************** Route methods ***********/

  /**
   * This method is call when the user is on the page /campaigns/conditions. It list condtions for a campaign
   * @return Return Ok Action when the user is on the page /campaigns/conditions with the list of conditions for a campaign
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
        campaignDao.findById(BSONObjectID(id)).map(
          data=> data match{
            case None=>Redirect(routes.CampagneManager.listCampaign())
            case Some(campaign)=>Ok(views.html.campaign.listCondition(campaign))
          }
        )
      }
  }
}

@Api(value = "/conditions", description = "Operations for conditions")
object ConditionsManager extends ConditionsManagerLike