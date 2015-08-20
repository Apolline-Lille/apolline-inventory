package controllers

import com.wordnik.swagger.annotations._
import dispatch.{Req, url, Http}
import models._
import org.apache.http.HttpResponse
import play.api.Play
import play.api.data.Form
import play.api.data.Forms._
import play.api.i18n.Messages
import play.api.libs.json.{JsObject, Json}
import play.api.libs.ws.WS
import play.api.mvc._
import reactivemongo.bson.BSONObjectID
import reactivemongo.extensions.BSONFormats.BSONObjectIDFormat
import scala.concurrent._

import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.Play.current

/**
 * This class is used when user submit form for campaign
 * @param nom Name of the campaign
 * @param send Value of the button press
 */
case class CampaignForm(nom:String,types:String,send:Option[String])

/**
 * This object is a controller for manage all campaign
 */
trait CampagneManagerLike extends Controller{

  /************* Property *********************/

  /**
   * DAO for campaign
   */
  val campaignDao:CampagneDao=CampagneDaoObj

  val app=Play.application

  val http:Http=Http

  val urlVal:(String=>Req)=url

  val form=Form[CampaignForm](
    mapping(
      "nom"->nonEmptyText,
      "types"->nonEmptyText,
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
              val campForm=CampaignForm(campaign.nom,campaign.types,None)
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
            case _=>login(campaignData.nom).flatMap(
              data => data match{
                case (dataToken,collectUrl,version)=>
                  campaignDao.insert(Campagne(nom = campaignData.nom,types=campaignData.types,dataToken=dataToken,collectUrl=collectUrl,version=version, conditions = List())).map(
                    data => Redirect(routes.CampagneManager.listCampaign())
                  ).recover({ case _ => InternalServerError("error") })
              }
            )
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

  /**
   * Verify if card type found and execute a function if campaign found or not
   * @param id Campaign id
   * @param found Function executed if campaign found
   * @param notFound Function executed if campaign not found
   * @return Return the result of executed function
   */
  def doIfCampaignFound(id:BSONObjectID)(found:Campagne=>Future[Result])(notFound:Unit=>Future[Result]):Future[Result]={
    //Find the card type
    campaignDao.findOne(Json.obj("_id"->id,"delete"->false)).flatMap(
      campaignOpt => campaignOpt match{

        //If the card type not found execute function not found
        case None => notFound()

        //If the card type found execute function found
        case Some(camp) => found(camp)
      }
    ).recover({
      //Send Internal Server Error if have mongoDB error
      case _=> InternalServerError("error")
    })
  }

  /**
   * Log the user on the apisense website for create and subscrib a crop associat to a campaign
   * @param name Name of the campaign
   * @return The dataToken, the collect url and the version number of the crop
   */
  def login(name:String):Future[(String,String,Int)]={
    //Get the page for log the user
    http(urlVal("http://apisense.io/login").GET).flatMap(
      resp=>{
        //Get the cookie header
        val header=resp.getHeader("Set-Cookie")

        //Find the end of token
        val endToken=header.lastIndexOf("\"")

        //Extract the token
        val token=header.substring(header.indexOf("csrfToken")+10,endToken)

        val login=app.configuration.getString("apisense.login").getOrElse("")

        val password=app.configuration.getString("apisense.password").getOrElse("")

        //Log the user on apisense
        http(urlVal("http://apisense.io/login").POST.addHeader("Cookie",header.substring(0,endToken+2)) << Map("username"->login,"password"->password,"csrfToken"->token)).flatMap(
         resp2=>

           //Create the crops associat to a campaign
           createCrops(name, resp2.getHeader("Set-Cookie"))
        )
      }
    )
  }

  /**
   * Create a crops and subscrib to a crops on apisense
   * @param name Name of the campaign
   * @param header Cookie header of the login return
   * @return The dataToken, the collect url and the version number of the crop
   */
  def createCrops(name:String,header:String): Future[(String,String,Int)] ={
    //Get the page for create a crop
    http(urlVal("http://apisense.io/new/crop").GET.addHeader("Cookie",header.substring(0,header.lastIndexOf("\"") +2))).flatMap(
      resp=>{

        //Get the cookie header
        val header=resp.getHeader("Set-Cookie")

        //Find the end of the token
        val endToken=header.lastIndexOf("\"")

        //Get the token
        val token=header.substring(header.indexOf("csrfToken")+10,endToken)

        //create the crops
        http(urlVal("http://apisense.io/new/crop").POST.addHeader("Cookie",header.substring(0,endToken+2)) << Map("name"->name,"description"->"","visibility"->"PRIVATE","csrfToken"->token)).flatMap(
          resp2=> {

            //Get cookie header
            val header=resp2.getHeader("Set-Cookie")

            //Find end of the token
            val endToken=header.lastIndexOf("\"")

            //Deploy the new crops
            http(urlVal("http://apisense.io" + resp2.getHeader("Location")).PUT.addHeader("Cookie",header.substring(0,endToken+2)).setContentType("application/json", "UTF-8") << """{"command":"deploy","script":""}""").flatMap(
              resp3 =>
                //Bee subscrib to the crops
                subscribeCrops(name)
            )
          }
        )
      }
    )
  }

  /**
   * Subscrib to the crop
   * @param name Name of the campaign
   * @return The dataToken, the collect url and the version number of the crop
   */
  def subscribeCrops(name:String):Future[(String,String,Int)]={
    //Encode to base64 the name of the campaign
    val nameEncode=new sun.misc.BASE64Encoder().encode(name.getBytes)

    //Find the authorization header
    val authorization="Bearer "+app.configuration.getString("apisense.token").getOrElse("")

    //Find the crops access header
    val cropsAccess=app.configuration.getString("apisense.cropAccess").getOrElse("")

    //Create the url for subscrib to the crops
    val urlStr="http://hive.apisense.io/v1/crops/"+app.configuration.getString("apisense.owner").getOrElse("")+"/"+nameEncode

    //Subscrib to the crop
    http(urlVal(urlStr).POST.addHeader("CropAccessToken",cropsAccess).addHeader("Authorization",authorization)).flatMap(
      resp=>{

        //Find the dataToken of the crop
        val dataToken=(Json.parse(resp.getResponseBody) \ "dataToken").as[String]

        //Find information of the crop
        http(urlVal(urlStr+"/"+dataToken).GET.addHeader("Authorization",authorization)).map(
          resp2=>{

            //Find the response
            val respJson=Json.parse(resp2.getResponseBody)

            //Get the collectUrl
            val collectUrl=(respJson \ "collectUrl").as[String]

            //get the version number
            val version=(respJson \ "version").as[Int]

            //Return informations of the collect
            (dataToken,collectUrl,version)
          }
        )
      }
    )
  }
}

@Api(value = "/campaign", description = "Operations for campaign")
object CampagneManager extends CampagneManagerLike