package controllers

import java.util.Date

import com.wordnik.swagger.annotations._
import models._
import play.api.data.Form
import play.api.data.Forms._
import play.api.i18n.Messages
import play.api.libs.json.{Json, JsObject}
import play.api.mvc._
import play.modules.reactivemongo.json.BSONFormats
import reactivemongo.bson.BSONObjectID
import scala.concurrent._

import play.api.libs.concurrent.Execution.Implicits.defaultContext

case class CardsForm(
   id:String,
   acquisition:Date,
   firstUse:Option[Date],
   agregateur:Boolean,
   apolline:Option[String],
   firmware:String,
   versionFirmware:String,
   hs:Boolean,
   commentaire:Option[String],
   send:String
)

trait CardsManagerLike extends Controller{

  val typeCardsDao:TypeCardsDao=TypeCardsDaoObj
  val cardDao:CardsDao=CardsDaoObj
  val firmwareDao:FirmwareDao=FirmwareDaoObj

  val typeCardsManager:TypeCardsManagerLike=TypeCardsManager

  val form=Form[CardsForm](
    mapping(
      "id"->nonEmptyText,
      "acquisition"->date,
      "firstUse"->optional(date),
      "agregateur"->boolean,
      "apolline"->optional(text),
      "firmware"->nonEmptyText,
      "versionFirmware"->nonEmptyText,
      "hs"->boolean,
      "commentaire"->optional(text),
      "send"->text
    )(CardsForm.apply)(CardsForm.unapply)
  )

  /**
   * This method is call when the user is on the page /inventary/cards/:id. It list sensors available for a particular type
   * @return Return Ok Action when the user is on the page /inventary/sensors/:id with the list of sensors
   *         Return Redirect Action when the user is not log in
   *         Return Internal Server Error Action when have mongoDB error
   */
  @ApiOperation(
    nickname = "inventary",
    value = "Get the html page for list cards",
    notes = "Get the html page for list cards",
    httpMethod = "GET")
  @ApiResponses(Array(
    new ApiResponse(code=303,message="Move resource to the login page at /login if the user is not log"),
    new ApiResponse(code=500,message="Have a mongoDB error")
  ))
  @ApiImplicitParams(Array(
    new ApiImplicitParam(value = "Id of the card type for list cards associated",required=true,name="id", dataType = "String", paramType = "path")
  ))
  def inventary(id:String,sort:String="id",sens:Int=1)=Action.async{
    implicit request =>
      //Verify if user is connect
      UserManager.doIfconnectAsync(request) {
        val futureCards=cardDao.findAll(Json.obj("delete"->false,"types"->BSONFormats.BSONObjectIDFormat.writes(BSONObjectID(id))),Json.obj(sort->sens))
        val futureFirmware=firmwareDao.findAll()
        //Find the card type
        typeCardsDao.findById(BSONObjectID(id)).flatMap(
          data=> data match{

              //If card type not found
            case None => future{Redirect(routes.TypeCardsManager.inventary())}

              //If card type found
            case Some(typeCards) => {
              futureCards.flatMap(listCards=>
                futureFirmware.map(firmware=>
                  Ok(views.html.cards.listCards(typeCards,listCards,firmware,sort,sens))
                ).recover({case _=>InternalServerError("error")})
              ).recover({case _=>InternalServerError("error")})

            }
          }
        ).recover({case _=>InternalServerError("error")})
      }
  }

  /**
   * This method is call when the user is on the page /inventary/cards/:id/card. It display a form for add new card
   * @return Return Ok Action when the user is on the page /inventary/cards/:id/card with the form for add new card
   *         Return Redirect Action when the user is not log in
   */
  @ApiOperation(
    nickname = "inventary/card/insert",
    value = "Get the html page a form for add new card",
    notes = "Get the html page a form for add new card",
    httpMethod = "GET")
  @ApiResponses(Array(
    new ApiResponse(code=303,message="Move resource to the login page at /login if the user is not log")
  ))
  @ApiImplicitParams(Array(
    new ApiImplicitParam(value = "Id of the card type for list cards associated",required=true,name="id", dataType = "String", paramType = "path")
  ))
  def cardPage(id:String)=Action.async{
    implicit request =>
      //Verify if user is connect
      UserManager.doIfconnectAsync(request) {
        //Verify if card type found
        typeCardsManager.doIfTypeCardsFound(BSONObjectID(id)) {_=>
          //Print an empty form for add new card
          printForm(Results.Ok,id,form,routes.CardsManager.cardInsert(id))
        }{_=>
          //Print an empty form with error type not found
          printForm(Results.BadRequest,id,form.withGlobalError(Messages("inventary.typeCards.error.typeNotExist")),routes.SensorManager.sensorInsert(id))
        }
      }
  }

  /**
   * This method is call when the user is on the page /inventary/cards/:id/:id2. It display a form for update a card
   * @return Return Ok Action when the user is on the page /inventary/cards/:id/:id2 with the form for update a card
   *         Return Redirect Action when the user is not log in or if card not found
   *         Return Internal Server Error Action when have mongoDB error
   */
  @ApiOperation(
    nickname = "inventary/card/update",
    value = "Get the html page a form for update a card",
    notes = "Get the html page a form for update a card",
    httpMethod = "GET")
  @ApiResponses(Array(
    new ApiResponse(code=303,message="<ul><li>Move resource to the login page at /login if the user is not log</li><li>Move resource to the sensors inventary at /inventary/cards/:id if card not found</li></ul>"),
    new ApiResponse(code=500,message="Have a mongoDB error")
  ))
  @ApiImplicitParams(Array(
    new ApiImplicitParam(value = "Id of the card type",required=true,name="id", dataType = "String", paramType = "path"),
    new ApiImplicitParam(value = "Id of the card",required=true,name="id2", dataType = "String", paramType = "path")
  ))
  def cardUpdatePage(id:String,id2:String)=Action.async{
    implicit request =>
      //If user is connect print a form with prefilled data
      printFormWithData(id,id2,routes.CardsManager.cardUpdate(id,id2)){
        (card,firmware)=>
          //Data prefilled into the form
          CardsForm(
            card.id,
            card.acquisition,
            card.firstUse,
            card.agregateur,
            card.apolline,
            firmware.nom,
            firmware.version,
            card.hs,
            card.commentaire,
            ""
          )
      }
  }

  /**
   * This method is call when the user is on the page /inventary/cards/:id/:id2/clone. It display a prefilled form for insert a new card
   * @return Return Ok Action when the user is on the page /inventary/cards/:id/:id2/clone with the prefilled form for insert a new card
   *         Return Redirect Action when the user is not log in or if card not found
   *         Return Internal Server Error Action when have mongoDB error
   */
  @ApiOperation(
    nickname = "inventary/card/clone",
    value = "Get the html page a prefilled form for insert a new card",
    notes = "Get the html page a prefilled form for insert a new card",
    httpMethod = "GET")
  @ApiResponses(Array(
    new ApiResponse(code=303,message="<ul><li>Move resource to the login page at /login if the user is not log</li><li>Move resource to the sensors inventary at /inventary/cards/:id if card not found</li></ul>"),
    new ApiResponse(code=500,message="Have a mongoDB error")
  ))
  @ApiImplicitParams(Array(
    new ApiImplicitParam(value = "Id of the card type",required=true,name="id", dataType = "String", paramType = "path"),
    new ApiImplicitParam(value = "Id of the card",required=true,name="id2", dataType = "String", paramType = "path")
  ))
  def cardClonePage(id:String,id2:String)=Action.async{
    implicit request =>
      //If user is connect print a form with prefilled data
      printFormWithData(id,id2,routes.CardsManager.cardInsert(id)){
        (card,firmware)=>
          //Data prefilled into the form
          CardsForm(
            "",
            card.acquisition,
            card.firstUse,
            card.agregateur,
            card.apolline,
            firmware.nom,
            firmware.version,
            card.hs,
            card.commentaire,
            ""
          )
      }
  }

  /**
   * This method is call when the user submit a form for insert new card
   * @return Return Ok Action when the user card was insert and return prefilled form for insert a new card
   *         Return Redirect Action when the user is not log in or if card was insert
   *         Return Bad request Action if the form was submit with data error
   *         Return Internal Server Error Action when have mongoDB error
   */
  @ApiOperation(
    nickname = "inventary/card/insert",
    value = "Insert a new card",
    notes = "Insert a new card to the mongoDB database",
    httpMethod = "POST")
  @ApiResponses(Array(
    new ApiResponse(code=303,message="<ul><li>Move resource to the login page at /login if the user is not log</li><li>Move resource to the cards inventary at /inventary/cards/:id if card was insert</li></ul>"),
    new ApiResponse(code=400,message="Fields required or not valid"),
    new ApiResponse(code=500,message="Have a mongoDB error")
  ))
  @ApiImplicitParams(Array(
    new ApiImplicitParam(value = "Id of the card type",required=true,name="id", dataType = "String", paramType = "path"),
    new ApiImplicitParam(value = "Cards id",required=true,name="id", dataType = "String", paramType = "form"),
    new ApiImplicitParam(value = "Acquisition date of the card",required=true,name="acquisition", dataType = "Date", paramType = "form"),
    new ApiImplicitParam(value = "First use date of the card",name="firstUse", dataType = "Date", paramType = "form"),
    new ApiImplicitParam(value = "Flag indicate if the card is an aggregator",name="agregateur",dataType = "Boolean", paramType = "form"),
    new ApiImplicitParam(value = "Apolline version",name="apolline",dataType = "String", paramType = "form"),
    new ApiImplicitParam(value = "Firmware name",name="firmware",dataType = "String", paramType = "form"),
    new ApiImplicitParam(value = "Firmware version",name="versionFirmware",dataType = "String", paramType = "form"),
    new ApiImplicitParam(value = "Flag indicate if the card is out of order",name="hs",dataType = "Boolean", paramType = "form"),
    new ApiImplicitParam(value = "Comment for the card",required=true,name="commentaire",dataType = "String", paramType = "form"),
    new ApiImplicitParam(value = "Value on the button used for send the form",required=true,name="send",defaultValue="Envoyer et continuer",dataType="String",paramType="form")
  ))
  def cardInsert(id:String)=Action.async {
    implicit request =>

      //Verify if the user is connect and if data received are valid
      submitForm(id,routes.CardsManager.cardInsert(id)){

        //Filter for verify if card exists
        cardData=>Json.obj("id" -> cardData.id, "types" -> BSONFormats.BSONObjectIDFormat.writes(BSONObjectID(id)))

      }{
        (cardData,firmware)=>

          //Insert the card into the mongoDB database
          cardDao.insert(
            Cards(
              id = cardData.id,
              types = BSONObjectID(id),
              firmware = firmware,
              acquisition = cardData.acquisition,
              firstUse = cardData.firstUse,
              hs = cardData.hs,
              commentaire = cardData.commentaire,
              agregateur=cardData.agregateur,
              apolline=cardData.apolline
            )
          ).flatMap(e=>
            //When the card was insert
            cardData.send match {

              //If use click on the button "Envoyer et continuer"
              case "Envoyer et continuer" =>{
                //Prepare prefilled data
                val cardForm=cardData.copy(id="")
                //Print the form with prefilled data
                printForm(Results.Ok,id,form.fill(cardForm),routes.CardsManager.cardInsert(id))
              }

              //If user click on an other button redirect her to the card inventary for the current card type
              case _ => future{Redirect(routes.CardsManager.inventary(id))}
            }
            ).recover({
            //Send Internal Server Error if have mongoDB error
            case e => InternalServerError("error")
          })
      }
  }

  /**
   * This method is call when the user submit a form for update a card
   * @return Return Redirect Action when the user is not log in or if card was update
   *         Return Bad request Action if the form was submit with data error
   *         Return Internal Server Error Action when have mongoDB error
   */
  @ApiOperation(
    nickname = "inventary/card/update",
    value = "Update a card",
    notes = "Update a card to the mongoDB database",
    httpMethod = "POST")
  @ApiResponses(Array(
    new ApiResponse(code=303,message="<ul><li>Move resource to the login page at /login if the user is not log</li><li>Move resource to the cards inventary at /inventary/cards/:id if card was update</li></ul>"),
    new ApiResponse(code=400,message="Fields required or not valid"),
    new ApiResponse(code=500,message="Have a mongoDB error")
  ))
  @ApiImplicitParams(Array(
    new ApiImplicitParam(value = "Id of the card type",required=true,name="id", dataType = "String", paramType = "path"),
    new ApiImplicitParam(value = "Cards id",required=true,name="id", dataType = "String", paramType = "form"),
    new ApiImplicitParam(value = "Acquisition date of the card",required=true,name="acquisition", dataType = "Date", paramType = "form"),
    new ApiImplicitParam(value = "First use date of the card",name="firstUse", dataType = "Date", paramType = "form"),
    new ApiImplicitParam(value = "Flag indicate if the card is an aggregator",name="agregateur",dataType = "Boolean", paramType = "form"),
    new ApiImplicitParam(value = "Apolline version",name="apolline",dataType = "String", paramType = "form"),
    new ApiImplicitParam(value = "Firmware name",name="firmware",dataType = "String", paramType = "form"),
    new ApiImplicitParam(value = "Firmware version",name="versionFirmware",dataType = "String", paramType = "form"),
    new ApiImplicitParam(value = "Flag indicate if the card is out of order",name="hs",dataType = "Boolean", paramType = "form"),
    new ApiImplicitParam(value = "Comment for the card",required=true,name="commentaire",dataType = "String", paramType = "form"),
    new ApiImplicitParam(value = "Value on the button used for send the form",required=true,name="send",defaultValue="Envoyer et continuer",dataType="String",paramType="form")
  ))
  def cardUpdate(idType:String,id:String)=Action.async{
    implicit request =>
      //Verify if the user is connect and if data received are valid
      submitForm(idType,routes.CardsManager.cardUpdate(idType,id)){

        //Filter for verify if sensor exists
        cardData=>Json.obj("_id"->Json.obj("$ne"->BSONFormats.BSONObjectIDFormat.writes(BSONObjectID(id))),"id" -> cardData.id, "types" -> BSONFormats.BSONObjectIDFormat.writes(BSONObjectID(idType)))

      }{
        //Update the card
        (cardData,firmware)=>{
          //Update the card
          cardDao.updateById(
            BSONObjectID(id),

            //Create card information
            Cards(
              id = cardData.id,
              types = BSONObjectID(id),
              firmware = firmware,
              acquisition = cardData.acquisition,
              firstUse = cardData.firstUse,
              hs = cardData.hs,
              commentaire = cardData.commentaire,
              agregateur=cardData.agregateur,
              apolline=cardData.apolline
            )
          ).map(e=>
            //If card was update, redirect to the card inventary
            Redirect(routes.CardsManager.inventary(idType))
          ).recover({
            //Send Internal Server Error if have mongoDB error
            case e => InternalServerError("error")
          })
        }
      }
  }

  /**
   * This method is call when the user delete a card
   * @param idType Cards type id
   * @param id Cards id
   * @return Return Redirect Action when the user is not log in or card is delete
   *         Return Internal Server Error Action when have mongoDB error
   */
  @ApiOperation(
    nickname = "inventary/cards/:id/:id2/delete",
    value = "Delete a card",
    notes = "Delete a card to the mongoDB database",
    httpMethod = "GET")
  @ApiResponses(Array(
    new ApiResponse(code=303,message="<ul><li>Move resource to the login page at /login if the user is not log</li><li>Move resource to the card inventary page at /inventary/cards/:id when card is delete"),
    new ApiResponse(code=500,message="Have a mongoDB error")
  ))
  @ApiImplicitParams(Array(
    new ApiImplicitParam(value = "Id of the card type",required=true,name="id", dataType = "String", paramType = "path"),
    new ApiImplicitParam(value = "Id of the card",required=true,name="id2", dataType = "String", paramType = "path")
  ))
  def delete(idType:String,id:String)=Action.async {
    implicit request=>

      //Verify if user is connect
      UserManager.doIfconnectAsync(request) {
        //Verify if card type found
        typeCardsManager.doIfTypeCardsFound(BSONObjectID(idType)) { _ =>
          //Find the card
          cardDao.findOne(Json.obj("_id" -> BSONFormats.BSONObjectIDFormat.writes(BSONObjectID(id)))).flatMap(

            data => data match {

              //If sensor not found redirect to the card inventary
              case None => future {
                Redirect(routes.CardsManager.inventary(idType))
              }

              //If card found
              case Some(cardData) => {
                cardDao.updateById(
                  BSONObjectID(id),
                  cardData.copy(delete=true)
                ).map(e=>
                  //If card was delete, redirect to the card inventary
                  Redirect(routes.CardsManager.inventary(idType))
                ).recover({
                  //Send Internal Server Error if have mongoDB error
                  case e => InternalServerError("error")
                })
              }
            }).recover({
            //Send Internal Server Error if have mongoDB error
            case e => InternalServerError("error")
          })
        }{
          _=> future{Redirect(routes.TypeCardsManager.inventary())}
        }
      }
  }

  def printForm(status: Results.Status,id:String,form:Form[CardsForm],r:Call):Future[Result]={
    val futureAppoline=cardDao.findApolline()
    val futureFirmware=firmwareDao.findFirmware()
    firmwareDao.findVersionFirmware().flatMap(versionFirmware=>
      futureFirmware.flatMap(firmware=>
        futureAppoline.map(apolline=>
          status(views.html.cards.formCards(form, id, r,apolline.toList,firmware.toList,versionFirmware.toList))
        ).recover({case _=>InternalServerError("error")})
      ).recover({case _=>InternalServerError("error")})
    ).recover({case _=>InternalServerError("error")})
  }

  /**
   * Verify if the user is connect and if data received are valid then apply function dedicated
   * @param id Cards type id
   * @param routeSubmit Route use for submit the form
   * @param verif Function use for get card selector
   * @param f Function dedicated
   * @param request
   * @return Return Bad request Action if the form is not valid
   *         Return Redirect if dedicated function is a success
   *         Return Internal server error if have mongoDB error
   */
  def submitForm(id:String,routeSubmit:Call)(verif:CardsForm=>JsObject)(f:(CardsForm,BSONObjectID)=>Future[Result])(implicit request: Request[AnyContent]):Future[Result]={
    //Verify if user is connect
    UserManager.doIfconnectAsync(request) {
      //Verify if sensor type found
      typeCardsManager.doIfTypeCardsFound(BSONObjectID(id)) {_=>
        form.bindFromRequest.fold(

          //If form contains errors
          formWithErrors => {
            //the form is redisplay with error descriptions
            printForm(Results.BadRequest,id,formWithErrors,routeSubmit)
          },

          // Else if form no contains errors
          cardData => {
            val formDate=verifyErrorAcquisitionAfterFirstUse(cardData,form)
            if(formDate.equals(form)) {
              //Find the card
              cardDao.findOne(verif(cardData)).flatMap(
                card => card match {

                  //If card not found, execute dedicated function
                  case None => insertFirmwareIfNotFound(cardData, f)

                  //If card found, return bad request with prefilled form
                  case _ => printForm(Results.BadRequest, id, form.withGlobalError(Messages("inventary.card.error.cardExist")).fill(cardData), routeSubmit)
                }
              ).recover({
                //Send Internal Server Error if have mongoDB error
                case e => InternalServerError("error")
              })
            }else{
              printForm(Results.BadRequest, id, formDate.fill(cardData), routeSubmit)
            }
          }
        )
      } {_=> printForm(Results.BadRequest,id,form.withGlobalError(Messages("inventary.typeCards.error.typeNotExist")),routeSubmit)}
    }
  }

  /**
   * Print a form with prefilled data
   * @param id Cards type id
   * @param id2 Cards id
   * @param r Route call when user submit the form
   * @param f Function for get prefilled information
   * @param request
   * @return Return OK page with the prefilled form
   *         Return Redirect to the card inventary if card not found or to the login page if user is not connect
   *         Return Internal Server Error if have mongoDB error
   */
  def printFormWithData(id:String,id2:String,r:Call)(f:(Cards,Firmware)=>CardsForm)(implicit request:Request[AnyContent]): Future[Result] ={
    //Verify if user is connect
    UserManager.doIfconnectAsync(request) {
      //Verify if card type found
      typeCardsManager.doIfTypeCardsFound(BSONObjectID(id)) {_=>
        //Find the card
        cardDao.findById(BSONObjectID(id2)).flatMap(
          cardOpt => cardOpt match {

            //If the card not found redirect to the card inventary
            case None => future{Redirect(routes.CardsManager.inventary(id))}

            //If the card found
            case Some(card) => {
              firmwareDao.findById(card.firmware).flatMap(
                firmwareOpt=>firmwareOpt match {
                  //If the firmware not found redirect to the card inventary
                  case None => future {
                    Redirect(routes.CardsManager.inventary(id))
                  }

                  //If the firmware found
                  case Some(firmware) => {
                    //print the prefilled form with card information
                    val cardData = f(card, firmware)
                    printForm(Results.Ok, id, form.fill(cardData), r)
                  }
                }
              ).recover({ case _ => InternalServerError("error")})
            }
          }
        ).recover({ case _ => InternalServerError("error")})
      }{_=> printForm(Results.BadRequest,id,form.withGlobalError(Messages("inventary.typeCards.error.typeNotExist")),r)}
    }
  }

  /**
   * Insert, if not exist, the firmware into the database before applyed the card dedicated function
   * @param cardData Data received when the form was submit
   * @param f Dedicated function
   * @return
   */
  def insertFirmwareIfNotFound(cardData:CardsForm,f:(CardsForm,BSONObjectID)=>Future[Result]): Future[Result] ={
    //Find the firmware
    firmwareDao.findOne(Json.obj("nom"->cardData.firmware,"version"->cardData.versionFirmware)).flatMap(
      data => data match{

        //If the firmware not found
        case None=>{
          val firmware=Firmware(nom=cardData.firmware,version=cardData.versionFirmware)
          //Insert the firmware into the database then execute the card dedicated function
          firmwareDao.insert(firmware).flatMap(_=>f(cardData,firmware._id)).recover({case _=> InternalServerError("error 1")})
        }

        //If the firmware found, execute the card dedicated function
        case Some(firmware)=>f(cardData,firmware._id)
      }
    ).recover({case _=>InternalServerError("error")})
  }

  /**
   * Verify if acquisition date is before first use date
   * @param cardData Data received from the form
   * @param form The current form
   * @return Return the form after verification
   */
  def verifyErrorAcquisitionAfterFirstUse(cardData:CardsForm,form:Form[CardsForm]):Form[CardsForm]={
    //If first use date is defined and acquisition date is after
    if(cardData.firstUse.nonEmpty && cardData.acquisition.after(cardData.firstUse.get)){
      //Return form with an error
      form.withError("firstUse",Messages("inventary.card.error.firstUseBeforeAcquisition"))
    }else{
      form
    }
  }
}

@Api(value = "/card", description = "Operations for cards")
object CardsManager extends CardsManagerLike