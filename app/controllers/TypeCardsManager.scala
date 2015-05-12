package controllers

import com.wordnik.swagger.annotations._
import models._
import play.api.i18n.Messages
import play.api.libs.json.{Json, JsObject}
import play.api.mvc._
import play.api.data._
import play.api.data.Forms._
import play.modules.reactivemongo.json.BSONFormats
import reactivemongo.bson.{BSONObjectID, BSONDocument}
import scala.concurrent._

import play.api.libs.concurrent.Execution.Implicits.defaultContext

case class TypeCardsForm(modele:String,types:String)

/**
 * This trait is a controller for manage sensors type
 */
trait TypeCardsManagerLike extends Controller {

  /** *********** Property *********************/

  /**
   * DAO for card type
   */
  val typeCardsDao: TypeCardsDao = TypeCardsDaoObj

  val cardDao: CardsDao = CardsDaoObj

  lazy val form=Form[TypeCardsForm](
    mapping(
      "modele"->nonEmptyText,
      "types"->nonEmptyText
    )(TypeCardsForm.apply)(TypeCardsForm.unapply)
  )

  /** **************** Route methods ***********/

  /**
   * This method is call when the user is on the page /inventary/cards. It list cards type available
   * @return Return Ok Action when the user is on the page /inventary/cards with the list of cards type
   *         Return Redirect Action when the user is not log in
   *         Return Internal Server Error Action when have mongoDB error
   */
  @ApiOperation(
    nickname = "inventary",
    value = "Get the html page for list cards type",
    notes = "Get the html page for list cards type",
    httpMethod = "GET")
  @ApiResponses(Array(
    new ApiResponse(code = 303, message = "Move resource to the login page at /login if the user is not log"),
    new ApiResponse(code = 500, message = "Have a mongoDB error")
  ))
  @ApiImplicitParams(Array(
    new ApiImplicitParam(value = "Cards type name for filter all cards type", name = "sort", dataType = "String", paramType = "query")
  ))
  def inventary(sort: String = "",filtreSto:String = "") = Action.async {
    request =>
      //Verify if user is connect
      UserManager.doIfconnectAsync(request) {
        //Create selector for select card type
        val selector=if(sort.isEmpty){Json.obj("delete"->false)}else{Json.obj("delete"->false,"types"->sort)}

        //Find all card type name
        val futureListType=typeCardsDao.findListType()
        val futureCountCards=cardDao.countCards()

        //Find all card type
        typeCardsDao.findAll(selector).flatMap(listType=>
          futureListType.flatMap(filtre=>
            futureCountCards.map(countCards=>

              //Print the list of card type
              Ok(views.html.cards.listTypeCards(filtreSto,sort,filtreStock(filtreSto),listType,countCards.toList,filtre.toList))

            ).recover({case _=>InternalServerError("error")})
          ).recover({case _=>InternalServerError("error")})
        ).recover({case _=>InternalServerError("error")})
      }
  }

  /**
   * This method is call when the user is on the page /inventary/cards/type. It display a form for add new cards type
   * @return Return Ok Action when the user is on the page /inventary/cards/type with the form
   *         Return Redirect Action when the user is not log in
   */
  @ApiOperation(
    nickname = "inventary/cards",
    value = "Get the html page for insert a new cards type",
    notes = "Get the html page for insert a new cards type",
    httpMethod = "GET")
  @ApiResponses(Array(
    new ApiResponse(code=303,message="Move resource to the login page at /login if the user is not log")
  ))
  def typePage=Action.async{
    implicit request =>
      //Verify if user is connect
      UserManager.doIfconnectAsync(request) {
          //Display the form for insert new card type
          printForm(Results.Ok,form,routes.TypeCardsManager.typeInsert())
      }
  }

  /**
   * This method is call when the user is on the page /inventary/cards/:id/update. It display a form for update card type
   * @param id Cards type id
   * @return Return Ok Action when the user is on the page /inventary/cards/:id/update with the form
   *         Return Redirect Action when the user is not log in or card type information not found
   *         Return Internal Server Error Action when have mongoDB error
   */
  @ApiOperation(
    nickname = "inventary/cards/:id/update",
    value = "Get the html page for update card type",
    notes = "Get the html page for update card type",
    httpMethod = "GET")
  @ApiResponses(Array(
    new ApiResponse(code=303,message="<ul><li>Move resource to the login page at /login if the user is not log</li><li>Move resource to the card inventary page at /inventary/cards when card type not found"),
    new ApiResponse(code=500,message="Have a mongoDB error")
  ))
  @ApiImplicitParams(Array(
    new ApiImplicitParam(value = "Id of the card type",required=true,name="id", dataType = "String", paramType = "path")
  ))
  def typeUpdatePage(id:String)=Action.async{
    implicit request =>
      //Verify if user is connect
      UserManager.doIfconnectAsync(request) {

        //Find the card type
        typeCardsDao.findById(BSONObjectID(id)).flatMap(
          typeCardsOpt=> typeCardsOpt match{

            //Cards type not found redirect to the card inventary
            case None=>future{Redirect(routes.TypeCardsManager.inventary())}

            //Cards type found
            case Some(typeCards)=>{

              //Prepare data for prefilled the form
              val typeCardsData = TypeCardsForm(typeCards.modele,typeCards.types)

              //Display the form for update card type
              printForm(Results.Ok,form.fill(typeCardsData),routes.TypeCardsManager.typeUpdate(id))
            }
          }
        ).recover({
          //Send an Internal Server Error for mongoDB error
          case e=>InternalServerError("error")
        })
      }
  }

  /**
   * This method is call when the user submit a form for insert a new card type
   * @return Return Bad Request Action if the form was submit with data error
   *         Return Redirect Action when the user is not log in or card type is insert
   *         Return Internal Server Error Action when have mongoDB error
   */
  @ApiOperation(
    nickname = "inventary/cards/type",
    value = "Insert a new card type",
    notes = "Insert a new card type to the mongoDB database",
    httpMethod = "POST")
  @ApiResponses(Array(
    new ApiResponse(code=303,message="<ul><li>Move resource to the login page at /login if the user is not log</li><li>Move resource to the card inventary page at /inventary/cards when card type is insert"),
    new ApiResponse(code=400,message="Fields required or not valid"),
    new ApiResponse(code=500,message="Have a mongoDB error")
  ))
  @ApiImplicitParams(Array(
    new ApiImplicitParam (value = "Name of the card model",required=true,name="modele", dataType = "String", paramType = "form"),
    new ApiImplicitParam (value = "Name of the card type",required=true,name="types", dataType = "String", paramType = "form")
  ))
  def typeInsert=Action.async{
    implicit request=>
      //Verify if the user is connect and if data received are valid
      submitForm(routes.TypeCardsManager.typeInsert()) {
        typeData => Json.obj("modele" -> typeData.modele, "type" -> typeData.types)
      }{typeData=>{

        //Insert card type
        typeCardsDao.insert(TypeCards(
          types=typeData.types,
          modele=typeData.modele
        )).map(
            //Redirect to the inventary if sensor type was insert
            e => Redirect(routes.TypeCardsManager.inventary())
        ).recover({
          //Send Internal Server Error if have mongoDB error
          case e => InternalServerError("error")
        })

      }
      }
  }

  /**
   * This method is call when the user submit a form for update a card type
   * @param id Sensor type id
   * @return Return Bad Request Action if the form was submit with data error
   *         Return Redirect Action when the user is not log in or card type is update
   *         Return Internal Server Error Action when have mongoDB error
   */
  @ApiOperation(
    nickname = "inventary/cards/:id/update",
    value = "Update a card type",
    notes = "Update a card type to the mongoDB database",
    httpMethod = "POST")
  @ApiResponses(Array(
    new ApiResponse(code=303,message="<ul><li>Move resource to the login page at /login if the user is not log</li><li>Move resource to the card inventary page at /inventary/cards when card type is update"),
    new ApiResponse(code=400,message="Fields required or not valid"),
    new ApiResponse(code=500,message="Have a mongoDB error")
  ))
  @ApiImplicitParams(Array(
    new ApiImplicitParam(value = "Id of the card type",required=true,name="id", dataType = "String", paramType = "path"),
    new ApiImplicitParam (value = "Name of the card model",required=true,name="modele", dataType = "String", paramType = "form"),
    new ApiImplicitParam (value = "Name of the card type",required=true,name="types", dataType = "String", paramType = "form")
  ))
  def typeUpdate(id:String)=Action.async{
    implicit request=>

      //Verify if the user is connect and if data received are valid
      submitForm(routes.TypeCardsManager.typeUpdate(id)){
        typeData => Json.obj("_id"->Json.obj("$ne"->BSONFormats.BSONObjectIDFormat.writes(BSONObjectID(id))),"modele" -> typeData.modele, "types" -> typeData.types)
      }{typeData=>{

        //Update card type
        typeCardsDao.updateById(BSONObjectID(id),
          TypeCards(
            _id=BSONObjectID(id),
            types=typeData.types,
            modele=typeData.modele
          )).map(
            //Redirect to the inventary if card type was update
            e => Redirect(routes.TypeCardsManager.inventary())
          ).recover({
          //Send Internal Server Error if have mongoDB error
          case e => InternalServerError("error")
        })
      }
      }
  }

  /**
   * This method is call when the user delete a card type
   * @param id Cards type id
   * @return Return Redirect Action when the user is not log in or card type is delete
   *         Return Internal Server Error Action when have mongoDB error
   */
  @ApiOperation(
    nickname = "inventary/cards/:id/delete",
    value = "Delete a card type",
    notes = "Delete a card type to the mongoDB database",
    httpMethod = "GET")
  @ApiResponses(Array(
    new ApiResponse(code=303,message="<ul><li>Move resource to the login page at /login if the user is not log</li><li>Move resource to the card inventary page at /inventary/cards when card type is delete"),
    new ApiResponse(code=500,message="Have a mongoDB error")
  ))
  @ApiImplicitParams(Array(
    new ApiImplicitParam(value = "Id of the card type",required=true,name="id", dataType = "String", paramType = "path")
  ))
  def delete(id:String)=Action.async{
    implicit request=>
      //Verify if user is connect
      UserManager.doIfconnectAsync(request) {

        val idFormat=BSONFormats.BSONObjectIDFormat.writes(BSONObjectID(id))
        val findCards=cardDao.findOne(Json.obj("delete"->false,"types"->idFormat))
        //find the card type
        typeCardsDao.findOne(Json.obj("_id"->idFormat)).flatMap(
          data => data match{

            //Cards type not found redirect to the cards inventary
            case None =>future{Redirect(routes.TypeCardsManager.inventary())}

            //Cards type found
            case Some(typeCardsData) => {
              findCards.flatMap(
                cardData=>cardData match{
                  case None=>{

                    //Update the card type and set the delete column to true
                    typeCardsDao.updateById(
                      BSONObjectID(id),
                      typeCardsData.copy(delete=true)
                    ).map(
                        //Redirect to the cards inventary after delete cards type
                        e => Redirect(routes.TypeCardsManager.inventary())
                      ).recover({
                      //Send Internal Server Error if have mongoDB error
                      case e => InternalServerError("error")
                    })
                  }
                  case _ => future{Redirect(routes.TypeCardsManager.inventary())}
                }
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

  def printForm(status: Results.Status,form:Form[TypeCardsForm],r:Call):Future[Result]={
    val futureModele=typeCardsDao.findListModele()
    val futureType=typeCardsDao.findListType()
    futureModele.flatMap(modele=>
      futureType.map(types=>
        status(views.html.cards.formType(form,modele.toList,types.toList,r))
      ).recover({case _=>InternalServerError("error")})
    ).recover({case _=>InternalServerError("error")})
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
  def submitForm(r:Call)(verif:TypeCardsForm=>JsObject)(f:TypeCardsForm=>Future[Result])(implicit request: Request[AnyContent]):Future[Result]={
    //Verify if user is connect
    UserManager.doIfconnectAsync(request) {
      form.bindFromRequest.fold(

        //If form contains errors
        formWithErrors => {
            //the form is redisplay with error descriptions
            printForm(Results.BadRequest, formWithErrors, r)
        },

        // Else if form no contains errors
        typeData => {

          //Find the sensor type
          typeCardsDao.findOne(verif(typeData)).flatMap(
            e=> e match {

              //If sensor type not found
              case None => f(typeData)

              //print form with prefilled data and a bad request
              case _ => printForm(Results.BadRequest, form.withGlobalError(Messages("inventary.typeCards.error.typeExist")).fill(typeData), r)
            }
          ).recover({
            //Send Internal Server Error if have mongoDB error
            case e => InternalServerError("error")
          })
        }
      )
    }
  }

  /**
   * Verify if card type found and execute a function if card type found or not
   * @param id Cards type id
   * @param found Function executed if card type found
   * @param notFound Function executed if card type not found
   * @return Return the result of executed function
   */
  def doIfTypeCardsFound(id:BSONObjectID)(found:Unit=>Future[Result])(notFound:Unit=>Future[Result]):Future[Result]={
    //Find the card type
    typeCardsDao.findOne(Json.obj("_id"->BSONFormats.BSONObjectIDFormat.writes(id),"delete"->false)).flatMap(
      typeCardsOpt => typeCardsOpt match{

        //If the card type not found execute function not found
        case None => notFound()

        //If the card type found execute function found
        case Some(_) => found()
      }
    ).recover({
      //Send Internal Server Error if have mongoDB error
      case _=> InternalServerError("error")
    })
  }

  def filtreStock(filtre:String)(v:Int)=filtre match{
    case "yes" => v>0
    case "no" => v==0
    case _ => v>=0
  }
}

@Api(value = "/typeCards", description = "Operations for cards type")
object TypeCardsManager extends TypeCardsManagerLike
