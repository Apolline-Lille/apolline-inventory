package controllers

import com.wordnik.swagger.annotations._
import models._
import play.api.i18n.Messages
import play.api.libs.json.{Json, JsObject}
import play.api.mvc._
import play.api.data._
import play.api.data.Forms._
import reactivemongo.bson.BSONDocument
import scala.concurrent._

import play.api.libs.concurrent.Execution.Implicits.defaultContext

case class TypeModuleForm(modele:String,types:String)

/**
 * This trait is a controller for manage sensors type
 */
trait TypeModuleManagerLike extends Controller {

  /** *********** Property *********************/

  /**
   * DAO for module type
   */
  val typeModuleDao: TypeModuleDao = TypeModuleDaoObj

  lazy val form=Form[TypeModuleForm](
    mapping(
      "modele"->nonEmptyText,
      "types"->nonEmptyText
    )(TypeModuleForm.apply)(TypeModuleForm.unapply)
  )

  /** **************** Route methods ***********/

  /**
   * This method is call when the user is on the page /inventary/modules. It list modules type available
   * @return Return Ok Action when the user is on the page /inventary/modules with the list of modules type
   *         Return Redirect Action when the user is not log in
   *         Return Internal Server Error Action when have mongoDB error
   */
  @ApiOperation(
    nickname = "inventary",
    value = "Get the html page for list modules type",
    notes = "Get the html page for list modules type",
    httpMethod = "GET")
  @ApiResponses(Array(
    new ApiResponse(code = 303, message = "Move resource to the login page at /login if the user is not log"),
    new ApiResponse(code = 500, message = "Have a mongoDB error")
  ))
  @ApiImplicitParams(Array(
    new ApiImplicitParam(value = "Module type name for filter all modules type", name = "sort", dataType = "String", paramType = "query")
  ))
  def inventary(sort: String = "",filtreSto:String = "") = Action.async {
    request =>
      //Verify if user is connect
      UserManager.doIfconnectAsync(request) {
        //Create selector for select module type
        val selector=if(sort.isEmpty){Json.obj("delete"->false)}else{Json.obj("delete"->false,"types"->sort)}

        //Find all module type name
        val futureListType=typeModuleDao.findListType()

        //Find all module type
        typeModuleDao.findAll(selector).flatMap(listType=>
          futureListType.map(filtre=>

            //Print the list of module type
            Ok(views.html.module.listTypeModule(filtreSto,sort,filtreStock(filtreSto),listType,List[BSONDocument](),filtre.toList))

          ).recover({case _=>InternalServerError("error")})
        ).recover({case _=>InternalServerError("error")})
      }
  }

  /**
   * This method is call when the user is on the page /inventary/modules/type. It display a form for add new modules type
   * @return Return Ok Action when the user is on the page /inventary/modules/type with the form
   *         Return Redirect Action when the user is not log in
   */
  @ApiOperation(
    nickname = "inventary/modules",
    value = "Get the html page for insert a new modules type",
    notes = "Get the html page for insert a new modules type",
    httpMethod = "GET")
  @ApiResponses(Array(
    new ApiResponse(code=303,message="Move resource to the login page at /login if the user is not log")
  ))
  def typePage=Action.async{
    implicit request =>
      //Verify if user is connect
      UserManager.doIfconnectAsync(request) {
          //Display the form for insert new module type
          printForm(Results.Ok,form,routes.TypeModuleManager.typeInsert())
      }
  }

  /**
   * This method is call when the user submit a form for insert a new module type
   * @return Return Bad Request Action if the form was submit with data error
   *         Return Redirect Action when the user is not log in or module type is insert
   *         Return Internal Server Error Action when have mongoDB error
   */
  @ApiOperation(
    nickname = "inventary/modules/type",
    value = "Insert a new module type",
    notes = "Insert a new module type to the mongoDB database",
    httpMethod = "POST")
  @ApiResponses(Array(
    new ApiResponse(code=303,message="<ul><li>Move resource to the login page at /login if the user is not log</li><li>Move resource to the module inventary page at /inventary/modules when module type is insert"),
    new ApiResponse(code=400,message="Fields required or not valid"),
    new ApiResponse(code=500,message="Have a mongoDB error")
  ))
  @ApiImplicitParams(Array(
    new ApiImplicitParam (value = "Name of the module model",required=true,name="modele", dataType = "String", paramType = "form"),
    new ApiImplicitParam (value = "Name of the module type",required=true,name="types", dataType = "String", paramType = "form")
  ))
  def typeInsert=Action.async{
    implicit request=>
      //Verify if the user is connect and if data received are valid
      submitForm(routes.TypeModuleManager.typeInsert()) {
        typeData => Json.obj("modele" -> typeData.modele, "type" -> typeData.types)
      }{typeData=>{

        //Insert module type
        typeModuleDao.insert(TypeModule(
          types=typeData.types,
          modele=typeData.modele
        )).map(
            //Redirect to the inventary if sensor type was insert
            e => Redirect(routes.TypeModuleManager.inventary())
        ).recover({
          //Send Internal Server Error if have mongoDB error
          case e => InternalServerError("error")
        })

      }
      }
  }

  def printForm(status: Results.Status,form:Form[TypeModuleForm],r:Call):Future[Result]={
    val futureModele=typeModuleDao.findListModele()
    val futureType=typeModuleDao.findListType()
    futureModele.flatMap(modele=>
      futureType.map(types=>
        status(views.html.module.formType(form,modele.toList,types.toList,r))
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
  def submitForm(r:Call)(verif:TypeModuleForm=>JsObject)(f:TypeModuleForm=>Future[Result])(implicit request: Request[AnyContent]):Future[Result]={
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
          typeModuleDao.findOne(verif(typeData)).flatMap(
            e=> e match {

              //If sensor type not found
              case None => f(typeData)

              //print form with prefilled data and a bad request
              case _ => printForm(Results.BadRequest, form.withGlobalError(Messages("inventary.typeModule.error.typeExist")).fill(typeData), r)
            }
          ).recover({
            //Send Internal Server Error if have mongoDB error
            case e => InternalServerError("error")
          })
        }
      )
    }
  }

  def filtreStock(filtre:String)(v:Int)=filtre match{
    case "yes" => v>0
    case "no" => v==0
    case _ => v>=0
  }
}

@Api(value = "/typeModule", description = "Operations for modules type")
object TypeModuleManager extends TypeModuleManagerLike
