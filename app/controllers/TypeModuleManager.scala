package controllers

import com.wordnik.swagger.annotations._
import models._
import play.api.mvc.{Action, Controller}
import scala.concurrent._

import play.api.libs.concurrent.Execution.Implicits.defaultContext

/**
 * This trait is a controller for manage sensors type
 */
trait TypeModuleManagerLike extends Controller {

  /** *********** Property *********************/


  /**
   * DAO for module type
   */
  val typeModuleDao: TypeModuleDao = TypeModuleDaoObj

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
  def inventary(sort: String = "") = Action.async {
    request =>
      //Verify if user is connect
      UserManager.doIfconnectAsync(request) {
        future{Ok(views.html.module.listTypeModule())}

      }
  }
}

@Api(value = "/typeModule", description = "Operations for modules type")
object TypeModuleManager extends TypeModuleManagerLike
