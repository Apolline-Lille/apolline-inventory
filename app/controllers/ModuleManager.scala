package controllers

import com.wordnik.swagger.annotations._
import models.{TypeModuleDaoObj, TypeModuleDao}
import play.api.mvc.{Action, Controller}
import reactivemongo.bson.BSONObjectID

import play.api.libs.concurrent.Execution.Implicits.defaultContext

trait ModuleManagerLike extends Controller{

  val typeModuleDao:TypeModuleDao=TypeModuleDaoObj

  /**
   * This method is call when the user is on the page /inventary/modules/:id. It list sensors available for a particular type
   * @return Return Ok Action when the user is on the page /inventary/sensors/:id with the list of sensors
   *         Return Redirect Action when the user is not log in
   *         Return Internal Server Error Action when have mongoDB error
   */
  @ApiOperation(
    nickname = "inventary",
    value = "Get the html page for list modules",
    notes = "Get the html page for list modules",
    httpMethod = "GET")
  @ApiResponses(Array(
    new ApiResponse(code=303,message="Move resource to the login page at /login if the user is not log"),
    new ApiResponse(code=500,message="Have a mongoDB error")
  ))
  @ApiImplicitParams(Array(
    new ApiImplicitParam(value = "Id of the module type for list modules associated",required=true,name="id", dataType = "String", paramType = "path")
  ))
  def inventary(id:String,sort:String="id",sens:Int=1)=Action.async{
    implicit request =>
      //Verify if user is connect
      UserManager.doIfconnectAsync(request) {
        //Find the module type
        typeModuleDao.findById(BSONObjectID(id)).map(
          data=> data match{

              //If module type not found
            case None => Redirect(routes.TypeModuleManager.inventary())

              //If module type found
            case Some(typeModule) => Ok(views.html.module.listModule(typeModule))
          }
        ).recover({case _=>InternalServerError("error")})
      }
  }
}

@Api(value = "/module", description = "Operations for modules")
object ModuleManager extends ModuleManagerLike
