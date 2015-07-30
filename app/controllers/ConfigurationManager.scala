package controllers

import play.api.data.Form
import play.api.data.Forms._
import play.api.libs.json.Json
import play.api.mvc.{Action, Controller}
import reactivemongo.bson.BSONObjectID

import scala.concurrent._

import scala.concurrent.ExecutionContext.Implicits.global

case class ConfigurationForm(port:String,timeout:Int=10000,baud:Int=9600,bits:Int=8,stopBits:Int=1,parity:Int=0,timeFilter:Int=1000,types:String)

/**
 * This trait is a controller for manager module configuration
 */
trait ConfigurationManagerLike extends Controller{

  implicit val formatsForm=Json.format[ConfigurationForm]

  val form=Form[ConfigurationForm](
    mapping(
      "port"->nonEmptyText,
      "timeout"->number(min=0),
      "baud"->number(min=0),
      "bits"->number(min=5,max=8),
      "stopBits"->number(min=1,max=3),
      "parity"->number(min=0,max=4),
      "timeFilter"->number(min=0),
      "types"->nonEmptyText
    )(ConfigurationForm.apply)(ConfigurationForm.unapply)
  )

  val moduleManager:ModuleManagerLike=ModuleManager

  /**
   * Display a form for insert module configuration
   * @param id Module id
   * @return A 200 OK page, with the form for insert module configuration
   *         Redirect if module not found or if the user is not log in
   */
  def formConfiguration(id:String)=Action.async{
    implicit request =>
      //Verify if user is connect
      UserManager.doIfconnectAsync(request) {

        //Verify if module exists
        moduleManager.doIfModuleFound(BSONObjectID(id)){

          //Display the form for insert module configuration
          module=> future{Ok(views.html.configuration.form(form.fill(ConfigurationForm(port="",types = "")),module))}
        }{
           //Redirect to the list of modules
          _ =>
            future{Redirect(routes.ModuleManager.inventary())}
        }
      }
  }

  /**
   * Insert module configuration to the session and redirect to the page for set index associat to data receive
   * @param id Module id
   * @return Redirect if module not found or if the user is not log in
   *         Bad request with a prefilled form if form contains errors
   */
  def insertFormConfiguration(id:String)=Action.async{
    implicit request =>
      //Verify if user is connect
      UserManager.doIfconnectAsync(request) {

        //Verify if module exists
        moduleManager.doIfModuleFound(BSONObjectID(id)){

          module=> form.bindFromRequest().fold(

            //If form contains errors, send bad request with a prefilled form
            formWithError =>future{BadRequest(views.html.configuration.form(formWithError,module))},

            //If contains not errors, redirect to the list of sensors contains in the module
            data=>future{Redirect(routes.ConfigurationManager.listSensors(id)).withSession("configForm"->"insert","config"->Json.stringify(formatsForm.writes(data)))}
          )
        }{

          //Redirect to the list of modules
          _ =>future{Redirect(routes.ModuleManager.inventary())}
        }
      }
  }

  def listSensors(id:String)=Action{
    Ok("ok")
  }
}
object ConfigurationManager extends ConfigurationManagerLike
