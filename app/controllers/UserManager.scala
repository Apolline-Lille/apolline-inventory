package controllers

import java.lang.annotation.Annotation

import com.wordnik.swagger.annotations._
import models._
import play.api._
import play.api.mvc._
import play.api.libs._
import play.modules.reactivemongo.MongoController
import reactivemongo.api._

import play.api.libs.json._
import play.api.data._
import play.api.data.Forms._
import scala.concurrent._
import scala.util.Random
import scala.util.Failure
import scala.util.Success

import scala.concurrent.duration.Duration

import play.api.libs.concurrent.Execution.Implicits.defaultContext

/**
 * This object is a controller for manage all user page
 */
@Api(value = "/user", description = "Operations for the user")
object UserManager extends Controller with MongoController{

  val userDao=new UserDao(db)

  /************ Internal classes ***************/

  /**
   * This class represent all information get when the user is log
   * @param login User login
   * @param passwd User password
   */
  case class LoginInfo(login: String, passwd : String)

  /**
   * This class represent all information get when the user is register
   * @param login User login
   * @param passwd User password
   * @param passwd2 User password verification
   */
  case class UserInfo(login: String, passwd : String, passwd2 : String)

  /************* Property *********************/

  /**
   * Value contains the configuration of the login form
   */
   private lazy val loginForm = Form[LoginInfo](
      mapping(
          "login" -> nonEmptyText,
          "passwd" -> nonEmptyText)(LoginInfo.apply)(LoginInfo.unapply)
      )

  /**
   * Value contains the configuration of the register form
   */
  private lazy val addUserForm = Form[UserInfo](
        mapping(
            "login" -> nonEmptyText,
            "passwd" -> nonEmptyText,
            "passwd2" -> nonEmptyText)(UserInfo.apply)(UserInfo.unapply)
  )

  /****************** Route methods ***********/

  /**
   * This method is call when the user is on the page /login
   * @return Return Action triggered when the user is on the page /login
   */
  @ApiOperation(
    nickname = "logIn",
    value = "Get the page for user log in",
    httpMethod = "GET")
  def loginPage = Action {
    //Send a page 200 Ok with the login form
    Ok(views.html.users.login(loginForm))
  }

  /**
   * This method is call when the user send her login data
   * @return Return Action triggered when the user send her login data
   */
  @ApiOperation(
    nickname = "logInSubmit",
    value = "Post login and password for user log in",
    notes="User set login and password into fields and submit the form for logs",
    httpMethod = "POST")
  @ApiResponses(Array(
    new ApiResponse(code=303,message="Move resource to the dashboard at /dashboard"),
    new ApiResponse(code=400,message="<ul><li>Fields required or not valid</li><li>Incorrect login or password</li></ul>")
  ))
  @ApiImplicitParams(Array(
    new ApiImplicitParam(value = "Login user",name="login", required = true, dataType = "String", paramType = "form"),
    new ApiImplicitParam(value = "Password user",name="passwd", required = true, dataType = "String", paramType = "form")
  ))
  def loginSubmit = Action.async { implicit request =>

    //When the user send her login data
    loginForm.bindFromRequest.fold(

      //If form contains errors
      formWithErrors => {

        //the form is redisplay with error descriptions
        Future.successful(BadRequest(views.html.users.login(formWithErrors)))
      },

      // Else if form no contains errors
      loginData => {

        //Verify if login correspond to an user
        loginVerification(loginData.login,loginData.passwd)

      }
    )
  }

  /**
   * This method is call when user disconnect her
   * @return Return Action triggered when user disconnect her
   */
  @ApiOperation(
    nickname = "logOut",
    value = "Disconnect the user",
    httpMethod = "GET")
  @ApiResponses(Array(
    new ApiResponse(code=303,message="Move resource to the login page at /login")
  ))
  def disconnect = Action {
    request =>
      //Redirect the user to the index page
   Redirect(routes.UserManager.loginPage()).withSession(request.session - "user")
  }

  /**
   * This method is call when the user in on the page /register
   * @return Return Action triggered when the user is on the page /register
   */
  def addUserPage = Action {
    //Send a page 200 Ok with the register form
    Ok(views.html.users.addUser(addUserForm))
  }

  /**
   * This method is call when the user register
   * @return Return Action triggered when the user register
   */
  def addUserSubmit = Action.async { implicit request =>

    //When the user send her register data
    addUserForm.bindFromRequest.fold(

      //If form contains errors
      formWithErrors => {

        //the form is redisplay with error descriptions
        Future.successful(BadRequest(views.html.users.addUser(formWithErrors)))
      },

      //Else if form no contains errors
      userData => {

        //Get register data
        val login = userData.login
        val passwd = userData.passwd
        val passwd2 = userData.passwd2

        //If password is equals
        if(passwd.equals(passwd2)){

          //Generate encrypt password
          val rand=new Random()
          val salt=rand.nextString(10)
          val passwdEncrypt=encryptPassword(passwd,salt,10)

          //Insert the user to the database
          val future=userDao.insert(User(username=login,password=passwdEncrypt,salt=salt))
          val p=promise[Result]
          future.onComplete({
            case Failure(e) => p success Ok(views.html.users.addUser(addUserForm.withGlobalError("Error when insert on the database")))
            case Success(lastError) => p success Redirect(routes.Dashboard.index())
          })
          p.future
        }
        else{
          Future.successful(Ok(views.html.users.addUser(addUserForm.withError("passwd2","Password is not the same"))))
        }


      }
    )
  }

  /****************  Methods  ***********************/

  /**
   * This method encrypt "iter" times to md5 an password with a salt key
   * @param passwd Password to encrypt
   * @param salt Key salt
   * @param iter Number of times the password is encrypt
   * @return Return the password encrypt
   */
  private def encryptPassword(passwd : String, salt : String,iter : Int):String =
      if(iter > 0)
        //Encrypt "iter" - 1 times the password
        encryptPassword(Codecs.md5((passwd + salt).toString.getBytes()),salt,iter-1)
      else
        //Return the password encrypt
        passwd

  /**
   * Verify if login data correspond to an user in a database
   * @param login User Login
   * @param passwd User password
   */
  private def loginVerification(login:String,passwd:String): Future[Result] = {
    //Get the list of user with this login
    val cursor = userDao.
      findAll(Json.obj("username" -> login))

    //Map result of the query
    cursor.map { result =>
      //If an user with the login have the good encrypt password
      val resFilter=result.filter(obj => obj.password.equals(encryptPassword(passwd, obj.salt,10)))
      if(resFilter.nonEmpty){

        //Redirect user to the dashboard
        Redirect(routes.Dashboard.index()).withSession("user" -> Json.stringify(Json.toJson(Map("login"->resFilter.head.username,"_id" -> resFilter.head._id.stringify))))
      }
      else{

        //the form is redisplay with error descriptions
        BadRequest(views.html.users.login(loginForm.withError("login", "login/password incorrect")))
      }
    }

  }

  def doIfconnect(request:Request[AnyContent])(f: => Result):Result={
    request.session.get("user").map(user => {
      f
    }
    ).getOrElse(
        Redirect(routes.UserManager.loginPage())
     )
  }

  def doIfconnectAsync(request:Request[AnyContent])(f: => Future[Result]):Future[Result]={
    request.session.get("user").map(user => {
      f
    }
    ).getOrElse(
        future(Redirect(routes.UserManager.loginPage()))
      )
  }
}