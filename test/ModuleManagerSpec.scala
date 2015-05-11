import java.util.Date

import controllers._
import models._
import org.junit.runner.RunWith
import org.specs2.matcher.{MatchResult, Expectable, Matcher}
import org.specs2.mock.Mockito
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner
import play.api.data.Form
import play.api.libs.json.{Writes, JsObject, Json}
import play.api.mvc.{Call, Results, Action, Result}
import play.api.test.Helpers._
import play.api.test.{FakeRequest, WithApplication}
import reactivemongo.bson.{BSONDocument, BSONObjectID}
import reactivemongo.core.commands.{LastError, GetLastError}

import scala.concurrent._

@RunWith(classOf[JUnitRunner])
class ModuleManagerSpec extends Specification with Mockito {

  class ModuleManagerTest extends ModuleManagerLike

  class TypeModuleManagerTest extends TypeModuleManagerLike

  case class matchRegex(a: String) extends Matcher[String]() {
    override def apply[S <: String](t: Expectable[S]): MatchResult[S] = result(a.r.findFirstIn(t.value).nonEmpty, "okMessage", t.value + "not found " + a, t)
  }

  case class contains(a: String, b: Int) extends Matcher[String]() {
    override def apply[S <: String](t: Expectable[S]): MatchResult[S] = result(a.r.findAllMatchIn(t.value).size == b, "okMessage", "not found " + b + " times but " + a.r.findAllMatchIn(t.value).size + " times " + a, t)
  }

  val date = new Date(115, 3, 22)
  val date2 = new Date(115, 3, 23)

  val bson = BSONObjectID.generate
  val bson2 = BSONObjectID.generate
  val bson3 = BSONObjectID.generate
  val bson4 = BSONObjectID.generate

  def fixture = new {
    val typeModuleDaoMock=mock[TypeModuleDao]
    val moduleDaoMock=mock[ModuleDao]
    val firmwareDaoMock=mock[FirmwareDao]
    val typeModuleManagerMock=mock[TypeModuleManagerLike]
    val controller=new ModuleManagerTest{
      override val typeModuleDao:TypeModuleDao=typeModuleDaoMock
      override val moduleDao:ModuleDao=moduleDaoMock
      override val firmwareDao:FirmwareDao=firmwareDaoMock
      override val typeModuleManager:TypeModuleManagerLike=typeModuleManagerMock
    }

    def applyFoundFunction() {
      typeModuleManagerMock.doIfTypeModuleFound(org.mockito.Matchers.eq(bson))(any[Unit => Future[Result]])(any[Unit => Future[Result]]) answers { (params, _) => params match {
        case Array(_, p: (Unit => Future[Result]), _) => p.apply()
      }}
    }

    def applyNotFoundFunction() {
      typeModuleManagerMock.doIfTypeModuleFound(org.mockito.Matchers.eq(bson))(any[Unit => Future[Result]])(any[Unit => Future[Result]]) answers { (params, _) => params match {
        case Array(_,_, p: (Unit => Future[Result])) => p.apply()
      }}
    }
  }

  "When user is not connected, ModuleManager" should {
    "redirect to login for resource /inventary/modules/:id" in new WithApplication {
      route(FakeRequest(GET, "/inventary/modules/" + bson.stringify)).map(
        r => {
          status(r) must equalTo(SEE_OTHER)
          header("Location", r) must equalTo(Some("/login"))
        }
      ).getOrElse(
          failure("Pas de retour de la fonction")
        )
    }

    "redirect to login for resource /inventary/modules/:id/module" in new WithApplication{
      route(FakeRequest(GET, "/inventary/modules/"+bson.stringify+"/module")).map(
        r=> {

          status(r) must equalTo(SEE_OTHER)
          header("Location", r) must equalTo(Some("/login"))
        }
      ).getOrElse(
          failure("Pas de retour de la fonction")
        )
    }

    "redirect to login for resource /inventary/modules/:id/module" in new WithApplication{
      route(FakeRequest(POST, "/inventary/modules/"+bson.stringify+"/module")).map(
        r=> {

          status(r) must equalTo(SEE_OTHER)
          header("Location", r) must equalTo(Some("/login"))
        }
      ).getOrElse(
          failure("Pas de retour de la fonction")
        )
    }
  }

  "When user is on resource /inventary/modules/:id, ModuleManager" should {

    "send 200 OK page with result" in new WithApplication{
      val f=fixture
      val typeModule=TypeModule(bson, "mod", "type")
      val list_module=List[Module](
        Module(bson2,"Id",bson,bson3,date,None,true,Some("v01"),true,None)
      )
      f.moduleDaoMock.findAll(any[JsObject],org.mockito.Matchers.eq(Json.obj("acquisition" -> -1)))(any[ExecutionContext]) returns future{list_module}
      f.typeModuleDaoMock.findById(org.mockito.Matchers.eq(bson))(any[ExecutionContext]) returns future{Some(typeModule)}
      f.firmwareDaoMock.findAll(any[JsObject],any[JsObject])(any[ExecutionContext]) returns  future{List[Firmware](Firmware(bson3,"firm","v02"))}

      val r=f.controller.inventary(bson.stringify,"acquisition",-1).apply(FakeRequest(GET,"/inventary/modules/"+bson.stringify).withSession("user" -> """{"login":"test"}"""))

      status(r) must equalTo(OK)
      contentType(r) must beSome.which(_ == "text/html")
      val content = contentAsString(r)
      content must contain("<title>Inventaire des modules</title>")
      content must contain("<td>Id</td>")
      content must contain("<td>22/04/2015</td>")
      content must contain("<td>-</td>")
      content must contain("<td> Oui </td>")
      content must contain("<td>firm (v02)</td>")
      content must contain("<td>v01</td>")
      content must contain("<td>Hors service</td>")

      there was one(f.moduleDaoMock).findAll(any[JsObject],org.mockito.Matchers.eq(Json.obj("acquisition" -> -1)))(any[ExecutionContext])
      there was one(f.typeModuleDaoMock).findById(org.mockito.Matchers.eq(bson))(any[ExecutionContext])
      there was one(f.firmwareDaoMock).findAll(any[JsObject],any[JsObject])(any[ExecutionContext])
    }

    "send 200 OK page with the message 'Aucun résultat trouvé', if not have modules" in new WithApplication {
      val f = fixture

      f.typeModuleDaoMock.findById(org.mockito.Matchers.eq(bson))(any[ExecutionContext]) returns future {
        Some(TypeModule(bson, "mod", "type"))
      }
      f.moduleDaoMock.findAll(any[JsObject],any[JsObject])(any[ExecutionContext]) returns future{List[Module]()}
      f.firmwareDaoMock.findAll(any[JsObject],any[JsObject])(any[ExecutionContext]) returns future{List[Firmware]()}

      val r = f.controller.inventary(bson.stringify).apply(FakeRequest(GET, "/inventary/modules/" + bson.stringify).withSession("user" -> """{"login":"test"}"""))

      status(r) must equalTo(OK)
      contentType(r) must beSome.which(_ == "text/html")
      val content = contentAsString(r)
      content must contain("<title>Inventaire des modules</title>")
      content must matchRegex("type\\s*/\\s*mod")
      content must contain("<h3 style=\"text-align:center\">Aucun résultat trouvé</h3>")
      content must matchRegex("<span class=\"bold\">\\s*Stocks\\s*</span>\\s*:\\s*0")

      there was one(f.typeModuleDaoMock).findById(org.mockito.Matchers.eq(bson))(any[ExecutionContext])
      there was one(f.moduleDaoMock).findAll(any[JsObject],any[JsObject])(any[ExecutionContext])
      there was one(f.firmwareDaoMock).findAll(any[JsObject],any[JsObject])(any[ExecutionContext])
    }

    "send 200 OK page with result" in new WithApplication{
      val f=fixture
      val typeModule=TypeModule(bson, "mod", "type")
      val list_module=List[Module](
        Module(bson2,"Id",bson,bson3,date,None,true,Some("v01"),true,None),
        Module(bson4,"Id2",bson,bson3,date,Some(date2), false,Some("v03"),false,None)
      )
      f.moduleDaoMock.findAll(any[JsObject],org.mockito.Matchers.eq(Json.obj("acquisition" -> -1)))(any[ExecutionContext]) returns future{list_module}
      f.typeModuleDaoMock.findById(org.mockito.Matchers.eq(bson))(any[ExecutionContext]) returns future{Some(typeModule)}
      f.firmwareDaoMock.findAll(any[JsObject],any[JsObject])(any[ExecutionContext]) returns  future{List[Firmware](Firmware(bson3,"firm","v02"))}

      val r=f.controller.inventary(bson.stringify,"acquisition",-1).apply(FakeRequest(GET,"/inventary/modules/"+bson.stringify).withSession("user" -> """{"login":"test"}"""))

      status(r) must equalTo(OK)
      contentType(r) must beSome.which(_ == "text/html")
      val content = contentAsString(r)
      content must contain("<title>Inventaire des modules</title>")
      content must contain("<td>Id</td>")
      content must contain("<td>22/04/2015</td>")
      content must contain("<td>-</td>")
      content must contain("<td> Oui </td>")
      content must contain("<td>firm (v02)</td>")
      content must contain("<td>v01</td>")
      content must contain("<td>Hors service</td>")

      content must contain("<td>Id2</td>")
      content must contain("<td>22/04/2015</td>")
      content must contain("<td>23/04/2015</td>")
      content must contain("<td> Non </td>")
      content must contain("<td>firm (v02)</td>")
      content must contain("<td>v03</td>")

      there was one(f.moduleDaoMock).findAll(any[JsObject],org.mockito.Matchers.eq(Json.obj("acquisition" -> -1)))(any[ExecutionContext])
      there was one(f.typeModuleDaoMock).findById(org.mockito.Matchers.eq(bson))(any[ExecutionContext])
      there was one(f.firmwareDaoMock).findAll(any[JsObject],any[JsObject])(any[ExecutionContext])
    }

    "send redirect if module type not found" in new WithApplication {
      val f = fixture

      f.typeModuleDaoMock.findById(org.mockito.Matchers.eq(bson))(any[ExecutionContext]) returns future {None}

      val r = f.controller.inventary(bson.stringify).apply(FakeRequest(GET, "/inventary/modules/" + bson.stringify).withSession("user" -> """{"login":"test"}"""))

      status(r) must equalTo(SEE_OTHER)
      header("Location",r) must beSome("/inventary/modules")

      there was one(f.typeModuleDaoMock).findById(org.mockito.Matchers.eq(bson))(any[ExecutionContext])
    }

    "send internal server error if mongoDB error when find module type" in new WithApplication {
      val f = fixture
      val futureMock=mock[Future[Option[TypeModule]]]
      val throwable=mock[Throwable]

      f.typeModuleDaoMock.findById(org.mockito.Matchers.eq(bson))(any[ExecutionContext]) returns futureMock
      futureMock.flatMap(any[Option[TypeModule]=>Future[Option[TypeModule]]])(any[ExecutionContext]) returns futureMock
      futureMock.recover(any[PartialFunction[Throwable,Result]])(any[ExecutionContext]) answers {value=>future{value.asInstanceOf[PartialFunction[Throwable,Result]](throwable)}}

      val r = f.controller.inventary(bson.stringify).apply(FakeRequest(GET, "/inventary/modules/" + bson.stringify).withSession("user" -> """{"login":"test"}"""))

      status(r) must equalTo(INTERNAL_SERVER_ERROR)

      there was one(f.typeModuleDaoMock).findById(org.mockito.Matchers.eq(bson))(any[ExecutionContext])
      there was one(futureMock).flatMap(any[Option[TypeModule]=>Future[Option[TypeModule]]])(any[ExecutionContext])
      there was one(futureMock).recover(any[PartialFunction[Throwable,Result]])(any[ExecutionContext])
    }

    "send internal server error if mongoDB error when find module" in new WithApplication{
      val f=fixture
      val typeModule=TypeModule(bson, "mod", "type")
      val futureMock=mock[Future[List[Module]]]
      val throwable=mock[Throwable]

      f.moduleDaoMock.findAll(any[JsObject],org.mockito.Matchers.eq(Json.obj("acquisition" -> -1)))(any[ExecutionContext]) returns futureMock
      f.typeModuleDaoMock.findById(org.mockito.Matchers.eq(bson))(any[ExecutionContext]) returns future{Some(typeModule)}
      f.firmwareDaoMock.findAll(any[JsObject],any[JsObject])(any[ExecutionContext]) returns  future{List[Firmware](Firmware(bson3,"firm","v02"))}
      futureMock.flatMap(any[List[Module]=>Future[List[Module]]])(any[ExecutionContext]) returns futureMock
      futureMock.recover(any[PartialFunction[Throwable,Result]])(any[ExecutionContext]) answers {value=>future{value.asInstanceOf[PartialFunction[Throwable,Result]](throwable)}}

      val r=f.controller.inventary(bson.stringify,"acquisition",-1).apply(FakeRequest(GET,"/inventary/modules/"+bson.stringify).withSession("user" -> """{"login":"test"}"""))

      status(r) must equalTo(INTERNAL_SERVER_ERROR)

      there was one(f.moduleDaoMock).findAll(any[JsObject],org.mockito.Matchers.eq(Json.obj("acquisition" -> -1)))(any[ExecutionContext])
      there was one(f.typeModuleDaoMock).findById(org.mockito.Matchers.eq(bson))(any[ExecutionContext])
      there was one(f.firmwareDaoMock).findAll(any[JsObject],any[JsObject])(any[ExecutionContext])
      there was one(futureMock).flatMap(any[List[Module]=>Future[List[Module]]])(any[ExecutionContext])
      there was one(futureMock).recover(any[PartialFunction[Throwable,Result]])(any[ExecutionContext])
    }

    "send internal server error if mongoDB error when find firmware" in new WithApplication{
      val f=fixture
      val typeModule=TypeModule(bson, "mod", "type")
      val futureMock=mock[Future[List[Firmware]]]
      val throwable=mock[Throwable]

      f.moduleDaoMock.findAll(any[JsObject],org.mockito.Matchers.eq(Json.obj("acquisition" -> -1)))(any[ExecutionContext]) returns future{List[Module]()}
      f.typeModuleDaoMock.findById(org.mockito.Matchers.eq(bson))(any[ExecutionContext]) returns future{Some(typeModule)}
      f.firmwareDaoMock.findAll(any[JsObject],any[JsObject])(any[ExecutionContext]) returns  futureMock
      futureMock.map(any[List[Firmware]=>List[Firmware]])(any[ExecutionContext]) returns futureMock
      futureMock.recover(any[PartialFunction[Throwable,Result]])(any[ExecutionContext]) answers {value=>future{value.asInstanceOf[PartialFunction[Throwable,Result]](throwable)}}

      val r=f.controller.inventary(bson.stringify,"acquisition",-1).apply(FakeRequest(GET,"/inventary/modules/"+bson.stringify).withSession("user" -> """{"login":"test"}"""))

      status(r) must equalTo(INTERNAL_SERVER_ERROR)

      there was one(f.moduleDaoMock).findAll(any[JsObject],org.mockito.Matchers.eq(Json.obj("acquisition" -> -1)))(any[ExecutionContext])
      there was one(f.typeModuleDaoMock).findById(org.mockito.Matchers.eq(bson))(any[ExecutionContext])
      there was one(f.firmwareDaoMock).findAll(any[JsObject],any[JsObject])(any[ExecutionContext])
      there was one(futureMock).map(any[List[Firmware]=>List[Firmware]])(any[ExecutionContext])
      there was one(futureMock).recover(any[PartialFunction[Throwable,Result]])(any[ExecutionContext])
    }
  }

  "When method printForm is called, ModuleManager" should{
    "print an empty form" in new WithApplication{
      val f=fixture

      f.moduleDaoMock.findApolline() returns future{Stream[BSONDocument]()}
      f.firmwareDaoMock.findFirmware() returns future{Stream[BSONDocument]()}
      f.firmwareDaoMock.findVersionFirmware() returns future{Stream[BSONDocument]()}

      val req=FakeRequest(GET, "/inventary/modules/"+bson.stringify+"/module").withSession("user" -> """{"login":"test"}""")
      val action = Action.async{implicit request=>f.controller.printForm(Results.Ok,bson.stringify,ModuleManager.form,mock[Call])}
      val r=call(action,req)

      status(r) must equalTo(OK)
      contentType(r) must beSome.which(_ == "text/html")
      val content = contentAsString(r)
      content must contain("<title>Inventaire des modules</title>")
      content must contain("<input type=\"text\" id=\"id\" name=\"id\" value=\"\" class=\"form-control\"/>")
      content must contain("<input type=\"date\" id=\"acquisition\" name=\"acquisition\" value=\"\" class=\"form-control\"/>")
      content must contain("<input type=\"date\" id=\"firstUse\" name=\"firstUse\" value=\"\" class=\"form-control\"/>")
      content must contain("<input type=\"checkbox\" id=\"agregateur\" name=\"agregateur\" value=\"true\"  />")
      content must contain("<input type=\"text\" id=\"apolline\" name=\"apolline\" class=\"form-control\" value=\"\" autocomplete=\"off\" list=\"list_apolline\"/>")
      content must contain("<input type=\"text\" id=\"firmware\" name=\"firmware\" class=\"form-control\" value=\"\" autocomplete=\"off\" list=\"list_firmware\"/>")
      content must contain("<input type=\"text\" id=\"versionFirmware\" name=\"versionFirmware\" class=\"form-control\" value=\"\" autocomplete=\"off\" list=\"list_versionFirmware\"/>")
      content must contain("<input type=\"checkbox\" id=\"hs\" name=\"hs\" value=\"true\"  />")
      content must contain("<textarea id=\"commentaire\" name=\"commentaire\" class=\"form-control\"></textarea>")

      there was one(f.moduleDaoMock).findApolline()
      there was one(f.firmwareDaoMock).findFirmware()
      there was one(f.firmwareDaoMock).findVersionFirmware()
    }

    "Send 500 internal error if mongoDB Error when find apolline data" in new WithApplication {
      val f = fixture
      val futureMock=mock[Future[Stream[BSONDocument]]]
      val throwable=mock[Throwable]

      f.moduleDaoMock.findApolline() returns futureMock
      f.firmwareDaoMock.findFirmware() returns future{Stream[BSONDocument]()}
      f.firmwareDaoMock.findVersionFirmware() returns future {Stream[BSONDocument]()}
      futureMock.map(any[Stream[BSONDocument]=>Stream[BSONDocument]])(any[ExecutionContext]) returns futureMock
      futureMock.recover(any[PartialFunction[Throwable,Result]])(any[ExecutionContext]) answers {value => future{value.asInstanceOf[PartialFunction[Throwable,Result]](throwable)}}

      val req = FakeRequest(GET, "/inventary/modules/" + bson.stringify + "/module").withSession("user" -> """{"login":"test"}""")
      val action = Action.async { implicit request => f.controller.printForm(Results.Ok, bson.stringify, ModuleManager.form, mock[Call]) }
      val r = call(action, req)

      status(r) must equalTo(INTERNAL_SERVER_ERROR)

      there was one(f.moduleDaoMock).findApolline()
      there was one(f.firmwareDaoMock).findFirmware()
      there was one(f.firmwareDaoMock).findVersionFirmware()
      there was one(futureMock).map(any[Stream[BSONDocument]=>Stream[BSONDocument]])(any[ExecutionContext])
      there was one(futureMock).recover(any[PartialFunction[Throwable,Result]])(any[ExecutionContext])
    }

    "Send 500 internal error if mongoDB Error when find all firmware" in new WithApplication {
      val f = fixture
      val futureMock=mock[Future[Stream[BSONDocument]]]
      val throwable=mock[Throwable]

      f.moduleDaoMock.findApolline() returns future{Stream[BSONDocument]()}
      f.firmwareDaoMock.findFirmware() returns futureMock
      f.firmwareDaoMock.findVersionFirmware() returns future {Stream[BSONDocument]()}
      futureMock.flatMap(any[Stream[BSONDocument]=>Future[Stream[BSONDocument]]])(any[ExecutionContext]) returns futureMock
      futureMock.recover(any[PartialFunction[Throwable,Result]])(any[ExecutionContext]) answers {value => future{value.asInstanceOf[PartialFunction[Throwable,Result]](throwable)}}

      val req = FakeRequest(GET, "/inventary/modules/" + bson.stringify + "/module").withSession("user" -> """{"login":"test"}""")
      val action = Action.async { implicit request => f.controller.printForm(Results.Ok, bson.stringify, ModuleManager.form, mock[Call]) }
      val r = call(action, req)

      status(r) must equalTo(INTERNAL_SERVER_ERROR)

      there was one(f.moduleDaoMock).findApolline()
      there was one(f.firmwareDaoMock).findFirmware()
      there was one(f.firmwareDaoMock).findVersionFirmware()
      there was one(futureMock).flatMap(any[Stream[BSONDocument]=>Future[Stream[BSONDocument]]])(any[ExecutionContext])
      there was one(futureMock).recover(any[PartialFunction[Throwable,Result]])(any[ExecutionContext])
    }

    "Send 500 internal error if mongoDB Error when find all firmware version" in new WithApplication {
      val f = fixture
      val futureMock=mock[Future[Stream[BSONDocument]]]
      val throwable=mock[Throwable]

      f.moduleDaoMock.findApolline() returns future{Stream[BSONDocument]()}
      f.firmwareDaoMock.findFirmware() returns future{Stream[BSONDocument]()}
      f.firmwareDaoMock.findVersionFirmware() returns futureMock
      futureMock.flatMap(any[Stream[BSONDocument]=>Future[Stream[BSONDocument]]])(any[ExecutionContext]) returns futureMock
      futureMock.recover(any[PartialFunction[Throwable,Result]])(any[ExecutionContext]) answers {value => future{value.asInstanceOf[PartialFunction[Throwable,Result]](throwable)}}

      val req = FakeRequest(GET, "/inventary/modules/" + bson.stringify + "/module").withSession("user" -> """{"login":"test"}""")
      val action = Action.async { implicit request => f.controller.printForm(Results.Ok, bson.stringify, ModuleManager.form, mock[Call]) }
      val r = call(action, req)

      status(r) must equalTo(INTERNAL_SERVER_ERROR)

      there was one(f.moduleDaoMock).findApolline()
      there was one(f.firmwareDaoMock).findFirmware()
      there was one(f.firmwareDaoMock).findVersionFirmware()
      there was one(futureMock).flatMap(any[Stream[BSONDocument]=>Future[Stream[BSONDocument]]])(any[ExecutionContext])
      there was one(futureMock).recover(any[PartialFunction[Throwable,Result]])(any[ExecutionContext])
    }
  }

  "When method printFormWIthData is called, ModuleManager" should{

    "send bad_request with the form if method type not exist" in new WithApplication{
      val f=fixture
      val func=mock[(Module,Firmware)=>ModuleForm]
      val routeCall=mock[Call]

      f.applyNotFoundFunction()
      f.moduleDaoMock.findApolline() returns future{Stream[BSONDocument]()}
      f.firmwareDaoMock.findFirmware() returns future{Stream[BSONDocument]()}
      f.firmwareDaoMock.findVersionFirmware() returns future{Stream[BSONDocument]()}

      val req=FakeRequest(GET,"/inventary/modules/"+bson.stringify+"/"+bson2.stringify).withSession("user" -> """{"login":"test"}""")
      val action=Action.async(implicit request => f.controller.printFormWithData(bson.stringify,bson2.stringify,routeCall)(func))
      val r=call(action,req)

      status(r) must equalTo(BAD_REQUEST)
      contentType(r) must beSome.which(_ == "text/html")
      val content = contentAsString(r)
      content must contain("<title>Inventaire des modules</title>")
      content must contain("Ce type de module n&#x27;existe pas")

      there was one(f.typeModuleManagerMock).doIfTypeModuleFound(org.mockito.Matchers.eq(bson))(any[Unit=>Future[Result]])(any[Unit=>Future[Result]])
      there was one(f.moduleDaoMock).findApolline()
      there was one(f.firmwareDaoMock).findFirmware()
      there was one(f.firmwareDaoMock).findVersionFirmware()
    }

    "send 200 Ok page with a prefilled form" in new WithApplication{
      val f=fixture
      val func=mock[(Module,Firmware)=>ModuleForm]
      val module=Module(bson2,"Id",bson,bson3,date,Some(date2),true,Some("v01"),true,Some("un com"))
      val firmware=Firmware(bson3,"firm","v02")
      val routeCall=mock[Call]

      f.applyFoundFunction()
      f.moduleDaoMock.findById(org.mockito.Matchers.eq(bson2))(any[ExecutionContext]) returns future{Some(module)}
      f.firmwareDaoMock.findById(org.mockito.Matchers.eq(bson3))(any[ExecutionContext]) returns future{Some(firmware)}
      func.apply(org.mockito.Matchers.eq(module),org.mockito.Matchers.eq(firmware)) returns ModuleForm("Id",date,Some(date2),true,Some("v01"),"firm","v02",true,Some("un com"),"")
      f.moduleDaoMock.findApolline() returns future{Stream[BSONDocument]()}
      f.firmwareDaoMock.findFirmware() returns future{Stream[BSONDocument]()}
      f.firmwareDaoMock.findVersionFirmware() returns future{Stream[BSONDocument]()}

      val req=FakeRequest(GET,"/inventary/modules/"+bson.stringify+"/"+bson2.stringify).withSession("user" -> """{"login":"test"}""")
      val action=Action.async(implicit request => f.controller.printFormWithData(bson.stringify,bson2.stringify,routeCall)(func))
      val r=call(action,req)

      status(r) must equalTo(OK)
      contentType(r) must beSome.which(_ == "text/html")
      val content = contentAsString(r)
      content must contain("<title>Inventaire des modules</title>")
      content must contain("<input type=\"text\" id=\"id\" name=\"id\" value=\"Id\" class=\"form-control\"/>")
      content must contain("<input type=\"date\" id=\"acquisition\" name=\"acquisition\" value=\"2015-04-22\" class=\"form-control\"/>")
      content must contain("<input type=\"date\" id=\"firstUse\" name=\"firstUse\" value=\"2015-04-23\" class=\"form-control\"/>")
      content must contain("<input type=\"checkbox\" id=\"agregateur\" name=\"agregateur\" value=\"true\" checked=\"checked\" />")
      content must contain("<input type=\"text\" id=\"apolline\" name=\"apolline\" class=\"form-control\" value=\"v01\" autocomplete=\"off\" list=\"list_apolline\"/>")
      content must contain("<input type=\"text\" id=\"firmware\" name=\"firmware\" class=\"form-control\" value=\"firm\" autocomplete=\"off\" list=\"list_firmware\"/>")
      content must contain("<input type=\"text\" id=\"versionFirmware\" name=\"versionFirmware\" class=\"form-control\" value=\"v02\" autocomplete=\"off\" list=\"list_versionFirmware\"/>")
      content must contain("<input type=\"checkbox\" id=\"hs\" name=\"hs\" value=\"true\" checked=\"checked\" />")
      content must contain("<textarea id=\"commentaire\" name=\"commentaire\" class=\"form-control\">un com</textarea>")

      there was one(f.typeModuleManagerMock).doIfTypeModuleFound(org.mockito.Matchers.eq(bson))(any[Unit=>Future[Result]])(any[Unit=>Future[Result]])
      there was one(f.moduleDaoMock).findById(org.mockito.Matchers.eq(bson2))(any[ExecutionContext])
      there was one(f.firmwareDaoMock).findById(org.mockito.Matchers.eq(bson3))(any[ExecutionContext])
      there was one(func).apply(org.mockito.Matchers.eq(module),org.mockito.Matchers.eq(firmware))
      there was one(f.moduleDaoMock).findApolline()
      there was one(f.firmwareDaoMock).findFirmware()
      there was one(f.firmwareDaoMock).findVersionFirmware()
    }

    "send redirect if module not found" in new WithApplication{
      val f=fixture
      val func=mock[(Module,Firmware)=>ModuleForm]
      val routeCall=mock[Call]

      f.applyFoundFunction()
      f.moduleDaoMock.findById(org.mockito.Matchers.eq(bson2))(any[ExecutionContext]) returns future{None}

      val req=FakeRequest(GET,"/inventary/modules/"+bson.stringify+"/"+bson2.stringify).withSession("user" -> """{"login":"test"}""")
      val action=Action.async(implicit request => f.controller.printFormWithData(bson.stringify,bson2.stringify,routeCall)(func))
      val r=call(action,req)

      status(r) must equalTo(SEE_OTHER)
      header("Location",r) must beSome("/inventary/modules/"+bson.stringify)

      there was one(f.typeModuleManagerMock).doIfTypeModuleFound(org.mockito.Matchers.eq(bson))(any[Unit=>Future[Result]])(any[Unit=>Future[Result]])
      there was one(f.moduleDaoMock).findById(org.mockito.Matchers.eq(bson2))(any[ExecutionContext])
    }

    "send redirect if firmware not found" in new WithApplication{
      val f=fixture
      val func=mock[(Module,Firmware)=>ModuleForm]
      val module=Module(bson2,"Id",bson,bson3,date,Some(date2),true,Some("v01"),true,Some("un com"))
      val routeCall=mock[Call]

      f.applyFoundFunction()
      f.moduleDaoMock.findById(org.mockito.Matchers.eq(bson2))(any[ExecutionContext]) returns future{Some(module)}
      f.firmwareDaoMock.findById(org.mockito.Matchers.eq(bson3))(any[ExecutionContext]) returns future{None}

      val req=FakeRequest(GET,"/inventary/modules/"+bson.stringify+"/"+bson2.stringify).withSession("user" -> """{"login":"test"}""")
      val action=Action.async(implicit request => f.controller.printFormWithData(bson.stringify,bson2.stringify,routeCall)(func))
      val r=call(action,req)

      status(r) must equalTo(SEE_OTHER)
      header("Location",r) must beSome("/inventary/modules/"+bson.stringify)

      there was one(f.typeModuleManagerMock).doIfTypeModuleFound(org.mockito.Matchers.eq(bson))(any[Unit=>Future[Result]])(any[Unit=>Future[Result]])
      there was one(f.moduleDaoMock).findById(org.mockito.Matchers.eq(bson2))(any[ExecutionContext])
      there was one(f.firmwareDaoMock).findById(org.mockito.Matchers.eq(bson3))(any[ExecutionContext])
    }

    "send 500 internal server error if have mongoDB error when find module" in new WithApplication{
      val f=fixture
      val func=mock[(Module,Firmware)=>ModuleForm]
      val futureMock=mock[Future[Option[Module]]]
      val routeCall=mock[Call]
      val throwable=mock[Throwable]

      f.applyFoundFunction()
      f.moduleDaoMock.findById(org.mockito.Matchers.eq(bson2))(any[ExecutionContext]) returns futureMock
      futureMock.flatMap(any[Option[Module]=>Future[Option[Module]]])(any[ExecutionContext]) returns futureMock
      futureMock.recover(any[PartialFunction[Throwable,Result]])(any[ExecutionContext]) answers {value=>future{value.asInstanceOf[PartialFunction[Throwable,Result]](throwable)}}

      val req=FakeRequest(GET,"/inventary/modules/"+bson.stringify+"/"+bson2.stringify).withSession("user" -> """{"login":"test"}""")
      val action=Action.async(implicit request => f.controller.printFormWithData(bson.stringify,bson2.stringify,routeCall)(func))
      val r=call(action,req)

      status(r) must equalTo(INTERNAL_SERVER_ERROR)

      there was one(f.typeModuleManagerMock).doIfTypeModuleFound(org.mockito.Matchers.eq(bson))(any[Unit=>Future[Result]])(any[Unit=>Future[Result]])
      there was one(f.moduleDaoMock).findById(org.mockito.Matchers.eq(bson2))(any[ExecutionContext])
      there was one(futureMock).flatMap(any[Option[Module]=>Future[Option[Module]]])(any[ExecutionContext])
      there was one(futureMock).recover(any[PartialFunction[Throwable,Result]])(any[ExecutionContext])
    }


    "send 500 internal server error if have mongoDB error when find firmware" in new WithApplication{
      val f=fixture
      val func=mock[(Module,Firmware)=>ModuleForm]
      val module=Module(bson2,"Id",bson,bson3,date,Some(date2),true,Some("v01"),true,Some("un com"))
      val futureMock=mock[Future[Option[Firmware]]]
      val routeCall=mock[Call]
      val throwable=mock[Throwable]

      f.applyFoundFunction()
      f.moduleDaoMock.findById(org.mockito.Matchers.eq(bson2))(any[ExecutionContext]) returns future{Some(module)}
      f.firmwareDaoMock.findById(org.mockito.Matchers.eq(bson3))(any[ExecutionContext]) returns futureMock
      futureMock.flatMap(any[Option[Firmware]=>Future[Option[Firmware]]])(any[ExecutionContext]) returns futureMock
      futureMock.recover(any[PartialFunction[Throwable,Result]])(any[ExecutionContext]) answers {value=>future{value.asInstanceOf[PartialFunction[Throwable,Result]](throwable)}}

      val req=FakeRequest(GET,"/inventary/modules/"+bson.stringify+"/"+bson2.stringify).withSession("user" -> """{"login":"test"}""")
      val action=Action.async(implicit request => f.controller.printFormWithData(bson.stringify,bson2.stringify,routeCall)(func))
      val r=call(action,req)

      status(r) must equalTo(INTERNAL_SERVER_ERROR)

      there was one(f.typeModuleManagerMock).doIfTypeModuleFound(org.mockito.Matchers.eq(bson))(any[Unit=>Future[Result]])(any[Unit=>Future[Result]])
      there was one(f.moduleDaoMock).findById(org.mockito.Matchers.eq(bson2))(any[ExecutionContext])
      there was one(f.firmwareDaoMock).findById(org.mockito.Matchers.eq(bson3))(any[ExecutionContext])
      there was one(futureMock).flatMap(any[Option[Firmware]=>Future[Option[Firmware]]])(any[ExecutionContext])
      there was one(futureMock).recover(any[PartialFunction[Throwable,Result]])(any[ExecutionContext])
    }
  }

  "When method insertFirmwareIfNotFound is called, ModuleManager" should{
    "applied function in parameter if firmware exist" in new WithApplication{
      val f=fixture
      val moduleForm=mock[ModuleForm]
      val func=mock[(ModuleForm,BSONObjectID)=>Future[Result]]
      val firmware=Firmware(bson,"firm","v01")

      moduleForm.firmware returns "firm"
      moduleForm.versionFirmware returns "v01"
      f.firmwareDaoMock.findOne(org.mockito.Matchers.eq(Json.obj("nom"->"firm","version"->"v01")))(any[ExecutionContext]) returns future{Some(firmware)}
      func.apply(org.mockito.Matchers.eq(moduleForm),org.mockito.Matchers.eq(bson)) returns future{Results.Ok("exec func")}

      val req=FakeRequest(GET, "/inventary/modules/" + bson.stringify).withSession("user" -> """{"login":"test"}""")
      val action=Action.async{f.controller.insertFirmwareIfNotFound(moduleForm,func)}
      val r=call(action,req)

      status(r) must beEqualTo(OK)
      contentAsString(r) must beEqualTo("exec func")

      there was one(moduleForm).firmware
      there was one(moduleForm).versionFirmware
      there was one(f.firmwareDaoMock).findOne(org.mockito.Matchers.eq(Json.obj("nom"->"firm","version"->"v01")))(any[ExecutionContext])
      there was one(func).apply(org.mockito.Matchers.eq(moduleForm),org.mockito.Matchers.eq(bson))
    }

    "insert firmware into the database and applied function in parameter if firmware not exist" in new WithApplication{
      val f=fixture
      val moduleForm=mock[ModuleForm]
      val func=mock[(ModuleForm,BSONObjectID)=>Future[Result]]
      val lastError=mock[LastError]

      moduleForm.firmware returns "firm"
      moduleForm.versionFirmware returns "v01"
      f.firmwareDaoMock.findOne(org.mockito.Matchers.eq(Json.obj("nom"->"firm","version"->"v01")))(any[ExecutionContext]) returns future{None}
      f.firmwareDaoMock.insert(any[Firmware],any[GetLastError])(any[ExecutionContext]) returns future{lastError}
      func.apply(org.mockito.Matchers.eq(moduleForm),any[BSONObjectID]) returns future{Results.Ok("exec func")}

      val req=FakeRequest(GET, "/inventary/modules/" + bson.stringify).withSession("user" -> """{"login":"test"}""")
      val action=Action.async{f.controller.insertFirmwareIfNotFound(moduleForm,func)}
      val r=call(action,req)

      println(contentAsString(r))
      status(r) must beEqualTo(OK)
      contentAsString(r) must beEqualTo("exec func")

      there was 2.times(moduleForm).firmware
      there was 2.times(moduleForm).versionFirmware
      there was one(f.firmwareDaoMock).findOne(org.mockito.Matchers.eq(Json.obj("nom"->"firm","version"->"v01")))(any[ExecutionContext])
      there was one(f.firmwareDaoMock).insert(any[Firmware],any[GetLastError])(any[ExecutionContext])
      there was one(func).apply(org.mockito.Matchers.eq(moduleForm),any[BSONObjectID])
    }

    "send 500 internal error if mongoDB error when find firmware" in new WithApplication{
      val f=fixture
      val moduleForm=mock[ModuleForm]
      val func=mock[(ModuleForm,BSONObjectID)=>Future[Result]]
      val futureMock=mock[Future[Option[Firmware]]]
      val throwable=mock[Throwable]

      moduleForm.firmware returns "firm"
      moduleForm.versionFirmware returns "v01"
      f.firmwareDaoMock.findOne(org.mockito.Matchers.eq(Json.obj("nom"->"firm","version"->"v01")))(any[ExecutionContext]) returns futureMock
      futureMock.flatMap(any[Option[Firmware]=>Future[Option[Firmware]]])(any[ExecutionContext]) returns futureMock
      futureMock.recover(any[PartialFunction[Throwable,Result]])(any[ExecutionContext]) answers {value => future{value.asInstanceOf[PartialFunction[Throwable,Result]](throwable)}}

      val req=FakeRequest(GET, "/inventary/modules/" + bson.stringify).withSession("user" -> """{"login":"test"}""")
      val action=Action.async{f.controller.insertFirmwareIfNotFound(moduleForm,func)}
      val r=call(action,req)

      status(r) must beEqualTo(INTERNAL_SERVER_ERROR)

      there was one(moduleForm).firmware
      there was one(moduleForm).versionFirmware
      there was one(f.firmwareDaoMock).findOne(org.mockito.Matchers.eq(Json.obj("nom"->"firm","version"->"v01")))(any[ExecutionContext])
      there was one(futureMock).flatMap(any[Option[Firmware]=>Future[Option[Firmware]]])(any[ExecutionContext])
      there was one(futureMock).recover(any[PartialFunction[Throwable,Result]])(any[ExecutionContext])
    }

    "send 500 internal error if mongoDB error when insert firmware" in new WithApplication{
      val f=fixture
      val moduleForm=mock[ModuleForm]
      val func=mock[(ModuleForm,BSONObjectID)=>Future[Result]]
      val futureMock=mock[Future[LastError]]
      val throwable=mock[Throwable]

      moduleForm.firmware returns "firm"
      moduleForm.versionFirmware returns "v01"
      f.firmwareDaoMock.findOne(org.mockito.Matchers.eq(Json.obj("nom"->"firm","version"->"v01")))(any[ExecutionContext]) returns future{None}
      f.firmwareDaoMock.insert(any[Firmware],any[GetLastError])(any[ExecutionContext]) returns futureMock
      futureMock.flatMap(any[LastError=>Future[LastError]])(any[ExecutionContext]) returns futureMock
      futureMock.recover(any[PartialFunction[Throwable,Result]])(any[ExecutionContext]) answers {value => future{value.asInstanceOf[PartialFunction[Throwable,Result]](throwable)}}

      val req=FakeRequest(GET, "/inventary/modules/" + bson.stringify).withSession("user" -> """{"login":"test"}""")
      val action=Action.async{f.controller.insertFirmwareIfNotFound(moduleForm,func)}
      val r=call(action,req)

      status(r) must beEqualTo(INTERNAL_SERVER_ERROR)

      there was 2.times(moduleForm).firmware
      there was 2.times(moduleForm).versionFirmware
      there was one(f.firmwareDaoMock).findOne(org.mockito.Matchers.eq(Json.obj("nom"->"firm","version"->"v01")))(any[ExecutionContext])
      there was one(f.firmwareDaoMock).insert(any[Firmware],any[GetLastError])(any[ExecutionContext])
      there was one(futureMock).flatMap(any[LastError=>Future[LastError]])(any[ExecutionContext])
      there was one(futureMock).recover(any[PartialFunction[Throwable,Result]])(any[ExecutionContext])
    }
  }

  "When user submit a form, SensorManager" should{
    "send bad_request with the form if module type not exist" in new WithApplication{
      val f=fixture
      val func=mock[(ModuleForm,BSONObjectID)=>Future[Result]]
      val funcVerif=mock[ModuleForm=>JsObject]
      val routeCall=mock[Call]

      f.applyNotFoundFunction()
      f.moduleDaoMock.findApolline() returns future{Stream[BSONDocument]()}
      f.firmwareDaoMock.findFirmware() returns future{Stream[BSONDocument]()}
      f.firmwareDaoMock.findVersionFirmware() returns future{Stream[BSONDocument]()}

      val req=FakeRequest(POST,"/inventary/modules/"+bson.stringify+"/module").withSession("user" -> """{"login":"test"}""")
      val action=Action.async(implicit request => f.controller.submitForm(bson.stringify,routeCall)(funcVerif)(func))
      val r=call(action,req)

      status(r) must equalTo(BAD_REQUEST)
      contentType(r) must beSome.which(_ == "text/html")
      val content = contentAsString(r)
      content must contain("<title>Inventaire des modules</title>")
      content must contain("Ce type de module n&#x27;existe pas")

      there was one(f.typeModuleManagerMock).doIfTypeModuleFound(org.mockito.Matchers.eq(bson))(any[Unit=>Future[Result]])(any[Unit=>Future[Result]])
      there was one(f.moduleDaoMock).findApolline()
      there was one(f.firmwareDaoMock).findFirmware()
      there was one(f.firmwareDaoMock).findVersionFirmware()
    }

    "send bad request when the form was submit with empty fields" in new WithApplication{
      val f=fixture
      val routeCall=mock[Call]
      val funcVerif=mock[ModuleForm=>JsObject]
      val func=mock[(ModuleForm,BSONObjectID)=>Future[Result]]

      f.applyFoundFunction()
      f.moduleDaoMock.findApolline() returns future{Stream[BSONDocument]()}
      f.firmwareDaoMock.findFirmware() returns future{Stream[BSONDocument]()}
      f.firmwareDaoMock.findVersionFirmware() returns future{Stream[BSONDocument]()}

      val req=FakeRequest(POST,"/inventary/modules/"+bson.stringify+"/module").withSession("user" -> """{"login":"test"}""")
      val action=Action.async(implicit request => f.controller.submitForm(bson.stringify,routeCall)(funcVerif)(func))
      val r=call(action,req)

      status(r) must equalTo(BAD_REQUEST)
      contentType(r) must beSome.which(_ == "text/html")
      val content = contentAsString(r)
      content must contain("<title>Inventaire des modules</title>")
      content must contains("<span class=\"control-label errors\">This field is required</span>",4)
      content must not contain("<span class=\"control-label errors\">Valid date required</span>")

      there was one(f.typeModuleManagerMock).doIfTypeModuleFound(org.mockito.Matchers.eq(bson))(any[Unit=>Future[Result]])(any[Unit=>Future[Result]])
      there was one(f.moduleDaoMock).findApolline()
      there was one(f.firmwareDaoMock).findFirmware()
      there was one(f.firmwareDaoMock).findVersionFirmware()
    }

    "send bad request when the form was submit with not valid date" in new WithApplication{
      val f=fixture
      val routeCall=mock[Call]
      val funcVerif=mock[ModuleForm=>JsObject]
      val func=mock[(ModuleForm,BSONObjectID)=>Future[Result]]
      val data=Json.parse("""{"id":"Id","acquisition":"a","firstUse":"a","firmware":"firm","versionFirmware":"v01","send":"submit"}""")

      f.applyFoundFunction()
      f.moduleDaoMock.findApolline() returns future{Stream[BSONDocument]()}
      f.firmwareDaoMock.findFirmware() returns future{Stream[BSONDocument]()}
      f.firmwareDaoMock.findVersionFirmware() returns future{Stream[BSONDocument]()}

      val req=FakeRequest(POST,"/inventary/modules/"+bson.stringify+"/module").withJsonBody(data).withSession("user" -> """{"login":"test"}""")
      val action=Action.async(implicit request => f.controller.submitForm(bson.stringify,routeCall)(funcVerif)(func))
      val r=call(action,req)

      status(r) must equalTo(BAD_REQUEST)
      contentType(r) must beSome.which(_ == "text/html")
      val content = contentAsString(r)
      content must contain("<title>Inventaire des modules</title>")
      content must not contain("<span class=\"control-label errors\">This field is required</span>")
      content must contains("<span class=\"control-label errors\">Valid date required</span>",2)

      there was one(f.typeModuleManagerMock).doIfTypeModuleFound(org.mockito.Matchers.eq(bson))(any[Unit=>Future[Result]])(any[Unit=>Future[Result]])
      there was one(f.moduleDaoMock).findApolline()
      there was one(f.firmwareDaoMock).findFirmware()
      there was one(f.firmwareDaoMock).findVersionFirmware()
    }

    "Verify if the acquisition date is before first use date" in new WithApplication{
      val f=fixture
      val formMock=mock[Form[ModuleForm]]
      val formMock2=mock[Form[ModuleForm]]
      val moduleForm=ModuleForm("",date2,Some(date),false,None,"","",false,None,"")

      formMock.withError(org.mockito.Matchers.eq("firstUse"),org.mockito.Matchers.eq("La date de première utilisation doit être supèrieur à la date d'acquisition"),any[Array[Any]]) returns formMock2

      val ret=f.controller.verifyErrorAcquisitionAfterFirstUse(moduleForm,formMock)

      ret must equalTo(formMock2)

      there was one(formMock).withError(org.mockito.Matchers.eq("firstUse"),org.mockito.Matchers.eq("La date de première utilisation doit être supèrieur à la date d'acquisition"),any[Array[Any]])
    }

    "send bad request when the form was submit with date error" in new WithApplication{
      val f=fixture
      val routeCall=mock[Call]
      val funcVerif=mock[ModuleForm=>JsObject]
      val func=mock[(ModuleForm,BSONObjectID)=>Future[Result]]
      val data=Json.parse("""{"id":"Id","acquisition":"2015-04-23","firstUse":"2015-04-22","firmware":"firm","versionFirmware":"v01","send":"submit"}""")

      f.applyFoundFunction()
      f.moduleDaoMock.findApolline() returns future{Stream[BSONDocument]()}
      f.firmwareDaoMock.findFirmware() returns future{Stream[BSONDocument]()}
      f.firmwareDaoMock.findVersionFirmware() returns future{Stream[BSONDocument]()}

      val req=FakeRequest(POST,"/inventary/modules/"+bson.stringify+"/module").withJsonBody(data).withSession("user" -> """{"login":"test"}""")
      val action=Action.async(implicit request => f.controller.submitForm(bson.stringify,routeCall)(funcVerif)(func))
      val r=call(action,req)

      status(r) must equalTo(BAD_REQUEST)
      contentType(r) must beSome.which(_ == "text/html")
      val content = contentAsString(r)
      content must contain("<title>Inventaire des modules</title>")
      content must not contain("<span class=\"control-panel errors\">This field is required</span>")
      content must contain("<span class=\"control-label errors\">La date de première utilisation doit être supèrieur à la date d&#x27;acquisition</span>")

      there was one(f.typeModuleManagerMock).doIfTypeModuleFound(org.mockito.Matchers.eq(bson))(any[Unit=>Future[Result]])(any[Unit=>Future[Result]])
      there was one(f.moduleDaoMock).findApolline()
      there was one(f.firmwareDaoMock).findFirmware()
      there was one(f.firmwareDaoMock).findVersionFirmware()
    }

    "send 500 internal error if mongoDB error when find module" in new WithApplication{
      val f=fixture
      val routeCall=mock[Call]
      val funcVerif=mock[ModuleForm=>JsObject]
      val func=mock[(ModuleForm,BSONObjectID)=>Future[Result]]
      val data=Json.parse("""{"id":"Id","acquisition":"2015-04-22","firmware":"firm","versionFirmware":"v01","send":"submit"}""")
      val futureMock=mock[Future[Option[Module]]]
      val throwable=mock[Throwable]

      f.applyFoundFunction()
      f.moduleDaoMock.findOne(any[JsObject])(any[ExecutionContext]) returns futureMock
      funcVerif.apply(any[ModuleForm]) returns Json.obj()
      futureMock.flatMap(any[Option[Module]=>Future[Option[Module]]])(any[ExecutionContext]) returns futureMock
      futureMock.recover(any[PartialFunction[Throwable,Result]])(any[ExecutionContext]) answers (value=>future{value.asInstanceOf[PartialFunction[Throwable,Result]](throwable)})

      val req=FakeRequest(POST,"/inventary/modules/"+bson.stringify+"/module").withJsonBody(data).withSession("user" -> """{"login":"test"}""")
      val action=Action.async(implicit request => f.controller.submitForm(bson.stringify,routeCall)(funcVerif)(func))
      val r=call(action,req)

      status(r) must equalTo(INTERNAL_SERVER_ERROR)

      there was one(f.typeModuleManagerMock).doIfTypeModuleFound(org.mockito.Matchers.eq(bson))(any[Unit=>Future[Result]])(any[Unit=>Future[Result]])
      there was one(f.moduleDaoMock).findOne(any[JsObject])(any[ExecutionContext])
      there was one(funcVerif).apply(any[ModuleForm])
      there was one(futureMock).flatMap(any[Option[Module]=>Future[Option[Module]]])(any[ExecutionContext])
      there was one(futureMock).recover(any[PartialFunction[Throwable,Result]])(any[ExecutionContext])
    }

    "send bad_request if module exist" in new WithApplication{
      val f=fixture
      val routeCall=mock[Call]
      val funcVerif=mock[ModuleForm=>JsObject]
      val func=mock[(ModuleForm,BSONObjectID)=>Future[Result]]
      val data=Json.parse("""{"id":"Id","acquisition":"2015-04-22","firmware":"firm","versionFirmware":"v01","send":"submit"}""")
      val futureMock=mock[Future[Option[Module]]]
      val module=mock[Module]

      f.applyFoundFunction()
      f.moduleDaoMock.findOne(any[JsObject])(any[ExecutionContext]) returns future{Some(module)}
      funcVerif.apply(any[ModuleForm]) returns Json.obj()
      f.moduleDaoMock.findApolline() returns future{Stream[BSONDocument]()}
      f.firmwareDaoMock.findFirmware() returns future{Stream[BSONDocument]()}
      f.firmwareDaoMock.findVersionFirmware() returns future{Stream[BSONDocument]()}

      val req=FakeRequest(POST,"/inventary/modules/"+bson.stringify+"/module").withJsonBody(data).withSession("user" -> """{"login":"test"}""")
      val action=Action.async(implicit request => f.controller.submitForm(bson.stringify,routeCall)(funcVerif)(func))
      val r=call(action,req)

      status(r) must equalTo(BAD_REQUEST)
      val content=contentAsString(r)
      content must contain("<div class=\"alert alert-danger\" role=\"alert\">Ce module existe déjà</div>")

      there was one(f.typeModuleManagerMock).doIfTypeModuleFound(org.mockito.Matchers.eq(bson))(any[Unit=>Future[Result]])(any[Unit=>Future[Result]])
      there was one(f.moduleDaoMock).findOne(any[JsObject])(any[ExecutionContext])
      there was one(funcVerif).apply(any[ModuleForm])
      there was one(f.moduleDaoMock).findApolline()
      there was one(f.firmwareDaoMock).findFirmware()
      there was one(f.firmwareDaoMock).findVersionFirmware()
    }
  }

  "When user is on the resource /inventary/modules/:id/module , ModuleManager" should {
    "send 200 on OK with an empty form" in new WithApplication {
      val f=fixture

      f.applyFoundFunction()
      f.moduleDaoMock.findApolline() returns future{Stream[BSONDocument]()}
      f.firmwareDaoMock.findFirmware() returns future{Stream[BSONDocument]()}
      f.firmwareDaoMock.findVersionFirmware() returns future{Stream[BSONDocument]()}

      val req=FakeRequest(GET, "/inventary/modules/"+bson.stringify+"/module").withSession("user" -> """{"login":"test"}""")
      val r = f.controller.modulePage(bson.stringify).apply(req)

      status(r) must equalTo(OK)
      contentType(r) must beSome.which(_ == "text/html")
      val content = contentAsString(r)
      content must contain("<title>Inventaire des modules</title>")
      content must contain("<input type=\"text\" id=\"id\" name=\"id\" value=\"\" class=\"form-control\"/>")
      content must contain("<input type=\"date\" id=\"acquisition\" name=\"acquisition\" value=\"\" class=\"form-control\"/>")
      content must contain("<input type=\"date\" id=\"firstUse\" name=\"firstUse\" value=\"\" class=\"form-control\"/>")
      content must contain("<input type=\"checkbox\" id=\"agregateur\" name=\"agregateur\" value=\"true\"  />")
      content must contain("<input type=\"text\" id=\"apolline\" name=\"apolline\" class=\"form-control\" value=\"\" autocomplete=\"off\" list=\"list_apolline\"/>")
      content must contain("<input type=\"text\" id=\"firmware\" name=\"firmware\" class=\"form-control\" value=\"\" autocomplete=\"off\" list=\"list_firmware\"/>")
      content must contain("<input type=\"text\" id=\"versionFirmware\" name=\"versionFirmware\" class=\"form-control\" value=\"\" autocomplete=\"off\" list=\"list_versionFirmware\"/>")
      content must contain("<input type=\"checkbox\" id=\"hs\" name=\"hs\" value=\"true\"  />")
      content must contain("<textarea id=\"commentaire\" name=\"commentaire\" class=\"form-control\"></textarea>")

      there was one(f.moduleDaoMock).findApolline()
      there was one(f.firmwareDaoMock).findFirmware()
      there was one(f.firmwareDaoMock).findVersionFirmware()
      there was one(f.typeModuleManagerMock).doIfTypeModuleFound(org.mockito.Matchers.eq(bson))(any[Unit => Future[Result]])(any[Unit => Future[Result]])
    }

    "send bad request with an empty form if module type not found" in new WithApplication {
      val f=fixture

      f.applyNotFoundFunction()
      f.moduleDaoMock.findApolline() returns future{Stream[BSONDocument]()}
      f.firmwareDaoMock.findFirmware() returns future{Stream[BSONDocument]()}
      f.firmwareDaoMock.findVersionFirmware() returns future{Stream[BSONDocument]()}

      val req=FakeRequest(GET, "/inventary/modules/"+bson.stringify+"/module").withSession("user" -> """{"login":"test"}""")
      val r = f.controller.modulePage(bson.stringify).apply(req)

      status(r) must equalTo(BAD_REQUEST)
      contentType(r) must beSome.which(_ == "text/html")
      val content = contentAsString(r)
      content must contain("<title>Inventaire des modules</title>")
      content must contain("<div class=\"alert alert-danger\" role=\"alert\">Ce type de module n&#x27;existe pas</div>")
      content must contain("<input type=\"text\" id=\"id\" name=\"id\" value=\"\" class=\"form-control\"/>")
      content must contain("<input type=\"date\" id=\"acquisition\" name=\"acquisition\" value=\"\" class=\"form-control\"/>")
      content must contain("<input type=\"date\" id=\"firstUse\" name=\"firstUse\" value=\"\" class=\"form-control\"/>")
      content must contain("<input type=\"checkbox\" id=\"agregateur\" name=\"agregateur\" value=\"true\"  />")
      content must contain("<input type=\"text\" id=\"apolline\" name=\"apolline\" class=\"form-control\" value=\"\" autocomplete=\"off\" list=\"list_apolline\"/>")
      content must contain("<input type=\"text\" id=\"firmware\" name=\"firmware\" class=\"form-control\" value=\"\" autocomplete=\"off\" list=\"list_firmware\"/>")
      content must contain("<input type=\"text\" id=\"versionFirmware\" name=\"versionFirmware\" class=\"form-control\" value=\"\" autocomplete=\"off\" list=\"list_versionFirmware\"/>")
      content must contain("<input type=\"checkbox\" id=\"hs\" name=\"hs\" value=\"true\"  />")
      content must contain("<textarea id=\"commentaire\" name=\"commentaire\" class=\"form-control\"></textarea>")

      there was one(f.moduleDaoMock).findApolline()
      there was one(f.firmwareDaoMock).findFirmware()
      there was one(f.firmwareDaoMock).findVersionFirmware()
      there was one(f.typeModuleManagerMock).doIfTypeModuleFound(org.mockito.Matchers.eq(bson))(any[Unit => Future[Result]])(any[Unit => Future[Result]])
    }

    "send 500 internal error if have mongoDB error when insert module" in new WithApplication{
      val f=fixture
      val data=Json.parse("""{"id":"Id","acquisition":"2015-04-22","firmware":"firm","versionFirmware":"v01","send":"submit"}""")
      val futureMock=mock[Future[LastError]]
      val throwable=mock[Throwable]

      f.applyFoundFunction()
      f.moduleDaoMock.findOne(any[JsObject])(any[ExecutionContext]) returns future{None}
      f.firmwareDaoMock.findOne(org.mockito.Matchers.eq(Json.obj("nom"->"firm","version"->"v01")))(any[ExecutionContext]) returns future{Some(Firmware(bson,"firm","v01"))}
      f.moduleDaoMock.insert(any[Module],any[GetLastError])(any[ExecutionContext]) returns futureMock
      futureMock.flatMap(any[LastError=>Future[LastError]])(any[ExecutionContext]) returns futureMock
      futureMock.recover(any[PartialFunction[Throwable,Result]])(any[ExecutionContext]) answers {value=>future{value.asInstanceOf[PartialFunction[Throwable,Result]](throwable)}}

      val req=FakeRequest(POST,"/inventary/modules/"+bson.stringify+"/module").withJsonBody(data).withSession("user" -> """{"login":"test"}""")
      val r=f.controller.moduleInsert(bson.stringify).apply(req)

      status(r) must equalTo(INTERNAL_SERVER_ERROR)

      there was one(f.typeModuleManagerMock).doIfTypeModuleFound(org.mockito.Matchers.eq(bson))(any[Unit=>Future[Result]])(any[Unit=>Future[Result]])
      there was one(f.moduleDaoMock).findOne(any[JsObject])(any[ExecutionContext])
      there was one(f.firmwareDaoMock).findOne(org.mockito.Matchers.eq(Json.obj("nom"->"firm","version"->"v01")))(any[ExecutionContext])
      there was one(f.moduleDaoMock).insert(any[Module],any[GetLastError])(any[ExecutionContext])
      there was one(futureMock).flatMap(any[LastError=>Future[LastError]])(any[ExecutionContext])
      there was one(futureMock).recover(any[PartialFunction[Throwable,Result]])(any[ExecutionContext])
    }

    "send redirect if user not would be continue to insert sensor" in new WithApplication{
      val f=fixture
      val data=Json.parse("""{"id":"Id","acquisition":"2015-04-22","firmware":"firm","versionFirmware":"v01","send":"Envoyer"}""")
      val lastError=mock[LastError]

      f.applyFoundFunction()
      f.moduleDaoMock.findOne(any[JsObject])(any[ExecutionContext]) returns future{None}
      f.firmwareDaoMock.findOne(org.mockito.Matchers.eq(Json.obj("nom"->"firm","version"->"v01")))(any[ExecutionContext]) returns future{Some(Firmware(bson,"firm","v01"))}
      f.moduleDaoMock.insert(any[Module],any[GetLastError])(any[ExecutionContext]) returns future{lastError}

      val req=FakeRequest(POST,"/inventary/modules/"+bson.stringify+"/module").withJsonBody(data).withSession("user" -> """{"login":"test"}""")
      val r=f.controller.moduleInsert(bson.stringify).apply(req)

      status(r) must equalTo(SEE_OTHER)
      header("Location",r) must equalTo(Some("/inventary/modules/"+bson.stringify))

      there was one(f.typeModuleManagerMock).doIfTypeModuleFound(org.mockito.Matchers.eq(bson))(any[Unit=>Future[Result]])(any[Unit=>Future[Result]])
      there was one(f.moduleDaoMock).findOne(any[JsObject])(any[ExecutionContext])
      there was one(f.firmwareDaoMock).findOne(org.mockito.Matchers.eq(Json.obj("nom"->"firm","version"->"v01")))(any[ExecutionContext])
      there was one(f.moduleDaoMock).insert(any[Module],any[GetLastError])(any[ExecutionContext])
    }

    "send 200 Ok page after insert sensor" in new WithApplication{
      val f=fixture
      val data=Json.parse("""{"id":"Id","acquisition":"2015-04-22","firstUse":"2015-04-22","apolline":"vapolline","firmware":"firm","versionFirmware":"v01","commentaire":"un com","send":"Envoyer et continuer"}""")
      val lastError=mock[LastError]

      f.applyFoundFunction()
      f.moduleDaoMock.findOne(any[JsObject])(any[ExecutionContext]) returns future{None}
      f.firmwareDaoMock.findOne(org.mockito.Matchers.eq(Json.obj("nom"->"firm","version"->"v01")))(any[ExecutionContext]) returns future{Some(Firmware(bson,"firm","v01"))}
      f.moduleDaoMock.insert(any[Module],any[GetLastError])(any[ExecutionContext]) returns future{lastError}
      f.moduleDaoMock.findApolline() returns future{Stream[BSONDocument]()}
      f.firmwareDaoMock.findFirmware() returns future{Stream[BSONDocument]()}
      f.firmwareDaoMock.findVersionFirmware() returns future{Stream[BSONDocument]()}

      val req=FakeRequest(POST,"/inventary/modules/"+bson.stringify+"/module").withJsonBody(data).withSession("user" -> """{"login":"test"}""")
      val r=f.controller.moduleInsert(bson.stringify).apply(req)

      status(r) must equalTo(OK)
      contentType(r) must beSome.which(_ == "text/html")
      val content=contentAsString(r)
      content must contain("<title>Inventaire des modules</title>")
      content must contain("<input type=\"text\" id=\"id\" name=\"id\" value=\"\" class=\"form-control\"/>")
      content must contain("<input type=\"date\" id=\"acquisition\" name=\"acquisition\" value=\"2015-04-22\" class=\"form-control\"/>")
      content must contain("<input type=\"date\" id=\"firstUse\" name=\"firstUse\" value=\"2015-04-22\" class=\"form-control\"/>")
      content must contain("<input type=\"checkbox\" id=\"agregateur\" name=\"agregateur\" value=\"true\"  />")
      content must contain("<input type=\"text\" id=\"apolline\" name=\"apolline\" class=\"form-control\" value=\"vapolline\" autocomplete=\"off\" list=\"list_apolline\"/>")
      content must contain("<input type=\"text\" id=\"firmware\" name=\"firmware\" class=\"form-control\" value=\"firm\" autocomplete=\"off\" list=\"list_firmware\"/>")
      content must contain("<input type=\"text\" id=\"versionFirmware\" name=\"versionFirmware\" class=\"form-control\" value=\"v01\" autocomplete=\"off\" list=\"list_versionFirmware\"/>")
      content must contain("<input type=\"checkbox\" id=\"hs\" name=\"hs\" value=\"true\"  />")
      content must contain("<textarea id=\"commentaire\" name=\"commentaire\" class=\"form-control\">un com</textarea>")

      there was one(f.typeModuleManagerMock).doIfTypeModuleFound(org.mockito.Matchers.eq(bson))(any[Unit=>Future[Result]])(any[Unit=>Future[Result]])
      there was one(f.moduleDaoMock).findOne(any[JsObject])(any[ExecutionContext])
      there was one(f.firmwareDaoMock).findOne(org.mockito.Matchers.eq(Json.obj("nom"->"firm","version"->"v01")))(any[ExecutionContext])
      there was one(f.moduleDaoMock).insert(any[Module],any[GetLastError])(any[ExecutionContext])
      there was one(f.moduleDaoMock).findApolline()
      there was one(f.firmwareDaoMock).findFirmware()
      there was one(f.firmwareDaoMock).findVersionFirmware()
    }
  }

  "When user is on the page /inventary/sensors/:id/:id2, ModuleManager" should{
    "send 200 Ok page with a prefilled form" in new WithApplication{
      val f=fixture
      val module=Module(bson2,"Id",bson,bson3,date,Some(date2),true,Some("v01"),true,Some("un com"))
      val firmware=Firmware(bson3,"firm","v02")
      val routeCall=mock[Call]

      f.applyFoundFunction()
      f.moduleDaoMock.findById(org.mockito.Matchers.eq(bson2))(any[ExecutionContext]) returns future{Some(module)}
      f.firmwareDaoMock.findById(org.mockito.Matchers.eq(bson3))(any[ExecutionContext]) returns future{Some(firmware)}
      f.moduleDaoMock.findApolline() returns future{Stream[BSONDocument]()}
      f.firmwareDaoMock.findFirmware() returns future{Stream[BSONDocument]()}
      f.firmwareDaoMock.findVersionFirmware() returns future{Stream[BSONDocument]()}

      val req=FakeRequest(GET,"/inventary/modules/"+bson.stringify+"/"+bson2.stringify).withSession("user" -> """{"login":"test"}""")
      val r=f.controller.moduleUpdatePage(bson.stringify,bson2.stringify).apply(req)

      status(r) must equalTo(OK)
      contentType(r) must beSome.which(_ == "text/html")
      val content = contentAsString(r)
      content must contain("<title>Inventaire des modules</title>")
      content must contain("<input type=\"text\" id=\"id\" name=\"id\" value=\"Id\" class=\"form-control\"/>")
      content must contain("<input type=\"date\" id=\"acquisition\" name=\"acquisition\" value=\"2015-04-22\" class=\"form-control\"/>")
      content must contain("<input type=\"date\" id=\"firstUse\" name=\"firstUse\" value=\"2015-04-23\" class=\"form-control\"/>")
      content must contain("<input type=\"checkbox\" id=\"agregateur\" name=\"agregateur\" value=\"true\" checked=\"checked\" />")
      content must contain("<input type=\"text\" id=\"apolline\" name=\"apolline\" class=\"form-control\" value=\"v01\" autocomplete=\"off\" list=\"list_apolline\"/>")
      content must contain("<input type=\"text\" id=\"firmware\" name=\"firmware\" class=\"form-control\" value=\"firm\" autocomplete=\"off\" list=\"list_firmware\"/>")
      content must contain("<input type=\"text\" id=\"versionFirmware\" name=\"versionFirmware\" class=\"form-control\" value=\"v02\" autocomplete=\"off\" list=\"list_versionFirmware\"/>")
      content must contain("<input type=\"checkbox\" id=\"hs\" name=\"hs\" value=\"true\" checked=\"checked\" />")
      content must contain("<textarea id=\"commentaire\" name=\"commentaire\" class=\"form-control\">un com</textarea>")

      there was one(f.typeModuleManagerMock).doIfTypeModuleFound(org.mockito.Matchers.eq(bson))(any[Unit=>Future[Result]])(any[Unit=>Future[Result]])
      there was one(f.moduleDaoMock).findById(org.mockito.Matchers.eq(bson2))(any[ExecutionContext])
      there was one(f.firmwareDaoMock).findById(org.mockito.Matchers.eq(bson3))(any[ExecutionContext])
      there was one(f.moduleDaoMock).findApolline()
      there was one(f.firmwareDaoMock).findFirmware()
      there was one(f.firmwareDaoMock).findVersionFirmware()
    }

    "send redirect after update module" in new WithApplication{
      val f=fixture
      val data=Json.parse("""{"id":"Id","acquisition":"2015-04-22","firstUse":"2015-04-22","firmware":"firm","versionFirmware":"v01","commentaire":"un com","send":"Envoyer"}""")
      val lastError=mock[LastError]

      f.applyFoundFunction()
      f.moduleDaoMock.findOne(any[JsObject])(any[ExecutionContext]) returns future{None}
      f.firmwareDaoMock.findOne(any[JsObject])(any[ExecutionContext]) returns future{Some(Firmware(bson3,"firm","v01"))}
      f.moduleDaoMock.updateById(org.mockito.Matchers.eq(bson2), any[Module],any[GetLastError])(any[Writes[Module]],any[ExecutionContext]) returns future{lastError}

      val req=FakeRequest(POST,"/inventary/modules/"+bson.stringify+"/module").withJsonBody(data).withSession("user" -> """{"login":"test"}""")
      val r=f.controller.moduleUpdate(bson.stringify,bson2.stringify).apply(req)

      status(r) must equalTo(SEE_OTHER)
      header("Location",r) must equalTo(Some("/inventary/modules/"+bson.stringify))

      there was one(f.typeModuleManagerMock).doIfTypeModuleFound(org.mockito.Matchers.eq(bson))(any[Unit=>Future[Result]])(any[Unit=>Future[Result]])
      there was one(f.moduleDaoMock).findOne(any[JsObject])(any[ExecutionContext])
      there was one(f.firmwareDaoMock).findOne(any[JsObject])(any[ExecutionContext])
      there was one(f.moduleDaoMock).updateById(org.mockito.Matchers.eq(bson2), any[Module],any[GetLastError])(any[Writes[Module]],any[ExecutionContext])
    }

    "send 500 internal error if have mongoDB error when update module" in new WithApplication{
      val f=fixture
      val data=Json.parse("""{"id":"Id","acquisition":"2015-04-22","firstUse":"2015-04-22","firmware":"firm","versionFirmware":"v01","commentaire":"un com","send":"Envoyer"}""")
      val futureMock=mock[Future[LastError]]
      val throwable=mock[Throwable]

      f.applyFoundFunction()
      f.moduleDaoMock.findOne(any[JsObject])(any[ExecutionContext]) returns future{None}
      f.firmwareDaoMock.findOne(any[JsObject])(any[ExecutionContext]) returns future{Some(Firmware(bson3,"firm","v01"))}
      f.moduleDaoMock.updateById(org.mockito.Matchers.eq(bson2), any[Module],any[GetLastError])(any[Writes[Module]],any[ExecutionContext]) returns futureMock
      futureMock.map(any[LastError=>LastError])(any[ExecutionContext]) returns futureMock
      futureMock.recover(any[PartialFunction[Throwable,Result]])(any[ExecutionContext]) answers {value=>future{value.asInstanceOf[PartialFunction[Throwable,Result]](throwable)}}

      val req=FakeRequest(POST,"/inventary/modules/"+bson.stringify+"/module").withJsonBody(data).withSession("user" -> """{"login":"test"}""")
      val r=f.controller.moduleUpdate(bson.stringify,bson2.stringify).apply(req)

      status(r) must equalTo(INTERNAL_SERVER_ERROR)

      there was one(f.typeModuleManagerMock).doIfTypeModuleFound(org.mockito.Matchers.eq(bson))(any[Unit=>Future[Result]])(any[Unit=>Future[Result]])
      there was one(f.moduleDaoMock).findOne(any[JsObject])(any[ExecutionContext])
      there was one(f.firmwareDaoMock).findOne(any[JsObject])(any[ExecutionContext])
      there was one(f.moduleDaoMock).updateById(org.mockito.Matchers.eq(bson2), any[Module],any[GetLastError])(any[Writes[Module]],any[ExecutionContext])
      there was one(futureMock).map(any[LastError=>LastError])(any[ExecutionContext])
      there was one(futureMock).recover(any[PartialFunction[Throwable,Result]])(any[ExecutionContext])
    }
  }

  "When user is on the page /inventary/sensors/:id/:id2/clone, ModuleManager" should{
    "send 200 Ok page with a prefilled form" in new WithApplication{
      val f=fixture
      val module=Module(bson2,"Id",bson,bson3,date,Some(date2),true,Some("v01"),true,Some("un com"))
      val firmware=Firmware(bson3,"firm","v02")
      val routeCall=mock[Call]

      f.applyFoundFunction()
      f.moduleDaoMock.findById(org.mockito.Matchers.eq(bson2))(any[ExecutionContext]) returns future{Some(module)}
      f.firmwareDaoMock.findById(org.mockito.Matchers.eq(bson3))(any[ExecutionContext]) returns future{Some(firmware)}
      f.moduleDaoMock.findApolline() returns future{Stream[BSONDocument]()}
      f.firmwareDaoMock.findFirmware() returns future{Stream[BSONDocument]()}
      f.firmwareDaoMock.findVersionFirmware() returns future{Stream[BSONDocument]()}

      val req=FakeRequest(GET,"/inventary/modules/"+bson.stringify+"/"+bson2.stringify).withSession("user" -> """{"login":"test"}""")
      val r=f.controller.moduleClonePage(bson.stringify,bson2.stringify).apply(req)

      status(r) must equalTo(OK)
      contentType(r) must beSome.which(_ == "text/html")
      val content = contentAsString(r)
      content must contain("<title>Inventaire des modules</title>")
      content must contain("<input type=\"text\" id=\"id\" name=\"id\" value=\"\" class=\"form-control\"/>")
      content must contain("<input type=\"date\" id=\"acquisition\" name=\"acquisition\" value=\"2015-04-22\" class=\"form-control\"/>")
      content must contain("<input type=\"date\" id=\"firstUse\" name=\"firstUse\" value=\"2015-04-23\" class=\"form-control\"/>")
      content must contain("<input type=\"checkbox\" id=\"agregateur\" name=\"agregateur\" value=\"true\" checked=\"checked\" />")
      content must contain("<input type=\"text\" id=\"apolline\" name=\"apolline\" class=\"form-control\" value=\"v01\" autocomplete=\"off\" list=\"list_apolline\"/>")
      content must contain("<input type=\"text\" id=\"firmware\" name=\"firmware\" class=\"form-control\" value=\"firm\" autocomplete=\"off\" list=\"list_firmware\"/>")
      content must contain("<input type=\"text\" id=\"versionFirmware\" name=\"versionFirmware\" class=\"form-control\" value=\"v02\" autocomplete=\"off\" list=\"list_versionFirmware\"/>")
      content must contain("<input type=\"checkbox\" id=\"hs\" name=\"hs\" value=\"true\" checked=\"checked\" />")
      content must contain("<textarea id=\"commentaire\" name=\"commentaire\" class=\"form-control\">un com</textarea>")

      there was one(f.typeModuleManagerMock).doIfTypeModuleFound(org.mockito.Matchers.eq(bson))(any[Unit=>Future[Result]])(any[Unit=>Future[Result]])
      there was one(f.moduleDaoMock).findById(org.mockito.Matchers.eq(bson2))(any[ExecutionContext])
      there was one(f.firmwareDaoMock).findById(org.mockito.Matchers.eq(bson3))(any[ExecutionContext])
      there was one(f.moduleDaoMock).findApolline()
      there was one(f.firmwareDaoMock).findFirmware()
      there was one(f.firmwareDaoMock).findVersionFirmware()
    }
  }
}