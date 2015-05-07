import java.util.Date

import controllers.{ModuleManager, TypeModuleManagerLike, ModuleManagerLike}
import models.{FirmwareDao, ModuleDao, TypeModule, TypeModuleDao}
import org.junit.runner.RunWith
import org.specs2.matcher.{MatchResult, Expectable, Matcher}
import org.specs2.mock.Mockito
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner
import play.api.mvc.{Call, Results, Action, Result}
import play.api.test.Helpers._
import play.api.test.{FakeRequest, WithApplication}
import reactivemongo.bson.{BSONDocument, BSONObjectID}

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
  }

  "When user is on resource /inventary/modules/:id, ModuleManager" should {

    "send 200 OK page with the message 'Aucun résultat trouvé', if not have modules" in new WithApplication {
      val f = fixture

      f.typeModuleDaoMock.findById(org.mockito.Matchers.eq(bson))(any[ExecutionContext]) returns future {
        Some(TypeModule(bson, "mod", "type"))
      }

      val r = f.controller.inventary(bson.stringify).apply(FakeRequest(GET, "/inventary/modules/" + bson.stringify).withSession("user" -> """{"login":"test"}"""))

      status(r) must equalTo(OK)
      contentType(r) must beSome.which(_ == "text/html")
      val content = contentAsString(r)
      content must contain("<title>Inventaire des modules</title>")
      content must matchRegex("type\\s*/\\s*mod")
      content must matchRegex("<span class=\"bold\">\\s*Stocks\\s*</span>\\s*:\\s*0")

      there was one(f.typeModuleDaoMock).findById(org.mockito.Matchers.eq(bson))(any[ExecutionContext])
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
      futureMock.map(any[Option[TypeModule]=>Option[TypeModule]])(any[ExecutionContext]) returns futureMock
      futureMock.recover(any[PartialFunction[Throwable,Result]])(any[ExecutionContext]) answers {value=>future{value.asInstanceOf[PartialFunction[Throwable,Result]](throwable)}}

      val r = f.controller.inventary(bson.stringify).apply(FakeRequest(GET, "/inventary/modules/" + bson.stringify).withSession("user" -> """{"login":"test"}"""))

      status(r) must equalTo(INTERNAL_SERVER_ERROR)

      there was one(f.typeModuleDaoMock).findById(org.mockito.Matchers.eq(bson))(any[ExecutionContext])
      there was one(futureMock).map(any[Option[TypeModule]=>Option[TypeModule]])(any[ExecutionContext])
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
  }
}
