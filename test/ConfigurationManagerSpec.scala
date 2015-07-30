import java.util.Date

import controllers.{ModuleManagerLike, ConfigurationForm, ConfigurationManagerLike}
import models.Module
import org.junit.runner.RunWith
import org.specs2.matcher.{MatchResult, Expectable, Matcher}
import org.specs2.mock.Mockito
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner
import play.api.libs.json.Json
import play.api.mvc.Result
import play.api.test.Helpers._
import play.api.test.{FakeRequest, WithApplication}
import reactivemongo.bson.BSONObjectID

import scala.concurrent.Future

@RunWith(classOf[JUnitRunner])
class ConfigurationManagerSpec extends Specification with Mockito{

  class ConfigurationManagerTest extends ConfigurationManagerLike

  case class contains(a: String, b: Int) extends Matcher[String]() {
    override def apply[S <: String](t: Expectable[S]): MatchResult[S] = result(a.r.findAllMatchIn(t.value).size == b, "okMessage", "not found " + b + " times but " + a.r.findAllMatchIn(t.value).size + " times " + a, t)
  }

  case class matchRegex(a: String) extends Matcher[String]() {
    override def apply[S <: String](t: Expectable[S]): MatchResult[S] = result(a.r.findFirstIn(t.value).nonEmpty, "okMessage", t.value + "not found " + a, t)
  }

  def fixture=new {
    val moduleManagerMock = mock[ModuleManagerLike]
    val controller = new ConfigurationManagerTest{
      override val moduleManager=moduleManagerMock
    }

    def applyFoundFunction() {
      moduleManagerMock.doIfModuleFound(org.mockito.Matchers.eq(bson))(any[Module => Future[Result]])(any[Unit => Future[Result]]) answers { (params, _) => params match {
        case Array(_, p: (Module => Future[Result]), _) => p.apply(Module(bson,"id","type",date,List(bson2),List(bson3),Some("un com")))
      }}
    }

    def applyNotFoundFunction() {
      moduleManagerMock.doIfModuleFound(org.mockito.Matchers.eq(bson))(any[Module => Future[Result]])(any[Unit => Future[Result]]) answers { (params, _) => params match {
        case Array(_,_, p: (Unit => Future[Result])) => p.apply()
      }}
    }
  }

  val date=new Date(115,3,22)

  val bson=BSONObjectID.generate
  val bson2=BSONObjectID.generate
  val bson3=BSONObjectID.generate

  "When user is not connected, ModuleManager" should {
    "redirect to login for resource /inventary/modules/:id/configuration" in new WithApplication {
      route(FakeRequest(GET, "/inventary/modules/"+bson.stringify+"/configuration")).map(
        r => {
          status(r) must equalTo(SEE_OTHER)
          header("Location", r) must equalTo(Some("/login"))
        }
      ).getOrElse(
          failure("Pas de retour de la fonction")
        )
    }

    "redirect to login for resource /inventary/modules/:id/configuration" in new WithApplication {
      route(FakeRequest(POST, "/inventary/modules/"+bson.stringify+"/configuration")).map(
        r => {
          status(r) must equalTo(SEE_OTHER)
          header("Location", r) must equalTo(Some("/login"))
        }
      ).getOrElse(
          failure("Pas de retour de la fonction")
        )
    }
  }

  "When user is on resource /inventary/modules/:id/configuration, ConfigurationManager" should{
    "send 200 Ok page with a prefilled form" in new WithApplication{
      val f=fixture

      f.applyFoundFunction()

      val r=f.controller.formConfiguration(bson.stringify).apply(FakeRequest(GET, "/inventary/modules/"+bson.stringify+"/configuration").withSession("user" -> """{"login":"test"}"""))
      status(r) must equalTo(OK)
      val content=contentAsString(r)
      content must contain("<h4>type / id</h4>")
      content must contain("<span class=\"bold\">Commentaires</span> : un com")
      content must matchRegex("<span class=\"bold\">Date d&#x27;assemblage</span> : \\d{2}/\\d{2}/\\d{4}")
      content must contain("<input type=\"text\" id=\"port\" name=\"port\" value=\"\" autofocus=\"autofocus\" autocomplete=\"off\" class=\"form-control\"/>")
      content must contain("<input id=\"timeout\" name=\"timeout\" class=\"form-control\" type=\"number\" autocomplete=\"off\" value=\"10000\"/>")
      content must contain("<input id=\"baud\" name=\"baud\" class=\"form-control\" type=\"number\" autocomplete=\"off\" value=\"9600\"/>")
      content must contain("<input id=\"bits\" name=\"bits\" class=\"form-control\" type=\"number\" autocomplete=\"off\" value=\"8\"/>")
      content must contain("<option value=\"0\" selected=\"selected\">None</option>")
      content must contain("<option value=\"1\" >Odd</option>")
      content must contain("<option value=\"2\" >Even</option>")
      content must contain("<option value=\"3\" >Mark</option>")
      content must contain("<option value=\"4\" >Space</option>")
      content must contain("<input id=\"stopBits\" name=\"stopBits\" class=\"form-control\" type=\"number\" autocomplete=\"off\" value=\"1\"/>")
      content must contain("<input id=\"timeFilter\" name=\"timeFilter\" class=\"form-control\" type=\"number\" autocomplete=\"off\" value=\"1000\"/>")
      content must contain("<input type=\"text\" id=\"types\" name=\"types\" value=\"\" class=\"form-control\"/>")

      there was one(f.moduleManagerMock).doIfModuleFound(org.mockito.Matchers.eq(bson))(any[Module=>Future[Result]])(any[Unit=>Future[Result]])
    }

    "send redirect if module not found" in new WithApplication{
      val f=fixture

      f.applyNotFoundFunction()

      val r=f.controller.formConfiguration(bson.stringify).apply(FakeRequest(GET, "/inventary/modules/"+bson.stringify+"/configuration").withSession("user" -> """{"login":"test"}"""))
      status(r) must equalTo(SEE_OTHER)
      header("Location",r) must beSome("/inventary/modules")

      there was one(f.moduleManagerMock).doIfModuleFound(org.mockito.Matchers.eq(bson))(any[Module=>Future[Result]])(any[Unit=>Future[Result]])
    }

    "send redirect after insert configuration" in new WithApplication{
      val data=Json.obj("port"->"/dev/ttyUSB0","timeout"->10000,"baud"->9600,"bits"->8,"stopBits"->1,"parity"->0,"timeFilter"->1000,"types"->"ADC")
      val f=fixture

      f.applyFoundFunction()

      val r=f.controller.insertFormConfiguration(bson.stringify).apply(FakeRequest(POST, "/inventary/modules/"+bson.stringify+"/configuration").withJsonBody(data).withSession("user" -> """{"login":"test"}"""))
      status(r) must equalTo(SEE_OTHER)
      header("Location",r) must beSome("/inventary/modules/"+bson.stringify+"/configuration/sensors")
      val s=session(r)
      s.get("configForm") must beSome("insert")
      s.get("config") must beSome(Json.stringify(data))

      there was one(f.moduleManagerMock).doIfModuleFound(org.mockito.Matchers.eq(bson))(any[Module=>Future[Result]])(any[Unit=>Future[Result]])
    }

    "send bad request if form is send with no data" in new WithApplication{
      val f=fixture

      f.applyFoundFunction()

      val r=f.controller.insertFormConfiguration(bson.stringify).apply(FakeRequest(POST, "/inventary/modules/"+bson.stringify+"/configuration").withSession("user" -> """{"login":"test"}"""))
      status(r) must equalTo(BAD_REQUEST)
      contentAsString(r) must contains("<span class=\"control-label errors\">This field is required</span>",8)

      there was one(f.moduleManagerMock).doIfModuleFound(org.mockito.Matchers.eq(bson))(any[Module=>Future[Result]])(any[Unit=>Future[Result]])
    }

    "send bad request if number ar not valid" in new WithApplication{
      val data=Json.obj("port"->"/dev/ttyUSB0","timeout"->"a","baud"->"a","bits"->"a","stopBits"->"a","parity"->"a","timeFilter"->"a","types"->"ADC")
      val f=fixture

      f.applyFoundFunction()

      val r=f.controller.insertFormConfiguration(bson.stringify).apply(FakeRequest(POST, "/inventary/modules/"+bson.stringify+"/configuration").withJsonBody(data).withSession("user" -> """{"login":"test"}"""))
      status(r) must equalTo(BAD_REQUEST)
      contentAsString(r) must contains("<span class=\"control-label errors\">Numeric value expected</span>",6)

      there was one(f.moduleManagerMock).doIfModuleFound(org.mockito.Matchers.eq(bson))(any[Module=>Future[Result]])(any[Unit=>Future[Result]])
    }

    "send redirect if module not found" in new WithApplication{
      val f=fixture

      f.applyNotFoundFunction()

      val r=f.controller.insertFormConfiguration(bson.stringify).apply(FakeRequest(GET, "/inventary/modules/"+bson.stringify+"/configuration").withSession("user" -> """{"login":"test"}"""))
      status(r) must equalTo(SEE_OTHER)
      header("Location",r) must beSome("/inventary/modules")

      there was one(f.moduleManagerMock).doIfModuleFound(org.mockito.Matchers.eq(bson))(any[Module=>Future[Result]])(any[Unit=>Future[Result]])
    }
  }
}
