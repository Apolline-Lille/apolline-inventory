import java.util.Date

import controllers.{TypeModuleManagerLike, ModuleManagerLike}
import models.{TypeModule, TypeModuleDao}
import org.junit.runner.RunWith
import org.specs2.matcher.{MatchResult, Expectable, Matcher}
import org.specs2.mock.Mockito
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner
import play.api.mvc.Result
import play.api.test.Helpers._
import play.api.test.{FakeRequest, WithApplication}
import reactivemongo.bson.BSONObjectID

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
    val controller=new ModuleManagerTest{
      override val typeModuleDao:TypeModuleDao=typeModuleDaoMock
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
}
