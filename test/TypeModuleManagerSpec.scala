
import controllers.{TypeModuleManager, TypeModuleManagerLike}
import models.TypeModuleDao
import org.junit.runner.RunWith
import org.specs2.matcher.{MatchResult, Expectable, Matcher}
import org.specs2.mock.Mockito
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner
import play.api.mvc.{Result, Action, Results, Call}
import play.api.test.Helpers._
import play.api.test.{FakeRequest, WithApplication}
import reactivemongo.bson.BSONDocument
import scala.concurrent._

@RunWith(classOf[JUnitRunner])
class TypeModuleManagerSpec extends Specification with Mockito {

  class TypeModuleManagerTest extends TypeModuleManagerLike

  case class matchRegex(a: String) extends Matcher[String]() {
    override def apply[S <: String](t: Expectable[S]): MatchResult[S] = result(a.r.findFirstIn(t.value).nonEmpty, "okMessage", "not found " + a, t)
  }

  case class contains(a: String, b: Int) extends Matcher[String]() {
    override def apply[S <: String](t: Expectable[S]): MatchResult[S] = result(a.r.findAllMatchIn(t.value).size == b, "okMessage", "not found " + b + " times but " + a.r.findAllMatchIn(t.value).size + " times " + a, t)
  }

  def fixture = new {
    val typeModuleDaoMock = mock[TypeModuleDao]
    val controller = new TypeModuleManagerTest {
      override val typeModuleDao: TypeModuleDao = typeModuleDaoMock
    }
  }

  "When user is not connected, TypeModuleManager" should {
    "redirect to login for resource /inventary/modules" in new WithApplication {
      route(FakeRequest(GET, "/inventary/modules")).map(
        r=>{
          status(r) must equalTo(SEE_OTHER)
          header("Location",r) must equalTo(Some("/login"))
        }
      ).getOrElse(
        failure("Pas de retour de la fonction")
      )
    }

    "redirect to login for GET resource /inventary/modules/type" in new WithApplication{
      route(FakeRequest(GET, "/inventary/modules/type")).map(
        r=>{
          status(r) must equalTo(SEE_OTHER)
          header("Location",r) must equalTo(Some("/login"))
        }
      ).getOrElse(
          failure("Pas de retour de la fonction")
        )
    }
  }

  "When method printForm is called, TypeSensorManager" should{
    "print an empty form" in new WithApplication{
      val f=fixture

      f.typeModuleDaoMock.findListModele() returns future{Stream[BSONDocument]()}
      f.typeModuleDaoMock.findListType() returns future{Stream[BSONDocument]()}

      val req=FakeRequest(GET, "/inventary/modules/type").withSession("user" -> """{"login":"test"}""")
      val action = Action.async{implicit request=>f.controller.printForm(Results.Ok,TypeModuleManager.form,mock[Call])}
      val r=call(action,req)


      status(r) must equalTo(OK)
      contentType(r) must beSome.which(_ == "text/html")
      val content = contentAsString(r)
      content must contain("<title>Inventaire des modules</title>")
      content must contain("<input id=\"modele\" name=\"modele\" class=\"form-control\" list=\"list_modele\" type=\"text\" autocomplete=\"off\" value=\"\"/>")
      content must contain("<input id=\"types\" name=\"types\" class=\"form-control\" list=\"list_type\" type=\"text\" autocomplete=\"off\" value=\"\"/>")

      there was one(f.typeModuleDaoMock).findListModele()
      there was one(f.typeModuleDaoMock).findListType()
    }

    "send Internal Error if mongoDB error when find list of modele" in new WithApplication{
      val f=fixture
      val futureMock=mock[Future[Stream[BSONDocument]]]
      val throwable=mock[Throwable]

      f.typeModuleDaoMock.findListModele() returns futureMock
      futureMock.flatMap(any[Stream[BSONDocument]=>Future[Stream[BSONDocument]]])(any[ExecutionContext]) returns futureMock
      futureMock.recover(any[PartialFunction[Throwable,Result]])(any[ExecutionContext]) answers {value=>future{value.asInstanceOf[PartialFunction[Throwable,Result]](throwable)}}

      val req=FakeRequest(GET, "/inventary/modules/type").withSession("user" -> """{"login":"test"}""")
      val action = Action.async{implicit request=>f.controller.printForm(Results.Ok,TypeModuleManager.form,mock[Call])}
      val r=call(action,req)

      status(r) must equalTo(INTERNAL_SERVER_ERROR)

      there was one(f.typeModuleDaoMock).findListModele()
      there was one(futureMock).flatMap(any[Stream[BSONDocument]=>Future[Stream[BSONDocument]]])(any[ExecutionContext])
      there was one(futureMock).recover(any[PartialFunction[Throwable,Result]])(any[ExecutionContext])
    }

    "send Internal Error if mongoDB error when find list of type" in new WithApplication{
      val f=fixture
      val futureMock=mock[Future[Stream[BSONDocument]]]
      val throwable=mock[Throwable]

      f.typeModuleDaoMock.findListModele() returns future{Stream[BSONDocument]()}
      f.typeModuleDaoMock.findListType() returns futureMock
      futureMock.map(any[Stream[BSONDocument]=>Stream[BSONDocument]])(any[ExecutionContext]) returns futureMock
      futureMock.recover(any[PartialFunction[Throwable,Result]])(any[ExecutionContext]) answers {value=>future{value.asInstanceOf[PartialFunction[Throwable,Result]](throwable)}}
      val req=FakeRequest(GET, "/inventary/modules/type").withSession("user" -> """{"login":"test"}""")
      val action = Action.async{implicit request=>f.controller.printForm(Results.Ok,TypeModuleManager.form,mock[Call])}
      val r=call(action,req)

      status(r) must equalTo(INTERNAL_SERVER_ERROR)

      there was one(f.typeModuleDaoMock).findListModele()
      there was one(f.typeModuleDaoMock).findListType()
      there was one(futureMock).map(any[Stream[BSONDocument]=>Stream[BSONDocument]])(any[ExecutionContext])
      there was one(futureMock).recover(any[PartialFunction[Throwable,Result]])(any[ExecutionContext])
    }
  }
}