import controllers._
import models._
import org.junit.runner.RunWith
import org.specs2.matcher.{MatchResult, Expectable, Matcher}
import org.specs2.mock.Mockito
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner
import play.api.libs.json.{JsObject, Writes, Json}
import play.api.mvc.{Results, Action, Result}
import play.api.test.Helpers._
import play.api.test.{FakeRequest, WithApplication}
import reactivemongo.bson.BSONObjectID
import reactivemongo.extensions.BSONFormats.BSONObjectIDFormat
import reactivemongo.core.commands.{LastError, GetLastError}

import scala.concurrent._
import scala.concurrent.duration.Duration

@RunWith(classOf[JUnitRunner])
class ParametreManagerSpec extends Specification with Mockito {

  class ParametreManagerTest extends ParametreManagerLike

  case class matchRegex(a: String) extends Matcher[String]() {
    override def apply[S <: String](t: Expectable[S]): MatchResult[S] = result(a.r.findFirstIn(t.value).nonEmpty, "okMessage", t.value + "not found " + a, t)
  }

  case class contains(a: String, b: Int) extends Matcher[String]() {
    override def apply[S <: String](t: Expectable[S]): MatchResult[S] = result(a.r.findAllMatchIn(t.value).size == b, "okMessage", "not found " + b + " times but " + a.r.findAllMatchIn(t.value).size + " times " + a, t)
  }

  val bson = BSONObjectID.generate
  val bson2 = BSONObjectID.generate

  def fixture = new {
    val parameterDaoMock=mock[ParametresDao]
    val controller=new ParametreManagerTest{
      override val parameterDao:ParametresDao=parameterDaoMock
    }
  }

  "When user is not connected, ParametreManager" should {
    "redirect to login for resource /campaigns/parameters" in new WithApplication {
      route(FakeRequest(GET, "/campaigns/parameters")).map(
        r => {
          status(r) must equalTo(SEE_OTHER)
          header("Location", r) must equalTo(Some("/login"))
        }
      ).getOrElse(
          failure("Pas de retour de la fonction")
        )
    }

    "redirect to login for resource /campaigns/parameters/parameter" in new WithApplication {
      route(FakeRequest(GET, "/campaigns/parameters/parameter")).map(
        r => {
          status(r) must equalTo(SEE_OTHER)
          header("Location", r) must equalTo(Some("/login"))
        }
      ).getOrElse(
          failure("Pas de retour de la fonction")
        )
    }
  }

  "When user is on resource /campaigns/parameters/parameter" should{
    "send 200 Ok page with an empty form" in new WithApplication{
      val f=fixture

      val r=f.controller.addParameterPage().apply(FakeRequest(GET,"/campaigns/parameters/parameter").withSession("user"->"""{"login":"test"}"""))

      status(r) must equalTo(OK)
      val content=contentAsString(r)
      content must contain("<input type=\"text\" id=\"key\" name=\"key\" value=\"\" class=\"form-control\"/>")
      content must contain("<input type=\"text\" id=\"value\" name=\"value\" value=\"\" class=\"form-control\"/>")
    }
  }
}