import controllers._
import models.{Campagne, CampagneDao, ConditionDao}
import org.junit.runner.RunWith
import org.specs2.matcher.{MatchResult, Expectable, Matcher}
import org.specs2.mock.Mockito
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner
import play.api.test.Helpers._
import play.api.test.{FakeRequest, WithApplication}
import reactivemongo.bson.BSONObjectID

import scala.concurrent.ExecutionContext
import scala.concurrent._

@RunWith(classOf[JUnitRunner])
class ConditionsManagerSpec extends Specification with Mockito {

  class ConditionsManagerTest extends ConditionsManagerLike

  case class matchRegex(a: String) extends Matcher[String]() {
    override def apply[S <: String](t: Expectable[S]): MatchResult[S] = result(a.r.findFirstIn(t.value).nonEmpty, "okMessage", t.value + "not found " + a, t)
  }

  case class contains(a: String, b: Int) extends Matcher[String]() {
    override def apply[S <: String](t: Expectable[S]): MatchResult[S] = result(a.r.findAllMatchIn(t.value).size == b, "okMessage", "not found " + b + " times but " + a.r.findAllMatchIn(t.value).size + " times " + a, t)
  }

  val bson = BSONObjectID.generate
  val bson2 = BSONObjectID.generate
  val bson3 = BSONObjectID.generate
  val bson4 = BSONObjectID.generate

  def fixture=new {
    val conditionDaoMock=mock[ConditionDao]
    val campaignDaoMock=mock[CampagneDao]
    val controller=new ConditionsManagerTest{
      override val conditionsDao=conditionDaoMock
      override val campaignDao=campaignDaoMock
    }
  }

  "When user is not connected, ConditionsManager" should {
    "redirect to login for resource /campaigns/campaign/:id" in new WithApplication {
      route(FakeRequest(GET, "/campaigns/campaign/"+bson.stringify)).map(
        r => {
          status(r) must equalTo(SEE_OTHER)
          header("Location", r) must equalTo(Some("/login"))
        }
      ).getOrElse(
          failure("Pas de retour de la fonction")
        )
    }
  }

  "When user is on resource /campaigns/campaign/:id, ConditionsManager" should{
    "send 200 Ok page" in new WithApplication {
      val f=fixture
      val camp=Campagne(bson,"camp","type",List())

      f.campaignDaoMock.findById(org.mockito.Matchers.eq(bson))(any[ExecutionContext]) returns future{Some(camp)}

      val r=f.controller.listConditions(bson.stringify).apply(FakeRequest(GET,"/campaigns/campaign/"+bson.stringify).withSession("user" -> """{"login":"test"}"""))

      status(r) must equalTo(OK)
      val content=contentAsString(r)
      content must contain("<h4>camp</h4>")
      content must contain("<div class=\"row\"><span class=\"bold\">Type</span> : type</div>")
      content must contain("<div class=\"row\"><span class=\"bold\">Conditions</span> : 0</div>")

      there was one(f.campaignDaoMock).findById(org.mockito.Matchers.eq(bson))(any[ExecutionContext])
    }

    "send redirect if campaign not found" in new WithApplication {
      val f=fixture

      f.campaignDaoMock.findById(org.mockito.Matchers.eq(bson))(any[ExecutionContext]) returns future{None}

      val r=f.controller.listConditions(bson.stringify).apply(FakeRequest(GET,"/campaigns/campaign/"+bson.stringify).withSession("user" -> """{"login":"test"}"""))

      status(r) must equalTo(SEE_OTHER)
      header("Location",r) must beSome("/campaigns")

      there was one(f.campaignDaoMock).findById(org.mockito.Matchers.eq(bson))(any[ExecutionContext])
    }
  }
}