
import controllers.TypeModuleManagerLike
import models.TypeModuleDao
import org.junit.runner.RunWith
import org.specs2.matcher.{MatchResult, Expectable, Matcher}
import org.specs2.mock.Mockito
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner
import play.api.test.Helpers._
import play.api.test.{FakeRequest, WithApplication}

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
  }
}