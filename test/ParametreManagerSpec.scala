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

    "redirect to login for resource /campaigns/parameters/parameter" in new WithApplication {
      route(FakeRequest(POST, "/campaigns/parameters/parameter")).map(
        r => {
          status(r) must equalTo(SEE_OTHER)
          header("Location", r) must equalTo(Some("/login"))
        }
      ).getOrElse(
          failure("Pas de retour de la fonction")
        )
    }

    "redirect to login for resource /campaigns/parameters/parameter/:id" in new WithApplication {
      route(FakeRequest(GET, "/campaigns/parameters/parameter/"+bson.stringify)).map(
        r => {
          status(r) must equalTo(SEE_OTHER)
          header("Location", r) must equalTo(Some("/login"))
        }
      ).getOrElse(
          failure("Pas de retour de la fonction")
        )
    }
  }

  "When user is on resource /campaigns/parameters" should{
    "send 200 Ok page with the message 'Aucun résultat trouvé'" in new WithApplication{
      val f=fixture

      f.parameterDaoMock.findAll(any[JsObject],any[JsObject])(any[ExecutionContext]) returns future{List()}

      val req=FakeRequest(GET,"/campaigns/parameters").withSession("user"->"""{"login":"test"}""")
      val r=f.controller.listParameter().apply(req)

      status(r) must equalTo(OK)
      val content=contentAsString(r)
      content must contain("<h3 style=\"text-align:center\">Aucun résultat trouvé</h3>")

      there was one(f.parameterDaoMock).findAll(any[JsObject],any[JsObject])(any[ExecutionContext])
    }

    "send 200 Ok page with 1 result" in new WithApplication{
      val f=fixture
      val param=Parametres(cle="key",valeur="value")

      f.parameterDaoMock.findAll(any[JsObject],any[JsObject])(any[ExecutionContext]) returns future{List(param)}

      val req=FakeRequest(GET,"/campaigns/parameters").withSession("user"->"""{"login":"test"}""")
      val r=f.controller.listParameter().apply(req)

      status(r) must equalTo(OK)
      val content=contentAsString(r)
      content must contain("<td>key</td>")
      content must contain("<td>value</td>")

      there was one(f.parameterDaoMock).findAll(any[JsObject],any[JsObject])(any[ExecutionContext])
    }

    "send 200 Ok page with 2 result" in new WithApplication{
      val f=fixture
      val params=List(Parametres(cle="key",valeur="value"),Parametres(cle="key2",valeur="value2"))

      f.parameterDaoMock.findAll(any[JsObject],any[JsObject])(any[ExecutionContext]) returns future{params}

      val req=FakeRequest(GET,"/campaigns/parameters").withSession("user"->"""{"login":"test"}""")
      val r=f.controller.listParameter().apply(req)

      status(r) must equalTo(OK)
      val content=contentAsString(r)
      content must contain("<td>key</td>")
      content must contain("<td>value</td>")
      content must contain("<td>key2</td>")
      content must contain("<td>value2</td>")

      there was one(f.parameterDaoMock).findAll(any[JsObject],any[JsObject])(any[ExecutionContext])
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

    "send redirect after insert parameter" in new WithApplication{
      val f=fixture
      val data=Json.parse("""{"key":"cle","value":"valeur"}""")
      val lastError=mock[LastError]

      f.parameterDaoMock.findOne(org.mockito.Matchers.eq(Json.obj("cle"->"cle")))(any[ExecutionContext]) returns future{None}
      f.parameterDaoMock.insert(any[Parametres],any[GetLastError])(any[ExecutionContext]) returns future{lastError}

      val req=FakeRequest(POST,"/campaigns/parameters/parameter").withJsonBody(data).withSession("user"->"""{"login":"test"}""")
      val r=f.controller.addParameter().apply(req)

      there was one(f.parameterDaoMock).findOne(org.mockito.Matchers.eq(Json.obj("cle"->"cle")))(any[ExecutionContext])
      there was one(f.parameterDaoMock).insert(any[Parametres],any[GetLastError])(any[ExecutionContext])
    }
  }

  "When user is on resource /campaigns/parameters/parameter/:id" should{
    "send 200 Ok page with a prefilled form" in new WithApplication{
      val f=fixture
      val param=Parametres(cle="cle",valeur="valeur")

      f.parameterDaoMock.findById(org.mockito.Matchers.eq(bson))(any[ExecutionContext]) returns future{Some(param)}

      val r=f.controller.updateParameterPage(bson.stringify).apply(FakeRequest(GET,"/campaigns/parameters/parameter/"+bson.stringify).withSession("user"->"""{"login":"test"}"""))

      status(r) must equalTo(OK)
      val content=contentAsString(r)
      content must contain("<input type=\"text\" id=\"key\" name=\"key\" value=\"cle\" class=\"form-control\"/>")
      content must contain("<input type=\"text\" id=\"value\" name=\"value\" value=\"valeur\" class=\"form-control\"/>")

      there was one(f.parameterDaoMock).findById(org.mockito.Matchers.eq(bson))(any[ExecutionContext])
    }

    "send redirect if parameter not found" in new WithApplication{
      val f=fixture

      f.parameterDaoMock.findById(org.mockito.Matchers.eq(bson))(any[ExecutionContext]) returns future{None}

      val r=f.controller.updateParameterPage(bson.stringify).apply(FakeRequest(GET,"/campaigns/parameters/parameter/"+bson.stringify).withSession("user"->"""{"login":"test"}"""))

      status(r) must equalTo(SEE_OTHER)
      header("Location",r) must beSome("/campaigns/parameters")

      there was one(f.parameterDaoMock).findById(org.mockito.Matchers.eq(bson))(any[ExecutionContext])
    }

    "send redirect after update parameter" in new WithApplication{
      val f=fixture
      val data=Json.parse("""{"key":"cle","value":"valeur"}""")
      val lastError=mock[LastError]
      val param=Parametres(bson,"cle","valeur")

      f.parameterDaoMock.findOne(org.mockito.Matchers.eq(Json.obj("cle"->"cle","_id"->Json.obj("$ne"->bson))))(any[ExecutionContext]) returns future{None}
      f.parameterDaoMock.updateById(org.mockito.Matchers.eq(bson),org.mockito.Matchers.eq(param),any[GetLastError])(any[Writes[Parametres]],any[ExecutionContext]) returns future{lastError}

      val req=FakeRequest(POST,"/campaigns/parameters/parameter/"+bson.stringify).withJsonBody(data).withSession("user"->"""{"login":"test"}""")
      val r=f.controller.updateParameter(bson.stringify).apply(req)

      there was one(f.parameterDaoMock).findOne(org.mockito.Matchers.eq(Json.obj("cle"->"cle","_id"->Json.obj("$ne"->bson))))(any[ExecutionContext])
      there was one(f.parameterDaoMock).updateById(org.mockito.Matchers.eq(bson),org.mockito.Matchers.eq(param),any[GetLastError])(any[Writes[Parametres]],any[ExecutionContext])
    }
  }

  "When user is on resource /campaigns/parameters/parameter/:id/delete, ParametreManager" should{
    "send redirect after delete the parameter" in new WithApplication{
      val f=fixture
      val lastError=mock[LastError]

      f.parameterDaoMock.removeById(org.mockito.Matchers.eq(bson),any[GetLastError])(any[ExecutionContext]) returns future{lastError}

      val r=f.controller.deleteParameter(bson.stringify).apply(FakeRequest(GET,"/campaigns/parameters/parameter/"+bson.stringify+"/delete").withSession("user"->"""{"login":"test"}"""))

      status(r) must equalTo(SEE_OTHER)
      header("Location",r) must beSome("/campaigns/parameters")

      there was one(f.parameterDaoMock).removeById(org.mockito.Matchers.eq(bson),any[GetLastError])(any[ExecutionContext])
    }
  }

  "When method submitForm is called, ParametreManager" should{
    "send bad request if form is submit with empty field" in new WithApplication{
      val f=fixture
      val route=routes.ParametreManager.listParameter()
      val verif=mock[ParameterForm=>JsObject]
      val func=mock[ParameterForm=>Future[Result]]

      val req=FakeRequest(POST,"/campaigns/parameters/parameter").withSession("user"->"""{"login":"test"}""")
      val action=Action.async{implicit request=>f.controller.submitForm(route)(verif)(func)}
      val r=call(action,req)

      status(r) must equalTo(BAD_REQUEST)
      contentAsString(r) must contains("<span class=\"control-label errors\">This field is required</span>",2)

      there was no(verif).apply(any[ParameterForm])
      there was no(func).apply(any[ParameterForm])
    }

    "send bad request if parameter exist" in new WithApplication{
      val f=fixture
      val data=Json.parse("""{"key":"cle","value":"valeur"}""")
      val param=mock[Parametres]
      val route=routes.ParametreManager.listParameter()
      val verif=mock[ParameterForm=>JsObject]
      val func=mock[ParameterForm=>Future[Result]]

      verif.apply(any[ParameterForm]) returns Json.obj("cle"->"cle")
      f.parameterDaoMock.findOne(org.mockito.Matchers.eq(Json.obj("cle"->"cle")))(any[ExecutionContext]) returns future{Some(param)}

      val req=FakeRequest(POST,"/campaigns/parameters/parameter").withJsonBody(data).withSession("user"->"""{"login":"test"}""")
      val action=Action.async{implicit request=>f.controller.submitForm(route)(verif)(func)}
      val r=call(action,req)

      status(r) must equalTo(BAD_REQUEST)
      contentAsString(r) must contain("<div class=\"alert alert-danger\" role=\"alert\">Ce paramètre existe déjà</div>")

      there was one(f.parameterDaoMock).findOne(org.mockito.Matchers.eq(Json.obj("cle"->"cle")))(any[ExecutionContext])
      there was one(verif).apply(any[ParameterForm])
      there was no(func).apply(any[ParameterForm])
    }

    "execute dedicated function" in new WithApplication{
      val f=fixture
      val data=Json.parse("""{"key":"cle","value":"valeur"}""")
      val route=routes.ParametreManager.listParameter()
      val verif=mock[ParameterForm=>JsObject]
      val func=mock[ParameterForm=>Future[Result]]

      verif.apply(any[ParameterForm]) returns Json.obj("cle"->"cle")
      f.parameterDaoMock.findOne(org.mockito.Matchers.eq(Json.obj("cle"->"cle")))(any[ExecutionContext]) returns future{None}
      func.apply(any[ParameterForm]) returns future{Results.Ok("exec func")}

      val req=FakeRequest(POST,"/campaigns/parameters/parameter").withJsonBody(data).withSession("user"->"""{"login":"test"}""")
      val action=Action.async{implicit request=>f.controller.submitForm(route)(verif)(func)}
      val r=call(action,req)

      status(r) must equalTo(OK)
      contentAsString(r) must equalTo("exec func")

      there was one(f.parameterDaoMock).findOne(org.mockito.Matchers.eq(Json.obj("cle"->"cle")))(any[ExecutionContext])
      there was one(verif).apply(any[ParameterForm])
      there was one(func).apply(any[ParameterForm])
    }
  }
}