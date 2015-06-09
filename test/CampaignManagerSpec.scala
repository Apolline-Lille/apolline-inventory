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
class CampaignManagerSpec extends Specification with Mockito {

  class CampaignManagerTest extends CampagneManagerLike

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

  def fixture = new {
    val campaignDaoMock=mock[CampagneDao]
    val controller=new CampaignManagerTest{
      override val campaignDao:CampagneDao=campaignDaoMock
    }
  }

  "When user is not connected, CampagneManager" should {
    "redirect to login for resource /campaigns" in new WithApplication {
      route(FakeRequest(GET, "/campaigns")).map(
        r => {
          status(r) must equalTo(SEE_OTHER)
          header("Location", r) must equalTo(Some("/login"))
        }
      ).getOrElse(
          failure("Pas de retour de la fonction")
        )
    }

    "redirect to login for resource /campaigns/campaign" in new WithApplication {
      route(FakeRequest(GET, "/campaigns/campaign")).map(
        r => {
          status(r) must equalTo(SEE_OTHER)
          header("Location", r) must equalTo(Some("/login"))
        }
      ).getOrElse(
          failure("Pas de retour de la fonction")
        )
    }

    "redirect to login for resource /campaigns/campaign" in new WithApplication {
      route(FakeRequest(POST, "/campaigns/campaign")).map(
        r => {
          status(r) must equalTo(SEE_OTHER)
          header("Location", r) must equalTo(Some("/login"))
        }
      ).getOrElse(
          failure("Pas de retour de la fonction")
        )
    }

    "redirect to login for resource /campaigns/campaign/:id/update" in new WithApplication {
      route(FakeRequest(GET, "/campaigns/campaign/:id/update")).map(
        r => {
          status(r) must equalTo(SEE_OTHER)
          header("Location", r) must equalTo(Some("/login"))
        }
      ).getOrElse(
          failure("Pas de retour de la fonction")
        )
    }

    "redirect to login for resource /campaigns/campaign/:id/update" in new WithApplication {
      route(FakeRequest(POST, "/campaigns/campaign/:id/update")).map(
        r => {
          status(r) must equalTo(SEE_OTHER)
          header("Location", r) must equalTo(Some("/login"))
        }
      ).getOrElse(
          failure("Pas de retour de la fonction")
        )
    }
  }

  "When user is on resource /campaigns, CampagneManager" should{
    "send 200 OK with the message 'Aucun résultats trouvé' if no campaign found" in new WithApplication{
      val f=fixture

      f.campaignDaoMock.findAll(org.mockito.Matchers.eq(Json.obj("delete"->false)),any[JsObject])(any[ExecutionContext]) returns future{List()}

      val r=f.controller.listCampaign().apply(FakeRequest(GET,"/campaigns").withSession("user" -> """{"login":"test"}"""))

      status(r) must equalTo(OK)
      contentAsString(r) must contain("<h3 style=\"text-align:center\">Aucun résultat trouvé</h3>")

      there was one(f.campaignDaoMock).findAll(org.mockito.Matchers.eq(Json.obj("delete"->false)),any[JsObject])(any[ExecutionContext])
    }

    "send 200 OK with 1 result" in new WithApplication{
      val f=fixture
      val campaign=List(
        Campagne(bson,"camp1","type",List(),false)
      )

      f.campaignDaoMock.findAll(org.mockito.Matchers.eq(Json.obj("delete"->false)),any[JsObject])(any[ExecutionContext]) returns future{campaign}

      val r=f.controller.listCampaign().apply(FakeRequest(GET,"/campaigns").withSession("user" -> """{"login":"test"}"""))

      status(r) must equalTo(OK)
      val content=contentAsString(r)
      content must not contain("<h3 style=\"text-align:center\">Aucun résultat trouvé</h3>")
      content must contain("camp1")
      content must contain("<div class=\"row\"><span class=\"bold\">Type</span> : type</div>")
      content must contain("<div class=\"row\"><span class=\"bold\">Conditions</span> : 0</div>")

      there was one(f.campaignDaoMock).findAll(org.mockito.Matchers.eq(Json.obj("delete"->false)),any[JsObject])(any[ExecutionContext])
    }

    "send 200 OK with 2 result" in new WithApplication{
      val f=fixture
      val campaign=List(
        Campagne(bson,"camp1","type1",List(),false),
        Campagne(bson2,"camp2","type2",List(bson3,bson4),false)
      )

      f.campaignDaoMock.findAll(org.mockito.Matchers.eq(Json.obj("delete"->false)),any[JsObject])(any[ExecutionContext]) returns future{campaign}

      val r=f.controller.listCampaign().apply(FakeRequest(GET,"/campaigns").withSession("user" -> """{"login":"test"}"""))

      status(r) must equalTo(OK)
      val content=contentAsString(r)
      content must not contain("<h3 style=\"text-align:center\">Aucun résultat trouvé</h3>")
      content must contain("camp1")
      content must contain("<div class=\"row\"><span class=\"bold\">Type</span> : type1</div>")
      content must contain("<div class=\"row\"><span class=\"bold\">Conditions</span> : 0</div>")
      content must contain("camp2")
      content must contain("<div class=\"row\"><span class=\"bold\">Type</span> : type2</div>")
      content must contain("<div class=\"row\"><span class=\"bold\">Conditions</span> : 2</div>")

      there was one(f.campaignDaoMock).findAll(org.mockito.Matchers.eq(Json.obj("delete"->false)),any[JsObject])(any[ExecutionContext])
    }
  }

  "When user is on resource /campaigns/campaign, CampagneManager" should{
    "send 200 OK with an empty form" in new WithApplication{
      val f=fixture

      val r=f.controller.addCampaign().apply(FakeRequest(GET, "/campaigns/campaign").withSession("user" -> """{"login":"test"}"""))

      status(r) must equalTo(OK)
      contentAsString(r) must contain("<input type=\"text\" id=\"nom\" name=\"nom\" value=\"\" class=\"form-control\"/>")
      contentAsString(r) must contain("<select id=\"types\" name=\"types\" class=\"form-control\">")
    }

    "send redirect after insert campaign" in new WithApplication{
      val f=fixture
      val data=Json.parse("""{"nom":"campagne","types":"type"}""")
      val lastError=mock[LastError]

      f.campaignDaoMock.findAll(org.mockito.Matchers.eq(Json.obj("nom"->"campagne")),any[JsObject])(any[ExecutionContext]) returns future{List()}
      f.campaignDaoMock.insert(any[Campagne],any[GetLastError])(any[ExecutionContext]) returns future{lastError}

      val r=f.controller.insertCampaign().apply(FakeRequest(POST, "/campaigns/campaign").withJsonBody(data).withSession("user" -> """{"login":"test"}"""))

      status(r) must equalTo(SEE_OTHER)
      header("Location",r) must beSome("/campaigns")

      there was one(f.campaignDaoMock).findAll(org.mockito.Matchers.eq(Json.obj("nom"->"campagne")),any[JsObject])(any[ExecutionContext])
      there was one(f.campaignDaoMock).insert(any[Campagne],any[GetLastError])(any[ExecutionContext])
    }

    "send redirect after reactivat campaign" in new WithApplication{
      val f=fixture
      val data=Json.parse("""{"nom":"campagne","types":"type","send":"Réactiver"}""")
      val lastError=mock[LastError]
      val campaign=Campagne(bson,"campagne","type",List(),true)

      f.campaignDaoMock.findAll(org.mockito.Matchers.eq(Json.obj("nom"->"campagne")),any[JsObject])(any[ExecutionContext]) returns future{List(campaign)}
      f.campaignDaoMock.findOne(org.mockito.Matchers.eq(Json.obj("nom"->"campagne")))(any[ExecutionContext]) returns future{Some(campaign)}
      f.campaignDaoMock.updateById(org.mockito.Matchers.eq(bson),any[Campagne],any[GetLastError])(any[Writes[Campagne]],any[ExecutionContext]) returns future{lastError}

      val r=f.controller.insertCampaign().apply(FakeRequest(POST, "/campaigns/campaign").withJsonBody(data).withSession("user" -> """{"login":"test"}"""))

      status(r) must equalTo(SEE_OTHER)
      header("Location",r) must beSome("/campaigns")

      there was one(f.campaignDaoMock).findAll(org.mockito.Matchers.eq(Json.obj("nom"->"campagne")),any[JsObject])(any[ExecutionContext])
      there was one(f.campaignDaoMock).findOne(org.mockito.Matchers.eq(Json.obj("nom"->"campagne")))(any[ExecutionContext])
      there was one(f.campaignDaoMock).updateById(org.mockito.Matchers.eq(bson),any[Campagne],any[GetLastError])(any[Writes[Campagne]],any[ExecutionContext])
    }

    "send 500 internal error if mongoDB error when insert campaign" in new WithApplication{
      val f=fixture
      val data=Json.parse("""{"nom":"campagne","types":"type"}""")
      val lastError=mock[Future[LastError]]
      val throwable=mock[Throwable]

      f.campaignDaoMock.findAll(org.mockito.Matchers.eq(Json.obj("nom"->"campagne")),any[JsObject])(any[ExecutionContext]) returns future{List()}
      f.campaignDaoMock.insert(any[Campagne],any[GetLastError])(any[ExecutionContext]) returns lastError
      lastError.map(any[LastError=>LastError])(any[ExecutionContext]) returns lastError
      lastError.recover(any[PartialFunction[Throwable,Result]])(any[ExecutionContext]) answers {value => future{value.asInstanceOf[PartialFunction[Throwable,Result]](throwable)}}

      val r=f.controller.insertCampaign().apply(FakeRequest(POST, "/campaigns/campaign").withJsonBody(data).withSession("user" -> """{"login":"test"}"""))

      status(r) must equalTo(INTERNAL_SERVER_ERROR)

      there was one(f.campaignDaoMock).findAll(org.mockito.Matchers.eq(Json.obj("nom"->"campagne")),any[JsObject])(any[ExecutionContext])
      there was one(f.campaignDaoMock).insert(any[Campagne],any[GetLastError])(any[ExecutionContext])
      there was one(lastError).map(any[LastError=>LastError])(any[ExecutionContext])
      there was one(lastError).recover(any[PartialFunction[Throwable,Result]])(any[ExecutionContext])
    }
  }

  "When user is on resource /campaigns/campaign/:id/update, CampagneManager" should{
    "send 200 OK with a prefilled form" in new WithApplication{
      val f=fixture
      val camp=Campagne(bson,"campagne","type",List(),false)

      f.campaignDaoMock.findOne(org.mockito.Matchers.eq(Json.obj("_id"->bson,"delete"->false)))(any[ExecutionContext]) returns future{Some(camp)}

      val r=f.controller.updateCampaignPage(bson.stringify).apply(FakeRequest(GET, "/campaigns/campaign/"+bson.stringify+"/update").withSession("user" -> """{"login":"test"}"""))

      status(r) must equalTo(OK)
      contentAsString(r) must contain("<input type=\"text\" id=\"nom\" name=\"nom\" value=\"campagne\" class=\"form-control\"/>")

      there was one(f.campaignDaoMock).findOne(org.mockito.Matchers.eq(Json.obj("_id"->bson,"delete"->false)))(any[ExecutionContext])
    }

    "send redirect if campaign not found" in new WithApplication{
      val f=fixture

      f.campaignDaoMock.findOne(org.mockito.Matchers.eq(Json.obj("_id"->bson,"delete"->false)))(any[ExecutionContext]) returns future{None}

      val r=f.controller.updateCampaignPage(bson.stringify).apply(FakeRequest(GET, "/campaigns/campaign/"+bson.stringify+"/update").withSession("user" -> """{"login":"test"}"""))

      status(r) must equalTo(SEE_OTHER)
      header("Location",r) must beSome("/campaigns")

      there was one(f.campaignDaoMock).findOne(org.mockito.Matchers.eq(Json.obj("_id"->bson,"delete"->false)))(any[ExecutionContext])
    }

    "send 500 internal error if mongoDB error when find campaign before update" in new WithApplication{
      val f=fixture
      val campaignError=mock[Future[Option[Campagne]]]
      val throwable=mock[Throwable]

      f.campaignDaoMock.findOne(org.mockito.Matchers.eq(Json.obj("_id"->bson,"delete"->false)))(any[ExecutionContext]) returns campaignError
      campaignError.map(any[Option[Campagne]=>Option[Campagne]])(any[ExecutionContext]) returns campaignError
      campaignError.recover(any[PartialFunction[Throwable,Result]])(any[ExecutionContext]) answers {value => future{value.asInstanceOf[PartialFunction[Throwable,Result]](throwable)}}

      val r=f.controller.updateCampaignPage(bson.stringify).apply(FakeRequest(GET, "/campaigns/campaign/"+bson.stringify+"/update").withSession("user" -> """{"login":"test"}"""))

      status(r) must equalTo(INTERNAL_SERVER_ERROR)

      there was one(f.campaignDaoMock).findOne(org.mockito.Matchers.eq(Json.obj("_id"->bson,"delete"->false)))(any[ExecutionContext])
      there was one(campaignError).map(any[Option[Campagne]=>Option[Campagne]])(any[ExecutionContext])
      there was one(campaignError).recover(any[PartialFunction[Throwable,Result]])(any[ExecutionContext])
    }

    "send redirect after update campaign" in new WithApplication{
      val f=fixture
      val data=Json.parse("""{"nom":"campagne","types":"type"}""")
      val lastError=mock[LastError]
      val camp=Campagne(bson,"campagne","type",List(),false)

      f.campaignDaoMock.findAll(org.mockito.Matchers.eq(Json.obj("nom"->"campagne","_id"->Json.obj("$ne"->bson))),any[JsObject])(any[ExecutionContext]) returns future{List()}
      f.campaignDaoMock.findOne(org.mockito.Matchers.eq(Json.obj("_id"->bson,"delete"->false)))(any[ExecutionContext]) returns future{Some(camp)}
      f.campaignDaoMock.updateById(org.mockito.Matchers.eq(bson),any[Campagne],any[GetLastError])(any[Writes[Campagne]],any[ExecutionContext]) returns future{lastError}

      val r=f.controller.updateCampaign(bson.stringify).apply(FakeRequest(POST, "/campaigns/campaign").withJsonBody(data).withSession("user" -> """{"login":"test"}"""))

      status(r) must equalTo(SEE_OTHER)
      header("Location",r) must beSome("/campaigns")

      there was one(f.campaignDaoMock).findAll(org.mockito.Matchers.eq(Json.obj("nom"->"campagne","_id"->Json.obj("$ne"->bson))),any[JsObject])(any[ExecutionContext])
      there was one(f.campaignDaoMock).findOne(org.mockito.Matchers.eq(Json.obj("_id"->bson,"delete"->false)))(any[ExecutionContext])
      there was one(f.campaignDaoMock).updateById(org.mockito.Matchers.eq(bson),any[Campagne],any[GetLastError])(any[Writes[Campagne]],any[ExecutionContext])
    }

    "send redirect if campaign not found before update campaign" in new WithApplication{
      val f=fixture
      val data=Json.parse("""{"nom":"campagne","types":"type"}""")
      val lastError=mock[LastError]

      f.campaignDaoMock.findAll(org.mockito.Matchers.eq(Json.obj("nom"->"campagne","_id"->Json.obj("$ne"->bson))),any[JsObject])(any[ExecutionContext]) returns future{List()}
      f.campaignDaoMock.findOne(org.mockito.Matchers.eq(Json.obj("_id"->bson,"delete"->false)))(any[ExecutionContext]) returns future{None}

      val r=f.controller.updateCampaign(bson.stringify).apply(FakeRequest(POST, "/campaigns/campaign").withJsonBody(data).withSession("user" -> """{"login":"test"}"""))

      status(r) must equalTo(SEE_OTHER)
      header("Location",r) must beSome("/campaigns")

      there was one(f.campaignDaoMock).findAll(org.mockito.Matchers.eq(Json.obj("nom"->"campagne","_id"->Json.obj("$ne"->bson))),any[JsObject])(any[ExecutionContext])
      there was one(f.campaignDaoMock).findOne(org.mockito.Matchers.eq(Json.obj("_id"->bson,"delete"->false)))(any[ExecutionContext])
    }

    "send 500 internal error if mongoDB error when find campaign before update" in new WithApplication{
      val f=fixture
      val data=Json.parse("""{"nom":"campagne","types":"type"}""")
      val campaignError=mock[Future[Option[Campagne]]]
      val throwable=mock[Throwable]

      f.campaignDaoMock.findAll(org.mockito.Matchers.eq(Json.obj("nom"->"campagne","_id"->Json.obj("$ne"->bson))),any[JsObject])(any[ExecutionContext]) returns future{List()}
      f.campaignDaoMock.findOne(org.mockito.Matchers.eq(Json.obj("_id"->bson,"delete"->false)))(any[ExecutionContext]) returns campaignError
      campaignError.flatMap(any[Option[Campagne]=>Future[Option[Campagne]]])(any[ExecutionContext]) returns campaignError
      campaignError.recover(any[PartialFunction[Throwable,Result]])(any[ExecutionContext]) answers {value => future{value.asInstanceOf[PartialFunction[Throwable,Result]](throwable)}}

      val r=f.controller.updateCampaign(bson.stringify).apply(FakeRequest(POST, "/campaigns/campaign/"+bson.stringify+"/update").withJsonBody(data).withSession("user" -> """{"login":"test"}"""))

      status(r) must equalTo(INTERNAL_SERVER_ERROR)

      there was one(f.campaignDaoMock).findAll(org.mockito.Matchers.eq(Json.obj("nom"->"campagne","_id"->Json.obj("$ne"->bson))),any[JsObject])(any[ExecutionContext])
      there was one(f.campaignDaoMock).findOne(org.mockito.Matchers.eq(Json.obj("_id"->bson,"delete"->false)))(any[ExecutionContext])
      there was one(campaignError).flatMap(any[Option[Campagne]=>Future[Option[Campagne]]])(any[ExecutionContext])
      there was one(campaignError).recover(any[PartialFunction[Throwable,Result]])(any[ExecutionContext])
    }

    "send 500 internal error if mongoDB error when update campaign" in new WithApplication{
      val f=fixture
      val data=Json.parse("""{"nom":"campagne","types":"type"}""")
      val lastError=mock[Future[LastError]]
      val throwable=mock[Throwable]
      val camp=Campagne(bson,"campagne","type",List(),false)

      f.campaignDaoMock.findAll(org.mockito.Matchers.eq(Json.obj("nom"->"campagne","_id"->Json.obj("$ne"->bson))),any[JsObject])(any[ExecutionContext]) returns future{List()}
      f.campaignDaoMock.findOne(org.mockito.Matchers.eq(Json.obj("_id"->bson,"delete"->false)))(any[ExecutionContext]) returns future{Some(camp)}
      f.campaignDaoMock.updateById(org.mockito.Matchers.eq(bson),any[Campagne],any[GetLastError])(any[Writes[Campagne]],any[ExecutionContext]) returns lastError
      lastError.map(any[LastError=>LastError])(any[ExecutionContext]) returns lastError
      lastError.recover(any[PartialFunction[Throwable,Result]])(any[ExecutionContext]) answers {value => future{value.asInstanceOf[PartialFunction[Throwable,Result]](throwable)}}

      val r=f.controller.updateCampaign(bson.stringify).apply(FakeRequest(POST, "/campaigns/campaign/"+bson.stringify+"/update").withJsonBody(data).withSession("user" -> """{"login":"test"}"""))

      status(r) must equalTo(INTERNAL_SERVER_ERROR)

      there was one(f.campaignDaoMock).findAll(org.mockito.Matchers.eq(Json.obj("nom"->"campagne","_id"->Json.obj("$ne"->bson))),any[JsObject])(any[ExecutionContext])
      there was one(f.campaignDaoMock).findOne(org.mockito.Matchers.eq(Json.obj("_id"->bson,"delete"->false)))(any[ExecutionContext])
      there was one(f.campaignDaoMock).updateById(org.mockito.Matchers.eq(bson),any[Campagne],any[GetLastError])(any[Writes[Campagne]],any[ExecutionContext])
      there was one(lastError).map(any[LastError=>LastError])(any[ExecutionContext])
      there was one(lastError).recover(any[PartialFunction[Throwable,Result]])(any[ExecutionContext])
    }
  }

  "When method submitForm is called, CampagneManager" should{
    "send bad_request with form if form submit with no value" in new WithApplication{
      val f=fixture
      val func=mock[CampaignForm=>Future[Result]]
      val verif=mock[CampaignForm=>JsObject]

      val req=FakeRequest(POST, "/campaigns/campaign").withSession("user" -> """{"login":"test"}""")
      val action=Action.async{implicit request=>f.controller.submitForm("error",routes.CampagneManager.addCampaign())(verif)(func)}
      val r=call(action,req)

      status(r) must equalTo(BAD_REQUEST)
      contentAsString(r) must contain("<span class=\"control-label errors\">This field is required</span>")
    }

    "Execute function if campaign not exist and form is valid" in new WithApplication{
      val f=fixture
      val func=mock[CampaignForm=>Future[Result]]
      val data=Json.parse("""{"nom":"campagne","types":"types"}""")
      val verif=mock[CampaignForm=>JsObject]
      val jsObj=Json.obj("nom" -> "campagne")

      f.campaignDaoMock.findAll(org.mockito.Matchers.eq(jsObj),any[JsObject])(any[ExecutionContext]) returns future{List()}
      func.apply(any[CampaignForm]) returns future{Results.Ok("execute func")}
      verif.apply(any[CampaignForm]) returns jsObj

      val req=FakeRequest(POST, "/campaigns/campaign").withJsonBody(data).withSession("user" -> """{"login":"test"}""")
      val action=Action.async{implicit request=>f.controller.submitForm("error",routes.CampagneManager.addCampaign())(verif)(func)}
      val r=call(action,req)

      status(r) must equalTo(OK)
      contentAsString(r) must equalTo("execute func")

      there was one(f.campaignDaoMock).findAll(org.mockito.Matchers.eq(jsObj),any[JsObject])(any[ExecutionContext])
      there was one(func).apply(any[CampaignForm])
      there was one(verif).apply(any[CampaignForm])
    }

    "Send error message if campaign exist and delete" in new WithApplication{
      val f=fixture
      val func=mock[CampaignForm=>Future[Result]]
      val data=Json.parse("""{"nom":"campagne","types":"types"}""")
      val camp=Campagne(nom="campagne",types="types",conditions=List(),delete=true)
      val verif=mock[CampaignForm=>JsObject]
      val jsObj=Json.obj("nom" -> "campagne")

      f.campaignDaoMock.findAll(org.mockito.Matchers.eq(jsObj),any[JsObject])(any[ExecutionContext]) returns future{List(camp)}
      verif.apply(any[CampaignForm]) returns jsObj

      val req=FakeRequest(POST, "/campaigns/campaign").withJsonBody(data).withSession("user" -> """{"login":"test"}""")
      val action=Action.async{implicit request=>f.controller.submitForm("error",routes.CampagneManager.addCampaign())(verif)(func)}
      val r=call(action,req)

      status(r) must equalTo(BAD_REQUEST)
      contentAsString(r) must contain("<div class=\"alert alert-danger\" role=\"alert\">error</div>")

      there was one(f.campaignDaoMock).findAll(org.mockito.Matchers.eq(jsObj),any[JsObject])(any[ExecutionContext])
      there was one(verif).apply(any[CampaignForm])
    }

    "Execute function if campaign exist and send value is \"Réactiver\" or \"Ignorer\"" in new WithApplication{
      val f=fixture
      val func=mock[CampaignForm=>Future[Result]]
      val data=Json.parse("""{"nom":"campagne","types":"types","send":"Ignorer"}""")
      val camp=Campagne(nom="campagne",types="types",conditions=List(),delete=true)
      val verif=mock[CampaignForm=>JsObject]
      val jsObj=Json.obj("nom" -> "campagne")

      f.campaignDaoMock.findAll(org.mockito.Matchers.eq(jsObj),any[JsObject])(any[ExecutionContext]) returns future{List(camp)}
      func.apply(any[CampaignForm]) returns future{Results.Ok("execute func")}
      verif.apply(any[CampaignForm]) returns jsObj

      val req=FakeRequest(POST, "/campaigns/campaign").withJsonBody(data).withSession("user" -> """{"login":"test"}""")
      val action=Action.async{implicit request=>f.controller.submitForm("error",routes.CampagneManager.addCampaign())(verif)(func)}
      val r=call(action,req)

      status(r) must equalTo(OK)
      contentAsString(r) must equalTo("execute func")

      there was one(f.campaignDaoMock).findAll(org.mockito.Matchers.eq(jsObj),any[JsObject])(any[ExecutionContext])
      there was one(func).apply(any[CampaignForm])
      there was one(verif).apply(any[CampaignForm])
    }

    "send bad request if campaign exist and not delete" in new WithApplication{
      val f=fixture
      val func=mock[CampaignForm=>Future[Result]]
      val data=Json.parse("""{"nom":"campagne","types":"types"}""")
      val camp=Campagne(nom="campagne",types="types",conditions=List())
      val verif=mock[CampaignForm=>JsObject]
      val jsObj=Json.obj("nom" -> "campagne")

      f.campaignDaoMock.findAll(org.mockito.Matchers.eq(jsObj),any[JsObject])(any[ExecutionContext]) returns future{List(camp)}
      verif.apply(any[CampaignForm]) returns jsObj

      val req=FakeRequest(POST, "/campaigns/campaign").withJsonBody(data).withSession("user" -> """{"login":"test"}""")
      val action=Action.async{implicit request=>f.controller.submitForm("error",routes.CampagneManager.addCampaign())(verif)(func)}
      val r=call(action,req)

      status(r) must equalTo(BAD_REQUEST)
      contentAsString(r) must contain("<div class=\"alert alert-danger\" role=\"alert\">Cette campagne existe déjà</div>")

      there was one(f.campaignDaoMock).findAll(org.mockito.Matchers.eq(jsObj),any[JsObject])(any[ExecutionContext])
      there was one(verif).apply(any[CampaignForm])
    }
  }

  "When method updateWithColumnDelete is called, CampagneManager" should{
    "send result not found if campaign not found" in new WithApplication {
      val f = fixture
      val notFound=mock[Result]
      val found=mock[Result]

      f.campaignDaoMock.findOne(any[JsObject])(any[ExecutionContext]) returns future{None}

      val r=f.controller.updateWithColumnDelete(Json.obj(),true,notFound,found)

      val res=Await.result(r,Duration.Inf)
      res must equalTo(notFound)

      there was one(f.campaignDaoMock).findOne(any[JsObject])(any[ExecutionContext])
    }

    "send result found after update campaign" in new WithApplication {
      val f = fixture
      val notFound=mock[Result]
      val found=mock[Result]
      val camp=Campagne(bson,"campagne","types",List(),true)
      val lastError=mock[LastError]

      f.campaignDaoMock.findOne(any[JsObject])(any[ExecutionContext]) returns future{Some(camp)}
      f.campaignDaoMock.updateById(org.mockito.Matchers.eq(bson),org.mockito.Matchers.eq(camp.copy(delete=false)),any[GetLastError])(any[Writes[Campagne]],any[ExecutionContext]) returns future{lastError}

      val r=f.controller.updateWithColumnDelete(Json.obj(),false,notFound,found)

      val res=Await.result(r,Duration.Inf)
      res must equalTo(found)

      there was one(f.campaignDaoMock).findOne(any[JsObject])(any[ExecutionContext])
      there was one(f.campaignDaoMock).updateById(org.mockito.Matchers.eq(bson),org.mockito.Matchers.eq(camp.copy(delete=false)),any[GetLastError])(any[Writes[Campagne]],any[ExecutionContext])
    }

    "send 500 internal error if mongoDB error when find campaign" in new WithApplication {
      val f = fixture
      val notFound=mock[Result]
      val found=mock[Result]
      val futureMock=mock[Future[Option[Campagne]]]
      val throwable=mock[Throwable]

      f.campaignDaoMock.findOne(any[JsObject])(any[ExecutionContext]) returns futureMock
      futureMock.flatMap(any[Option[Campagne]=>Future[Option[Campagne]]])(any[ExecutionContext]) returns futureMock
      futureMock.recover(any[PartialFunction[Throwable,Result]])(any[ExecutionContext]) answers {value => future{value.asInstanceOf[PartialFunction[Throwable,Result]](throwable)}}

      val r=f.controller.updateWithColumnDelete(Json.obj(),true,notFound,found)

      status(r) must equalTo(INTERNAL_SERVER_ERROR)

      there was one(f.campaignDaoMock).findOne(any[JsObject])(any[ExecutionContext])
      there was one(futureMock).flatMap(any[Option[Campagne]=>Future[Option[Campagne]]])(any[ExecutionContext])
      there was one(futureMock).recover(any[PartialFunction[Throwable,Result]])(any[ExecutionContext])
    }

    "send result found after update campaign" in new WithApplication {
      val f = fixture
      val notFound=mock[Result]
      val found=mock[Result]
      val camp=Campagne(bson,"campagne","types",List(),true)
      val lastError=mock[Future[LastError]]
      val throwable=mock[Throwable]

      f.campaignDaoMock.findOne(any[JsObject])(any[ExecutionContext]) returns future{Some(camp)}
      f.campaignDaoMock.updateById(org.mockito.Matchers.eq(bson),org.mockito.Matchers.eq(camp.copy(delete=false)),any[GetLastError])(any[Writes[Campagne]],any[ExecutionContext]) returns lastError
      lastError.map(any[LastError=>LastError])(any[ExecutionContext]) returns lastError
      lastError.recover(any[PartialFunction[Throwable,Result]])(any[ExecutionContext]) answers {value => future{value.asInstanceOf[PartialFunction[Throwable,Result]](throwable)}}

      val r=f.controller.updateWithColumnDelete(Json.obj(),false,notFound,found)

      status(r) must equalTo(INTERNAL_SERVER_ERROR)

      there was one(f.campaignDaoMock).findOne(any[JsObject])(any[ExecutionContext])
      there was one(f.campaignDaoMock).updateById(org.mockito.Matchers.eq(bson),org.mockito.Matchers.eq(camp.copy(delete=false)),any[GetLastError])(any[Writes[Campagne]],any[ExecutionContext])
      there was one(lastError).map(any[LastError=>LastError])(any[ExecutionContext])
      there was one(lastError).recover(any[PartialFunction[Throwable,Result]])(any[ExecutionContext])
    }
  }
}