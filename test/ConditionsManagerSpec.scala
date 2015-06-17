import java.text.ParseException
import java.util.Date

import controllers._
import models._
import org.junit.runner.RunWith
import org.specs2.matcher.{MatchResult, Expectable, Matcher}
import org.specs2.mock.Mockito
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner
import play.api.data.Form
import play.api.libs.json.{JsObject, Json}
import play.api.mvc.{Action, Results, Result}
import play.api.test.Helpers._
import play.api.test.{FakeRequest, WithApplication}
import reactivemongo.bson.{BSONDocument, BSONObjectID}
import reactivemongo.extensions.BSONFormats.BSONObjectIDFormat

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

  val date=new Date(115,3,22)
  val date2=new Date(115,3,23)

  val bson = BSONObjectID.generate
  val bson2 = BSONObjectID.generate
  val bson3 = BSONObjectID.generate
  val bson4 = BSONObjectID.generate

  def fixture=new {
    val conditionDaoMock=mock[ConditionDao]
    val moduleDaoMock=mock[ModuleDao]
    val paramDaoMock=mock[ParametresDao]
    val localisationDaoMock=mock[LocalisationDao]
    val campaignManagerMock=mock[CampagneManagerLike]
    val moduleManagerMock=mock[ModuleManagerLike]
    val controller=new ConditionsManagerTest{
      override val conditionsDao=conditionDaoMock
      override val moduleDao=moduleDaoMock
      override val parameterDao=paramDaoMock
      override val localisationDao=localisationDaoMock
      override val campaignManager=campaignManagerMock
      override val moduleManager=moduleManagerMock
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

    "redirect to login for resource /campaigns/campaign/:id/form" in new WithApplication {
      route(FakeRequest(GET, "/campaigns/campaign/"+bson.stringify+"/form")).map(
        r => {
          status(r) must equalTo(SEE_OTHER)
          header("Location", r) must equalTo(Some("/login"))
        }
      ).getOrElse(
          failure("Pas de retour de la fonction")
        )
    }

    "redirect to login for resource /campaigns/campaign/:id/form for POST request" in new WithApplication {
      route(FakeRequest(POST, "/campaigns/campaign/"+bson.stringify+"/form")).map(
        r => {
          status(r) must equalTo(SEE_OTHER)
          header("Location", r) must equalTo(Some("/login"))
        }
      ).getOrElse(
          failure("Pas de retour de la fonction")
        )
    }

    "redirect to login for resource /campaigns/campaign/:id/form/module" in new WithApplication {
      route(FakeRequest(GET, "/campaigns/campaign/"+bson.stringify+"/form/module")).map(
        r => {
          status(r) must equalTo(SEE_OTHER)
          header("Location", r) must equalTo(Some("/login"))
        }
      ).getOrElse(
          failure("Pas de retour de la fonction")
        )
    }

    "redirect to login for resource /campaigns/campaign/:id/form/module for POST request" in new WithApplication {
      route(FakeRequest(POST, "/campaigns/campaign/"+bson.stringify+"/form/module")).map(
        r => {
          status(r) must equalTo(SEE_OTHER)
          header("Location", r) must equalTo(Some("/login"))
        }
      ).getOrElse(
          failure("Pas de retour de la fonction")
        )
    }

    "redirect to login for resource /campaigns/campaign/:id/form/parameter" in new WithApplication {
      route(FakeRequest(GET, "/campaigns/campaign/"+bson.stringify+"/form/parameter")).map(
        r => {
          status(r) must equalTo(SEE_OTHER)
          header("Location", r) must equalTo(Some("/login"))
        }
      ).getOrElse(
          failure("Pas de retour de la fonction")
        )
    }

    "redirect to login for resource /campaigns/campaign/:id/form/localisation" in new WithApplication {
      route(FakeRequest(GET, "/campaigns/campaign/"+bson.stringify+"/form/localisation")).map(
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

      f.campaignManagerMock.doIfCampaignFound(org.mockito.Matchers.eq(bson))(any[Campagne=>Future[Result]])(any[Unit=>Future[Result]]) answers {(params,_) => params match{
        case Array(_,p:(Campagne=>Future[Result]),_) => p.apply(camp)
      }}

      val r=f.controller.listConditions(bson.stringify).apply(FakeRequest(GET,"/campaigns/campaign/"+bson.stringify).withSession("user" -> """{"login":"test"}"""))

      status(r) must equalTo(OK)
      val content=contentAsString(r)
      content must contain("<h4>camp</h4>")
      content must contain("<div class=\"row\"><span class=\"bold\">Type</span> : type</div>")
      content must contain("<div class=\"row\"><span class=\"bold\">Conditions</span> : 0</div>")

      there was one(f.campaignManagerMock).doIfCampaignFound(org.mockito.Matchers.eq(bson))(any[Campagne=>Future[Result]])(any[Unit=>Future[Result]])
    }

    "send redirect if campaign not found" in new WithApplication {
      val f=fixture

      f.campaignManagerMock.doIfCampaignFound(org.mockito.Matchers.eq(bson))(any[Campagne=>Future[Result]])(any[Unit=>Future[Result]]) answers {(params,_) => params match{
        case Array(_,_,p:(Unit=>Future[Result])) => p.apply()
      }}

      val r=f.controller.listConditions(bson.stringify).apply(FakeRequest(GET,"/campaigns/campaign/"+bson.stringify).withSession("user" -> """{"login":"test"}"""))

      status(r) must equalTo(SEE_OTHER)
      header("Location",r) must beSome("/campaigns")

      there was one(f.campaignManagerMock).doIfCampaignFound(org.mockito.Matchers.eq(bson))(any[Campagne=>Future[Result]])(any[Unit=>Future[Result]])
    }
  }

  "When user is on resource /campaigns/campaign/:id/form, ConditionsManager" should{
    "send 200 Ok page with an empty form" in new WithApplication {
      val f=fixture
      val camp=Campagne(bson,"camp","type",List())

      f.campaignManagerMock.doIfCampaignFound(org.mockito.Matchers.eq(bson))(any[Campagne=>Future[Result]])(any[Unit=>Future[Result]]) answers {(params,_) => params match{
        case Array(_,p:(Campagne=>Future[Result]),_) => p.apply(camp)
      }}

      val r=f.controller.formGeneral(bson.stringify).apply(FakeRequest(GET,"/campaigns/campaign/"+bson.stringify+"/form").withSession("user" -> """{"login":"test"}"""))

      status(r) must equalTo(OK)
      val content=contentAsString(r)
      content must matchRegex("<input type=\"text\" id=\"debut\" name=\"debut\" value=\"\\d{2}/\\d{2}/\\d{4} \\d{2}:\\d{2}:\\d{2}\" class=\"form-control\" placeholder=\"dd/mm/YYYY hh:mm:ss\"/>")
      content must contain("<input type=\"text\" id=\"fin\" name=\"fin\" value=\"\" class=\"form-control\" placeholder=\"dd/mm/YYYY hh:mm:ss\"/>")
      content must contain("<textarea id=\"commentaire\" name=\"commentaire\" class=\"form-control\"></textarea>")

      there was one(f.campaignManagerMock).doIfCampaignFound(org.mockito.Matchers.eq(bson))(any[Campagne=>Future[Result]])(any[Unit=>Future[Result]])
    }

    "send redirect if campaign not found" in new WithApplication {
      val f=fixture

      f.campaignManagerMock.doIfCampaignFound(org.mockito.Matchers.eq(bson))(any[Campagne=>Future[Result]])(any[Unit=>Future[Result]]) answers {(params,_) => params match{
        case Array(_,_,p:(Unit=>Future[Result])) => p.apply()
      }}

      val r=f.controller.formGeneral(bson.stringify).apply(FakeRequest(GET,"/campaigns/campaign/"+bson.stringify+"/form").withSession("user" -> """{"login":"test"}"""))

      status(r) must equalTo(SEE_OTHER)
      header("Location",r) must beSome("/campaigns")

      there was one(f.campaignManagerMock).doIfCampaignFound(org.mockito.Matchers.eq(bson))(any[Campagne=>Future[Result]])(any[Unit=>Future[Result]])
    }

    "send redirect if campaign not found when submit data" in new WithApplication {
      val f=fixture

      f.campaignManagerMock.doIfCampaignFound(org.mockito.Matchers.eq(bson))(any[Campagne=>Future[Result]])(any[Unit=>Future[Result]]) answers {(params,_) => params match{
        case Array(_,_,p:(Unit=>Future[Result])) => p.apply()
      }}

      val r=f.controller.addGeneralInformation(bson.stringify).apply(FakeRequest(POST,"/campaigns/campaign/"+bson.stringify+"/form").withSession("user" -> """{"login":"test"}"""))

      status(r) must equalTo(SEE_OTHER)
      header("Location",r) must beSome("/campaigns")

      there was one(f.campaignManagerMock).doIfCampaignFound(org.mockito.Matchers.eq(bson))(any[Campagne=>Future[Result]])(any[Unit=>Future[Result]])
    }

    "send bad request if the form is send with empty field" in new WithApplication{
      val f=fixture
      val camp=Campagne(bson,"camp","type",List())

      f.campaignManagerMock.doIfCampaignFound(org.mockito.Matchers.eq(bson))(any[Campagne=>Future[Result]])(any[Unit=>Future[Result]]) answers {(params,_) => params match{
        case Array(_,p:(Campagne=>Future[Result]),_) => p.apply(camp)
      }}

      val req=FakeRequest(POST,"/campaigns/campaign/"+bson.stringify+"/form").withSession("user" -> """{"login":"test"}""" , "condition" -> """{}""")
      val r=f.controller.addGeneralInformation(bson.stringify).apply(req)

      status(r) must equalTo(BAD_REQUEST)
      val content=contentAsString(r)
      content must contain("<span class=\"control-label errors\">This field is required</span>")

      there was one(f.campaignManagerMock).doIfCampaignFound(org.mockito.Matchers.eq(bson))(any[Campagne=>Future[Result]])(any[Unit=>Future[Result]])
    }

    "send Redirect if data are valid" in new WithApplication{
      val f=fixture
      val camp=Campagne(bson,"camp","type",List())
      val data=Json.parse("""{"debut":"22/04/2015 00:00:00","fin":"23/04/2015 00:00:00","commentaire":"un com"}""")

      f.campaignManagerMock.doIfCampaignFound(org.mockito.Matchers.eq(bson))(any[Campagne=>Future[Result]])(any[Unit=>Future[Result]]) answers {(params,_) => params match{
        case Array(_,p:(Campagne=>Future[Result]),_) => p.apply(camp)
      }}

      val req=FakeRequest(POST,"/campaigns/campaign/"+bson.stringify+"/form").withJsonBody(data).withSession("user" -> """{"login":"test"}""" , "condition" -> """{}""")
      val r=f.controller.addGeneralInformation(bson.stringify).apply(req)

      status(r) must equalTo(SEE_OTHER)
      header("Location",r) must beSome("/campaigns/campaign/"+bson.stringify+"/form/module")
      val s=session(r)
      s.get("condition") must not beNone
      val cond=Json.parse(s.get("condition").getOrElse("{}"))
      new Date(cond.\("debut").as[Long]) must equalTo(date)
      new Date(cond.\("fin").as[Long]) must equalTo(date2)
      cond.\("commentaire").as[String] must equalTo("un com")

      there was one(f.campaignManagerMock).doIfCampaignFound(org.mockito.Matchers.eq(bson))(any[Campagne=>Future[Result]])(any[Unit=>Future[Result]])
    }

    "send bad request with the form if begin date is not valid" in new WithApplication{
      val f=fixture
      val camp=Campagne(bson,"camp","type",List())
      val data=Json.parse("""{"debut":"52/04/2015 00:00:00"}""")

      f.campaignManagerMock.doIfCampaignFound(org.mockito.Matchers.eq(bson))(any[Campagne=>Future[Result]])(any[Unit=>Future[Result]]) answers {(params,_) => params match{
        case Array(_,p:(Campagne=>Future[Result]),_) => p.apply(camp)
      }}

      val req=FakeRequest(POST,"/campaigns/campaign/"+bson.stringify+"/form").withJsonBody(data).withSession("user" -> """{"login":"test"}""" , "condition" -> """{}""")
      val r=f.controller.addGeneralInformation(bson.stringify).apply(req)

      status(r) must equalTo(BAD_REQUEST)
      val content=contentAsString(r)
      content must matchRegex("<input type=\"text\" id=\"debut\" name=\"debut\" value=\"52/04/2015 00:00:00\" class=\"form-control\" placeholder=\"dd/mm/YYYY hh:mm:ss\"/>")
      content must contain("<input type=\"text\" id=\"fin\" name=\"fin\" value=\"\" class=\"form-control\" placeholder=\"dd/mm/YYYY hh:mm:ss\"/>")
      content must contain("<textarea id=\"commentaire\" name=\"commentaire\" class=\"form-control\"></textarea>")
      content must contain("<span class=\"control-label errors\">Valid date required</span>")

      there was one(f.campaignManagerMock).doIfCampaignFound(org.mockito.Matchers.eq(bson))(any[Campagne=>Future[Result]])(any[Unit=>Future[Result]])
    }

    "send bad request with the form if end date is not valid" in new WithApplication{
      val f=fixture
      val camp=Campagne(bson,"camp","type",List())
      val data=Json.parse("""{"debut":"22/04/2015 00:00:00","fin":"52/04/2015 00:00:00"}""")

      f.campaignManagerMock.doIfCampaignFound(org.mockito.Matchers.eq(bson))(any[Campagne=>Future[Result]])(any[Unit=>Future[Result]]) answers {(params,_) => params match{
        case Array(_,p:(Campagne=>Future[Result]),_) => p.apply(camp)
      }}

      val req=FakeRequest(POST,"/campaigns/campaign/"+bson.stringify+"/form").withJsonBody(data).withSession("user" -> """{"login":"test"}""" , "condition" -> """{}""")
      val r=f.controller.addGeneralInformation(bson.stringify).apply(req)

      status(r) must equalTo(BAD_REQUEST)
      val content=contentAsString(r)
      content must matchRegex("<input type=\"text\" id=\"debut\" name=\"debut\" value=\"22/04/2015 00:00:00\" class=\"form-control\" placeholder=\"dd/mm/YYYY hh:mm:ss\"/>")
      content must contain("<input type=\"text\" id=\"fin\" name=\"fin\" value=\"52/04/2015 00:00:00\" class=\"form-control\" placeholder=\"dd/mm/YYYY hh:mm:ss\"/>")
      content must contain("<textarea id=\"commentaire\" name=\"commentaire\" class=\"form-control\"></textarea>")
      content must contain("<span class=\"control-label errors\">Valid date required</span>")

      there was one(f.campaignManagerMock).doIfCampaignFound(org.mockito.Matchers.eq(bson))(any[Campagne=>Future[Result]])(any[Unit=>Future[Result]])
    }

    "send bad request with the form if end date is before begin date" in new WithApplication{
      val f=fixture
      val camp=Campagne(bson,"camp","type",List())
      val data=Json.parse("""{"debut":"23/04/2015 00:00:00","fin":"22/04/2015 00:00:00"}""")

      f.campaignManagerMock.doIfCampaignFound(org.mockito.Matchers.eq(bson))(any[Campagne=>Future[Result]])(any[Unit=>Future[Result]]) answers {(params,_) => params match{
        case Array(_,p:(Campagne=>Future[Result]),_) => p.apply(camp)
      }}

      val req=FakeRequest(POST,"/campaigns/campaign/"+bson.stringify+"/form").withJsonBody(data).withSession("user" -> """{"login":"test"}""" , "condition" -> """{}""")
      val r=f.controller.addGeneralInformation(bson.stringify).apply(req)

      status(r) must equalTo(BAD_REQUEST)
      val content=contentAsString(r)
      content must matchRegex("<input type=\"text\" id=\"debut\" name=\"debut\" value=\"23/04/2015 00:00:00\" class=\"form-control\" placeholder=\"dd/mm/YYYY hh:mm:ss\"/>")
      content must contain("<input type=\"text\" id=\"fin\" name=\"fin\" value=\"22/04/2015 00:00:00\" class=\"form-control\" placeholder=\"dd/mm/YYYY hh:mm:ss\"/>")
      content must contain("<textarea id=\"commentaire\" name=\"commentaire\" class=\"form-control\"></textarea>")
      content must contain("<div class=\"alert alert-danger\" role=\"alert\">La date de début doit être inférieur à la date de fin</div>")

      there was one(f.campaignManagerMock).doIfCampaignFound(org.mockito.Matchers.eq(bson))(any[Campagne=>Future[Result]])(any[Unit=>Future[Result]])
    }
  }

  "When user is on resource /campaigns/campaign/:id/form/module, ConditionsManager" should{
    "send 200 Ok page with message 'Aucun résultat trouvé'" in new WithApplication {
      val f=fixture
      val camp=Campagne(bson,"camp","type",List())

      f.campaignManagerMock.doIfCampaignFound(org.mockito.Matchers.eq(bson))(any[Campagne=>Future[Result]])(any[Unit=>Future[Result]]) answers {(params,_) => params match{
        case Array(_,p:(Campagne=>Future[Result]),_) => p.apply(camp)
      }}

      f.moduleManagerMock.getInventaryModule(org.mockito.Matchers.eq(""))(any[(List[Module],List[BSONDocument])=>Result]) answers {(params,_)=>params match{
        case Array(_,p:((List[Module],List[BSONDocument])=>Result)) => future{p.apply(List(),List())}
      }}

      val r=f.controller.formModule(bson.stringify,"").apply(FakeRequest(GET,"/campaigns/campaign/"+bson.stringify+"/form/module").withSession("user" -> """{"login":"test"}"""))

      status(r) must equalTo(OK)
      contentAsString(r) must contain("<h3 style=\"text-align:center\">Aucun résultat trouvé</h3>")

      there was one(f.campaignManagerMock).doIfCampaignFound(org.mockito.Matchers.eq(bson))(any[Campagne=>Future[Result]])(any[Unit=>Future[Result]])
      there was one(f.moduleManagerMock).getInventaryModule(org.mockito.Matchers.eq(""))(any[(List[Module],List[BSONDocument])=>Result])
    }

    "send 200 Ok page with 1 result" in new WithApplication {
      val f=fixture
      val camp=Campagne(bson,"camp","type",List())
      val modules=List(
        Module(bson,"idMod","typesMod",date,List(bson),List(bson),Some("un com"))
      )

      f.campaignManagerMock.doIfCampaignFound(org.mockito.Matchers.eq(bson))(any[Campagne=>Future[Result]])(any[Unit=>Future[Result]]) answers {(params,_) => params match{
        case Array(_,p:(Campagne=>Future[Result]),_) => p.apply(camp)
      }}

      f.moduleManagerMock.getInventaryModule(org.mockito.Matchers.eq(""))(any[(List[Module],List[BSONDocument])=>Result]) answers {(params,_)=>params match{
        case Array(_,p:((List[Module],List[BSONDocument])=>Result)) => future{p.apply(modules,List())}
      }}

      val r=f.controller.formModule(bson.stringify,"").apply(FakeRequest(GET,"/campaigns/campaign/"+bson.stringify+"/form/module").withSession("user" -> """{"login":"test"}"""))

      status(r) must equalTo(OK)
      val content = contentAsString(r)
      content must contain("typesMod")
      content must contain("idMod")
      content must contain("<div class=\"row\"><span class=\"bold\">Commentaires</span> : un com</div>")
      content must contain("<div class=\"row\"><span class=\"bold\">Date d&#x27;assemblage</span> : 22/04/2015</div>")
      content must contain("<div class=\"row\"><span class=\"bold\">Cartes</span> : 1</div>")
      content must contain("<div class=\"row\"><span class=\"bold\">Capteurs</span> : 1</div>")

      there was one(f.campaignManagerMock).doIfCampaignFound(org.mockito.Matchers.eq(bson))(any[Campagne=>Future[Result]])(any[Unit=>Future[Result]])
      there was one(f.moduleManagerMock).getInventaryModule(org.mockito.Matchers.eq(""))(any[(List[Module],List[BSONDocument])=>Result])
    }

    "send 200 Ok page with 2 result" in new WithApplication {
      val f=fixture
      val camp=Campagne(bson,"camp","type",List())
      val modules=List(
        Module(bson,"idMod","typesMod",date,List(bson),List(bson),Some("un com")),
        Module(bson,"idMod2","typesMod2",date,List(bson,bson2),List(bson,bson2,bson3),Some("un com2"))
      )

      f.campaignManagerMock.doIfCampaignFound(org.mockito.Matchers.eq(bson))(any[Campagne=>Future[Result]])(any[Unit=>Future[Result]]) answers {(params,_) => params match{
        case Array(_,p:(Campagne=>Future[Result]),_) => p.apply(camp)
      }}

      f.moduleManagerMock.getInventaryModule(org.mockito.Matchers.eq(""))(any[(List[Module],List[BSONDocument])=>Result]) answers {(params,_)=>params match{
        case Array(_,p:((List[Module],List[BSONDocument])=>Result)) => future{p.apply(modules,List())}
      }}

      val r=f.controller.formModule(bson.stringify,"").apply(FakeRequest(GET,"/campaigns/campaign/"+bson.stringify+"/form/module").withSession("user" -> """{"login":"test"}"""))

      status(r) must equalTo(OK)
      val content = contentAsString(r)
      content must contain("typesMod")
      content must contain("idMod")
      content must contain("<div class=\"row\"><span class=\"bold\">Commentaires</span> : un com</div>")
      content must contain("<div class=\"row\"><span class=\"bold\">Date d&#x27;assemblage</span> : 22/04/2015</div>")
      content must contain("<div class=\"row\"><span class=\"bold\">Cartes</span> : 1</div>")
      content must contain("<div class=\"row\"><span class=\"bold\">Capteurs</span> : 1</div>")
      content must contain("typesMod2")
      content must contain("idMod2")
      content must contain("<div class=\"row\"><span class=\"bold\">Commentaires</span> : un com2</div>")
      content must contain("<div class=\"row\"><span class=\"bold\">Cartes</span> : 2</div>")
      content must contain("<div class=\"row\"><span class=\"bold\">Capteurs</span> : 3</div>")

      there was one(f.campaignManagerMock).doIfCampaignFound(org.mockito.Matchers.eq(bson))(any[Campagne=>Future[Result]])(any[Unit=>Future[Result]])
      there was one(f.moduleManagerMock).getInventaryModule(org.mockito.Matchers.eq(""))(any[(List[Module],List[BSONDocument])=>Result])
    }

    "send redirect if campaign not found" in new WithApplication {
      val f=fixture

      f.campaignManagerMock.doIfCampaignFound(org.mockito.Matchers.eq(bson))(any[Campagne=>Future[Result]])(any[Unit=>Future[Result]]) answers {(params,_) => params match{
        case Array(_,_,p:(Unit=>Future[Result])) => p.apply()
      }}

      val r=f.controller.formModule(bson.stringify,"").apply(FakeRequest(GET,"/campaigns/campaign/"+bson.stringify+"/form/module").withSession("user" -> """{"login":"test"}"""))

      status(r) must equalTo(SEE_OTHER)
      header("Location",r) must beSome("/campaigns")

      there was one(f.campaignManagerMock).doIfCampaignFound(org.mockito.Matchers.eq(bson))(any[Campagne=>Future[Result]])(any[Unit=>Future[Result]])
    }

    "send redirect if campaign not found when select module" in new WithApplication {
      val f=fixture

      f.campaignManagerMock.doIfCampaignFound(org.mockito.Matchers.eq(bson))(any[Campagne=>Future[Result]])(any[Unit=>Future[Result]]) answers {(params,_) => params match{
        case Array(_,_,p:(Unit=>Future[Result])) => p.apply()
      }}

      val r=f.controller.addSelectedModule(bson.stringify).apply(FakeRequest(POST,"/campaigns/campaign/"+bson.stringify+"/form/module").withSession("user" -> """{"login":"test"}"""))

      status(r) must equalTo(SEE_OTHER)
      header("Location",r) must beSome("/campaigns")

      there was one(f.campaignManagerMock).doIfCampaignFound(org.mockito.Matchers.eq(bson))(any[Campagne=>Future[Result]])(any[Unit=>Future[Result]])
    }

    "send bad request with the list of modules if have error in data received" in new WithApplication{
      val f=fixture
      val camp=Campagne(bson,"camp","type",List())
      val modules=List(
        Module(bson,"idMod","typesMod",date,List(bson),List(bson),Some("un com"))
      )

      f.campaignManagerMock.doIfCampaignFound(org.mockito.Matchers.eq(bson))(any[Campagne=>Future[Result]])(any[Unit=>Future[Result]]) answers {(params,_) => params match{
        case Array(_,p:(Campagne=>Future[Result]),_) => p.apply(camp)
      }}

      f.moduleManagerMock.getInventaryModule(org.mockito.Matchers.eq(""))(any[(List[Module],List[BSONDocument])=>Result]) answers {(params,_)=>params match{
        case Array(_,p:((List[Module],List[BSONDocument])=>Result)) => future{p.apply(modules,List())}
      }}

      val r=f.controller.addSelectedModule(bson.stringify).apply(FakeRequest(POST,"/campaigns/campaign/"+bson.stringify+"/form/module").withSession("user" -> """{"login":"test"}"""))

      status(r) must equalTo(BAD_REQUEST)
      val content = contentAsString(r)
      content must contain("typesMod")
      content must contain("idMod")
      content must contain("<div class=\"row\"><span class=\"bold\">Commentaires</span> : un com</div>")
      content must contain("<div class=\"row\"><span class=\"bold\">Date d&#x27;assemblage</span> : 22/04/2015</div>")
      content must contain("<div class=\"row\"><span class=\"bold\">Cartes</span> : 1</div>")
      content must contain("<div class=\"row\"><span class=\"bold\">Capteurs</span> : 1</div>")

      there was one(f.campaignManagerMock).doIfCampaignFound(org.mockito.Matchers.eq(bson))(any[Campagne=>Future[Result]])(any[Unit=>Future[Result]])
      there was one(f.moduleManagerMock).getInventaryModule(org.mockito.Matchers.eq(""))(any[(List[Module],List[BSONDocument])=>Result])
    }

    "send bad request with the list of modules if module not found" in new WithApplication{
      val f=fixture
      val camp=Campagne(bson,"camp","type",List())
      val modules=List(
        Module(bson,"idMod","typesMod",date,List(bson),List(bson),Some("un com"))
      )
      val data=Json.obj("id"->bson2.stringify)

      f.campaignManagerMock.doIfCampaignFound(org.mockito.Matchers.eq(bson))(any[Campagne=>Future[Result]])(any[Unit=>Future[Result]]) answers {(params,_) => params match{
        case Array(_,p:(Campagne=>Future[Result]),_) => p.apply(camp)
      }}

      f.moduleManagerMock.getInventaryModule(org.mockito.Matchers.eq(""))(any[(List[Module],List[BSONDocument])=>Result]) answers {(params,_)=>params match{
        case Array(_,p:((List[Module],List[BSONDocument])=>Result)) => future{p.apply(modules,List())}
      }}

      f.moduleDaoMock.findOne(org.mockito.Matchers.eq(Json.obj("_id"->bson2,"delete"->false)))(any[ExecutionContext]) returns future{None}

      val req=FakeRequest(POST,"/campaigns/campaign/"+bson.stringify+"/form/module").withJsonBody(data).withSession("user" -> """{"login":"test"}""")
      val r=f.controller.addSelectedModule(bson.stringify).apply(req)

      status(r) must equalTo(BAD_REQUEST)
      val content = contentAsString(r)
      content must contain("typesMod")
      content must contain("idMod")
      content must contain("<div class=\"row\"><span class=\"bold\">Commentaires</span> : un com</div>")
      content must contain("<div class=\"row\"><span class=\"bold\">Date d&#x27;assemblage</span> : 22/04/2015</div>")
      content must contain("<div class=\"row\"><span class=\"bold\">Cartes</span> : 1</div>")
      content must contain("<div class=\"row\"><span class=\"bold\">Capteurs</span> : 1</div>")

      there was one(f.campaignManagerMock).doIfCampaignFound(org.mockito.Matchers.eq(bson))(any[Campagne=>Future[Result]])(any[Unit=>Future[Result]])
      there was one(f.moduleManagerMock).getInventaryModule(org.mockito.Matchers.eq(""))(any[(List[Module],List[BSONDocument])=>Result])
      there was one(f.moduleDaoMock).findOne(org.mockito.Matchers.eq(Json.obj("_id"->bson2,"delete"->false)))(any[ExecutionContext])
    }

    "send bad request with the list of modules if module not found" in new WithApplication{
      val f=fixture
      val camp=Campagne(bson,"camp","type",List())
      val module=Module(bson2,"idMod","typesMod",date,List(bson),List(bson),Some("un com"))
      val data=Json.obj("id"->bson2.stringify)

      f.campaignManagerMock.doIfCampaignFound(org.mockito.Matchers.eq(bson))(any[Campagne=>Future[Result]])(any[Unit=>Future[Result]]) answers {(params,_) => params match{
        case Array(_,p:(Campagne=>Future[Result]),_) => p.apply(camp)
      }}

      f.moduleDaoMock.findOne(org.mockito.Matchers.eq(Json.obj("_id"->bson2,"delete"->false)))(any[ExecutionContext]) returns future{Some(module)}

      val req=FakeRequest(POST,"/campaigns/campaign/"+bson.stringify+"/form/module").withJsonBody(data).withSession("user" -> """{"login":"test"}""")
      val r=f.controller.addSelectedModule(bson.stringify).apply(req)

      status(r) must equalTo(SEE_OTHER)

      there was one(f.campaignManagerMock).doIfCampaignFound(org.mockito.Matchers.eq(bson))(any[Campagne=>Future[Result]])(any[Unit=>Future[Result]])
      there was one(f.moduleDaoMock).findOne(org.mockito.Matchers.eq(Json.obj("_id"->bson2,"delete"->false)))(any[ExecutionContext])
    }
  }

  "When user is on resource /campaigns/campaign/:id/form/parameter, ConditionsManager" should{
    "send redirect if campaign not found" in new WithApplication{
      val f=fixture

      f.campaignManagerMock.doIfCampaignFound(org.mockito.Matchers.eq(bson))(any[Campagne=>Future[Result]])(any[Unit=>Future[Result]]) answers {(params,_) => params match{
        case Array(_,_,p:(Unit=>Future[Result])) => p.apply()
      }}

      val r=f.controller.formParam(bson.stringify).apply(FakeRequest(GET,"/campaigns/campaign/"+bson.stringify+"/form/parameter").withSession("user" -> """{"login":"test"}"""))

      status(r) must equalTo(SEE_OTHER)
      header("Location",r) must beSome("/campaigns")

      there was one(f.campaignManagerMock).doIfCampaignFound(org.mockito.Matchers.eq(bson))(any[Campagne=>Future[Result]])(any[Unit=>Future[Result]])
    }

    "send redirect if campaign type is not equal to 'Calibration'" in new WithApplication{
      val f=fixture
      val camp=Campagne(nom="camp",types="Test",conditions=List())

      f.campaignManagerMock.doIfCampaignFound(org.mockito.Matchers.eq(bson))(any[Campagne=>Future[Result]])(any[Unit=>Future[Result]]) answers {(params,_) => params match{
        case Array(_,p:(Campagne=>Future[Result]),_) => p.apply(camp)
      }}

      val r=f.controller.formParam(bson.stringify).apply(FakeRequest(GET,"/campaigns/campaign/"+bson.stringify+"/form/parameter").withSession("user" -> """{"login":"test"}"""))

      status(r) must equalTo(SEE_OTHER)
      header("Location",r) must beSome("/campaigns/campaign/"+bson.stringify)

      there was one(f.campaignManagerMock).doIfCampaignFound(org.mockito.Matchers.eq(bson))(any[Campagne=>Future[Result]])(any[Unit=>Future[Result]])
    }

    "send 200 Ok page with 'Aucun résultat trouvé' if not parameter found" in new WithApplication{
      val f=fixture
      val camp=Campagne(nom="camp",types="Calibration",conditions=List())

      f.campaignManagerMock.doIfCampaignFound(org.mockito.Matchers.eq(bson))(any[Campagne=>Future[Result]])(any[Unit=>Future[Result]]) answers {(params,_) => params match{
        case Array(_,p:(Campagne=>Future[Result]),_) => p.apply(camp)
      }}

      f.paramDaoMock.findAll(any[JsObject],any[JsObject])(any[ExecutionContext]) returns future{List()}

      val r=f.controller.formParam(bson.stringify).apply(FakeRequest(GET,"/campaigns/campaign/"+bson.stringify+"/form/parameter").withSession("user" -> """{"login":"test"}"""))

      status(r) must equalTo(OK)
      contentAsString(r) must contain("<h3 style=\"text-align:center\">Aucun résultat trouvé</h3>")

      there was one(f.campaignManagerMock).doIfCampaignFound(org.mockito.Matchers.eq(bson))(any[Campagne=>Future[Result]])(any[Unit=>Future[Result]])
      there was one(f.paramDaoMock).findAll(any[JsObject],any[JsObject])(any[ExecutionContext])
    }

    "send 200 Ok page with 1 result" in new WithApplication{
      val f=fixture
      val camp=Campagne(nom="camp",types="Calibration",conditions=List())
      val param=Parametres(cle="key",valeur="value")

      f.campaignManagerMock.doIfCampaignFound(org.mockito.Matchers.eq(bson))(any[Campagne=>Future[Result]])(any[Unit=>Future[Result]]) answers {(params,_) => params match{
        case Array(_,p:(Campagne=>Future[Result]),_) => p.apply(camp)
      }}

      f.paramDaoMock.findAll(any[JsObject],any[JsObject])(any[ExecutionContext]) returns future{List(param)}

      val r=f.controller.formParam(bson.stringify).apply(FakeRequest(GET,"/campaigns/campaign/"+bson.stringify+"/form/parameter").withSession("user" -> """{"login":"test"}"""))

      status(r) must equalTo(OK)
      val content=contentAsString(r)
      content must contain("<td>key</td>")
      content must contain("<td>value</td>")

      there was one(f.campaignManagerMock).doIfCampaignFound(org.mockito.Matchers.eq(bson))(any[Campagne=>Future[Result]])(any[Unit=>Future[Result]])
      there was one(f.paramDaoMock).findAll(any[JsObject],any[JsObject])(any[ExecutionContext])
    }

    "send 200 Ok page with 2 result" in new WithApplication{
      val f=fixture
      val camp=Campagne(nom="camp",types="Calibration",conditions=List())
      val param=List(
        Parametres(cle="key",valeur="value"),
        Parametres(cle="key2",valeur="value2")
      )

      f.campaignManagerMock.doIfCampaignFound(org.mockito.Matchers.eq(bson))(any[Campagne=>Future[Result]])(any[Unit=>Future[Result]]) answers {(params,_) => params match{
        case Array(_,p:(Campagne=>Future[Result]),_) => p.apply(camp)
      }}

      f.paramDaoMock.findAll(any[JsObject],any[JsObject])(any[ExecutionContext]) returns future{param}

      val r=f.controller.formParam(bson.stringify).apply(FakeRequest(GET,"/campaigns/campaign/"+bson.stringify+"/form/parameter").withSession("user" -> """{"login":"test"}"""))

      status(r) must equalTo(OK)
      val content=contentAsString(r)
      content must contain("<td>key</td>")
      content must contain("<td>value</td>")
      content must contain("<td>key2</td>")
      content must contain("<td>value2</td>")

      there was one(f.campaignManagerMock).doIfCampaignFound(org.mockito.Matchers.eq(bson))(any[Campagne=>Future[Result]])(any[Unit=>Future[Result]])
      there was one(f.paramDaoMock).findAll(any[JsObject],any[JsObject])(any[ExecutionContext])
    }
  }

  "When user is on resource /campaigns/campaign/:id/form/localisation, ConditionsManager" should{
    "send redirect if campaign not found" in new WithApplication{
      val f=fixture

      f.campaignManagerMock.doIfCampaignFound(org.mockito.Matchers.eq(bson))(any[Campagne=>Future[Result]])(any[Unit=>Future[Result]]) answers {(params,_) => params match{
        case Array(_,_,p:(Unit=>Future[Result])) => p.apply()
      }}

      val r=f.controller.formLocalisation(bson.stringify).apply(FakeRequest(GET,"/campaigns/campaign/"+bson.stringify+"/form/localisation").withSession("user" -> """{"login":"test"}"""))

      status(r) must equalTo(SEE_OTHER)
      header("Location",r) must beSome("/campaigns")

      there was one(f.campaignManagerMock).doIfCampaignFound(org.mockito.Matchers.eq(bson))(any[Campagne=>Future[Result]])(any[Unit=>Future[Result]])
    }

    "send redirect if campaign type is not equal to 'Terrain'" in new WithApplication{
      val f=fixture
      val camp=Campagne(nom="camp",types="Test",conditions=List())

      f.campaignManagerMock.doIfCampaignFound(org.mockito.Matchers.eq(bson))(any[Campagne=>Future[Result]])(any[Unit=>Future[Result]]) answers {(params,_) => params match{
        case Array(_,p:(Campagne=>Future[Result]),_) => p.apply(camp)
      }}

      val r=f.controller.formLocalisation(bson.stringify).apply(FakeRequest(GET,"/campaigns/campaign/"+bson.stringify+"/form/localisation").withSession("user" -> """{"login":"test"}"""))

      status(r) must equalTo(SEE_OTHER)
      header("Location",r) must beSome("/campaigns/campaign/"+bson.stringify)

      there was one(f.campaignManagerMock).doIfCampaignFound(org.mockito.Matchers.eq(bson))(any[Campagne=>Future[Result]])(any[Unit=>Future[Result]])
    }

    "send 200 Ok page with 'Aucun résultat trouvé' if not localisation found" in new WithApplication{
      val f=fixture
      val camp=Campagne(nom="camp",types="Terrain",conditions=List())

      f.campaignManagerMock.doIfCampaignFound(org.mockito.Matchers.eq(bson))(any[Campagne=>Future[Result]])(any[Unit=>Future[Result]]) answers {(params,_) => params match{
        case Array(_,p:(Campagne=>Future[Result]),_) => p.apply(camp)
      }}

      f.localisationDaoMock.findAll(any[JsObject],any[JsObject])(any[ExecutionContext]) returns future{List()}

      val r=f.controller.formLocalisation(bson.stringify).apply(FakeRequest(GET,"/campaigns/campaign/"+bson.stringify+"/form/localisation").withSession("user" -> """{"login":"test"}"""))

      status(r) must equalTo(OK)
      contentAsString(r) must contain("<h3 style=\"text-align:center\">Aucun résultat trouvé</h3>")

      there was one(f.campaignManagerMock).doIfCampaignFound(org.mockito.Matchers.eq(bson))(any[Campagne=>Future[Result]])(any[Unit=>Future[Result]])
      there was one(f.localisationDaoMock).findAll(any[JsObject],any[JsObject])(any[ExecutionContext])
    }

    "send 200 Ok page with 1 result" in new WithApplication{
      val f=fixture
      val camp=Campagne(nom="camp",types="Terrain",conditions=List())
      val localisation=Localisation(nom="unNom",lat=Some(1.2f),lon=Some(3.4f),commentaire=Some("un com"),photo=List())

      f.campaignManagerMock.doIfCampaignFound(org.mockito.Matchers.eq(bson))(any[Campagne=>Future[Result]])(any[Unit=>Future[Result]]) answers {(params,_) => params match{
        case Array(_,p:(Campagne=>Future[Result]),_) => p.apply(camp)
      }}

      f.localisationDaoMock.findAll(any[JsObject],any[JsObject])(any[ExecutionContext]) returns future{List(localisation)}

      val r=f.controller.formLocalisation(bson.stringify).apply(FakeRequest(GET,"/campaigns/campaign/"+bson.stringify+"/form/localisation").withSession("user" -> """{"login":"test"}"""))

      status(r) must equalTo(OK)
      contentType(r) must beSome.which(_ == "text/html")
      val content=contentAsString(r)
      content must contain("unNom")
      content must contain("<div class=\"row\"><span class=\"bold\">Commentaire</span> : un com</div>")
      content must contain("<div class=\"row\"><span class=\"bold\">Latitude</span> : 1.2</div>")
      content must contain("<div class=\"row\"><span class=\"bold\">Longitude</span> : 3.4</div>")
      content must contain("<div class=\"row\"><span class=\"bold\">Photo</span> : 0</div>")

      there was one(f.campaignManagerMock).doIfCampaignFound(org.mockito.Matchers.eq(bson))(any[Campagne=>Future[Result]])(any[Unit=>Future[Result]])
      there was one(f.localisationDaoMock).findAll(any[JsObject],any[JsObject])(any[ExecutionContext])
    }

    "send 200 Ok page with 2 result" in new WithApplication{
      val f=fixture
      val camp=Campagne(nom="camp",types="Terrain",conditions=List())
      val localisation=List(
        Localisation(nom="unNom",lat=Some(1.2f),lon=Some(3.4f),commentaire=Some("un com"),photo=List()),
        Localisation(nom="unNom2",lat=Some(5.6f),lon=Some(7.8f),commentaire=Some("un com2"),photo=List("img.jpg","img2.jpg"))
      )

      f.campaignManagerMock.doIfCampaignFound(org.mockito.Matchers.eq(bson))(any[Campagne=>Future[Result]])(any[Unit=>Future[Result]]) answers {(params,_) => params match{
        case Array(_,p:(Campagne=>Future[Result]),_) => p.apply(camp)
      }}

      f.localisationDaoMock.findAll(any[JsObject],any[JsObject])(any[ExecutionContext]) returns future{localisation}

      val r=f.controller.formLocalisation(bson.stringify).apply(FakeRequest(GET,"/campaigns/campaign/"+bson.stringify+"/form/localisation").withSession("user" -> """{"login":"test"}"""))

      status(r) must equalTo(OK)
      contentType(r) must beSome.which(_ == "text/html")
      val content=contentAsString(r)
      content must contain("unNom")
      content must contain("<div class=\"row\"><span class=\"bold\">Commentaire</span> : un com</div>")
      content must contain("<div class=\"row\"><span class=\"bold\">Latitude</span> : 1.2</div>")
      content must contain("<div class=\"row\"><span class=\"bold\">Longitude</span> : 3.4</div>")
      content must contain("<div class=\"row\"><span class=\"bold\">Photo</span> : 0</div>")
      content must contain("unNom2")
      content must contain("<div class=\"row\"><span class=\"bold\">Commentaire</span> : un com2</div>")
      content must contain("<div class=\"row\"><span class=\"bold\">Latitude</span> : 5.6</div>")
      content must contain("<div class=\"row\"><span class=\"bold\">Longitude</span> : 7.8</div>")
      content must contain("<div class=\"row\"><span class=\"bold\">Photo</span> : 2</div>")

      there was one(f.campaignManagerMock).doIfCampaignFound(org.mockito.Matchers.eq(bson))(any[Campagne=>Future[Result]])(any[Unit=>Future[Result]])
      there was one(f.localisationDaoMock).findAll(any[JsObject],any[JsObject])(any[ExecutionContext])
    }
  }

  "When method printStateForm is called, ConditionsManager" should{
    "display 3 links with 'general information', 'modules' and 'validate' for Test conditions" in new WithApplication{
      val f=fixture

      val r=f.controller.printStateForm("infoConditions","Test",bson.stringify)

      val content=contentAsString(r)
      content must contain("Informations générales")
      content must contain("Module")
      content must contain("Validation")
      content must not contain("Paramétre")
      content must not contain("Localisation")
    }

    "display 3 links with 'general information', 'modules', 'parameter' and 'validate' for Calibrations conditions" in new WithApplication{
      val f=fixture

      val r=f.controller.printStateForm("infoConditions","Calibration",bson.stringify)

      val content=contentAsString(r)
      content must contain("Informations générales")
      content must contain("Module")
      content must contain("Validation")
      content must contain("Paramétres")
      content must not contain("Localisation")
    }

    "display 3 links with 'general information', 'modules', 'localisation' and 'validate' for Ground conditions" in new WithApplication{
      val f=fixture

      val r=f.controller.printStateForm("infoConditions","Terrain",bson.stringify)

      val content=contentAsString(r)
      content must contain("Informations générales")
      content must contain("Module")
      content must contain("Validation")
      content must not contain("Paramétre")
      content must contain("Localisation")
    }
  }

  "When method verifyDate is called, ConditionsManager" should{
    "return true if end date is empty" in {
      val f=fixture

      f.controller.verifyDate(date,None) must beTrue
    }

    "return true if end date is after begin date" in {
      val f=fixture

      f.controller.verifyDate(date,Some(date2)) must beTrue
    }

    "return false if end date is before begin date" in {
      val f=fixture

      f.controller.verifyDate(date2,Some(date)) must beFalse
    }
  }

  "When method findCondition is called, ConditionsManager" should{
    "return an empty JsObject if session not contain key 'condition'" in new WithApplication{
      val f=fixture

      val req=FakeRequest(GET,"url")
      val r=f.controller.findCondition(req)

      r must equalTo(Json.obj())
    }

    "return JsObject represent 'condition' session" in new WithApplication{
      val f=fixture

      val req=FakeRequest(GET,"url").withSession("condition"->"""{"data":"test"}""")
      val r=f.controller.findCondition(req)

      r must equalTo(Json.obj("data"->"test"))
    }
  }

  "When method verifyDateValid is called, ConditionsManager" should{
    "apply valid function if begin date and end date are valid" in {
      val f=fixture
      val condition=ConditionForm(date,Some(date2),None)
      val error=mock[Form[ConditionForm]=>Future[Result]]
      val valid=mock[(Date,Option[Date])=>Future[Result]]

      valid.apply(org.mockito.Matchers.eq(date),org.mockito.Matchers.eq(Some(date2))) returns future{Results.Ok("execute func valid")}

      val r=f.controller.verifyDateValid(condition)(error)(valid)
      status(r) must equalTo(OK)
      contentAsString(r) must equalTo("execute func valid")

      there was one(valid).apply(org.mockito.Matchers.eq(date),org.mockito.Matchers.eq(Some(date2)))
    }

    "apply valid function if begin date is valid and end date is empty" in {
      val f=fixture
      val condition=ConditionForm(date,None,None)
      val error=mock[Form[ConditionForm]=>Future[Result]]
      val valid=mock[(Date,Option[Date])=>Future[Result]]

      valid.apply(org.mockito.Matchers.eq(date),org.mockito.Matchers.eq(None)) returns future{Results.Ok("execute func valid")}

      val r=f.controller.verifyDateValid(condition)(error)(valid)
      status(r) must equalTo(OK)
      contentAsString(r) must equalTo("execute func valid")

      there was one(valid).apply(org.mockito.Matchers.eq(date),org.mockito.Matchers.eq(None))
    }

    "apply error function if end date is before begin date" in {
      val f=fixture
      val condition=ConditionForm(date2,Some(date),None)
      val error=mock[Form[ConditionForm]=>Future[Result]]
      val valid=mock[(Date,Option[Date])=>Future[Result]]

      error.apply(any[Form[ConditionForm]]) returns future{Results.Ok("execute func error")}

      val r=f.controller.verifyDateValid(condition)(error)(valid)
      status(r) must equalTo(OK)
      contentAsString(r) must equalTo("execute func error")

      there was one(error).apply(any[Form[ConditionForm]])
    }
  }

  "When method verifyGeneralData is called, ConditionsManager" should{
    "send Redirect if data are valid" in new WithApplication{
      val f=fixture
      val camp=Campagne(bson,"camp","type",List())
      val condition=ConditionForm(date,None,None)

      val req=FakeRequest(GET,"url").withSession("user" -> """{"login":"test"}""")
      val action=Action.async{implicit request=>f.controller.verifyGeneralData(condition,bson.stringify,camp)}
      val r=call(action,req)

      status(r) must equalTo(SEE_OTHER)
      header("Location",r) must beSome("/campaigns/campaign/"+bson.stringify+"/form/module")
    }

    "send bad request with the form if end date is before begin date" in new WithApplication{
      val f=fixture
      val condition=ConditionForm(date2,Some(date),None)
      val camp=Campagne(bson,"camp","type",List())

      val req=FakeRequest(GET,"url").withSession("user" -> """{"login":"test"}""")
      val action=Action.async{implicit request=>f.controller.verifyGeneralData(condition,bson.stringify,camp)}
      val r=call(action,req)

      status(r) must equalTo(BAD_REQUEST)
      val content=contentAsString(r)
      content must matchRegex("<input type=\"text\" id=\"debut\" name=\"debut\" value=\"23/04/2015 00:00:00\" class=\"form-control\" placeholder=\"dd/mm/YYYY hh:mm:ss\"/>")
      content must contain("<input type=\"text\" id=\"fin\" name=\"fin\" value=\"22/04/2015 00:00:00\" class=\"form-control\" placeholder=\"dd/mm/YYYY hh:mm:ss\"/>")
      content must contain("<textarea id=\"commentaire\" name=\"commentaire\" class=\"form-control\"></textarea>")
      content must contain("<div class=\"alert alert-danger\" role=\"alert\">La date de début doit être inférieur à la date de fin</div>")
    }
  }
}