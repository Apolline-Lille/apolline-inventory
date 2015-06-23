import java.io.File
import java.util.Date

import controllers._
import models._
import org.junit.runner.RunWith
import org.mockito.ArgumentCaptor
import org.specs2.matcher.{MatchResult, Expectable, Matcher}
import org.specs2.mock.Mockito
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner
import play.api.data.Form
import play.api.libs.Files.TemporaryFile
import play.api.libs.json._
import play.api.mvc.{MultipartFormData, Action, Results, Result}
import play.api.test.Helpers._
import play.api.test.{FakeHeaders, FakeRequest, WithApplication}
import reactivemongo.bson.{BSONDocument, BSONObjectID}
import reactivemongo.core.commands.{GetLastError, LastError}
import reactivemongo.extensions.BSONFormats.BSONObjectIDFormat

import scala.concurrent.ExecutionContext
import scala.concurrent._
import scala.concurrent.duration.Duration

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
    val localisationDaoMock=mock[LocalisationDao]
    val campaignDaoMock=mock[CampagneDao]
    val campaignManagerMock=mock[CampagneManagerLike]
    val moduleManagerMock=mock[ModuleManagerLike]
    val appMock=mock[play.api.Application]
    val tempFileBuilderMock=mock[TemporaryFileBuilder]
    val controller=new ConditionsManagerTest{
      override val conditionsDao=conditionDaoMock
      override val moduleDao=moduleDaoMock
      override val localisationDao=localisationDaoMock
      override val campaignDao=campaignDaoMock
      override val campaignManager=campaignManagerMock
      override val moduleManager=moduleManagerMock
      override val app=appMock
      override val tempFileBuilder=tempFileBuilderMock
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

    "redirect to login for resource /campaigns/campaign/:id/form/localisation" in new WithApplication {
      val f=fixture
      val formData=MultipartFormData[TemporaryFile](dataParts = Map(), files = Seq(), badParts = Seq(), missingFileParts = Seq())
      f.controller.addLocalisation(bson.stringify).apply(FakeRequest(POST, "/campaigns/campaign/"+bson.stringify+"/form/localisation").withMultipartFormDataBody(formData)).map(
        data => {
          val r=future{data}
          status(r) must equalTo(SEE_OTHER)
          header("Location", r) must equalTo(Some("/login"))
        }
      )
    }

    "redirect to login for resource /campaigns/campaign/:id/form/validate" in new WithApplication {
      route(FakeRequest(GET, "/campaigns/campaign/"+bson.stringify+"/form/validate")).map(
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
    "send 200 Ok page with the message 'Aucun résultat trouvé'" in new WithApplication {
      val f=fixture
      val camp=Campagne(bson,"camp","type",List())

      f.campaignManagerMock.doIfCampaignFound(org.mockito.Matchers.eq(bson))(any[Campagne=>Future[Result]])(any[Unit=>Future[Result]]) answers {(params,_) => params match{
        case Array(_,p:(Campagne=>Future[Result]),_) => p.apply(camp)
      }}

      f.conditionDaoMock.findAll(org.mockito.Matchers.eq(Json.obj("_id"->Json.obj("$in"->JsArray()))),any[JsObject])(any[ExecutionContext]) returns future{List()}
      f.moduleDaoMock.findAll(org.mockito.Matchers.eq(Json.obj("_id"->Json.obj("$in"->JsArray()))),any[JsObject])(any[ExecutionContext]) returns future{List()}

      val r=f.controller.listConditions(bson.stringify).apply(FakeRequest(GET,"/campaigns/campaign/"+bson.stringify).withSession("user" -> """{"login":"test"}"""))

      status(r) must equalTo(OK)
      val content=contentAsString(r)
      content must contain("<h4>camp</h4>")
      content must contain("<div class=\"row\"><span class=\"bold\">Type</span> : type</div>")
      content must contain("<div class=\"row\"><span class=\"bold\">Installations</span> : 0</div>")
      content must contain("<h3 style=\"text-align:center\">Aucun résultat trouvé</h3>")

      there was one(f.campaignManagerMock).doIfCampaignFound(org.mockito.Matchers.eq(bson))(any[Campagne=>Future[Result]])(any[Unit=>Future[Result]])
      there was one(f.conditionDaoMock).findAll(org.mockito.Matchers.eq(Json.obj("_id"->Json.obj("$in"->JsArray()))),any[JsObject])(any[ExecutionContext])
      there was one(f.moduleDaoMock).findAll(org.mockito.Matchers.eq(Json.obj("_id"->Json.obj("$in"->JsArray()))),any[JsObject])(any[ExecutionContext])
      there was no(f.localisationDaoMock).findAll(org.mockito.Matchers.eq(Json.obj("condition"->Json.obj("$in"->JsArray()))),any[JsObject])(any[ExecutionContext])
    }

    "send 200 Ok page with 1 result" in new WithApplication {
      val f=fixture
      val idCond=List(Json.toJson(bson2))
      val idMod=List(Json.toJson(bson3))
      val camp=Campagne(bson,"camp","Terrain",List(bson2))
      val cond=Condition(bson2,date,Some(date2),Some("unCom"),bson3)
      val module=Module(bson3,"idMod","typeMod",date,List(),List(),None)
      val localisation=Localisation(bson4,bson2,"loc",Some(1.2f),Some(3.4f),None,List())

      f.campaignManagerMock.doIfCampaignFound(org.mockito.Matchers.eq(bson))(any[Campagne=>Future[Result]])(any[Unit=>Future[Result]]) answers {(params,_) => params match{
        case Array(_,p:(Campagne=>Future[Result]),_) => p.apply(camp)
      }}

      f.conditionDaoMock.findAll(any[JsObject],any[JsObject])(any[ExecutionContext]) returns future{List(cond)}
      f.moduleDaoMock.findAll(org.mockito.Matchers.eq(Json.obj("_id"->Json.obj("$in"->JsArray(idMod)))),any[JsObject])(any[ExecutionContext]) returns future{List(module)}
      f.localisationDaoMock.findAll(org.mockito.Matchers.eq(Json.obj("condition"->Json.obj("$in"->JsArray(idCond)))),any[JsObject])(any[ExecutionContext]) returns future{List(localisation)}

      val r=f.controller.listConditions(bson.stringify,"ongoing").apply(FakeRequest(GET,"/campaigns/campaign/"+bson.stringify).withSession("user" -> """{"login":"test"}"""))

      status(r) must equalTo(OK)
      val content=contentAsString(r)
      content must contain("<h4>camp</h4>")
      content must contain("<div class=\"row\"><span class=\"bold\">Type</span> : Terrain</div>")
      content must contain("<div class=\"row\"><span class=\"bold\">Installations</span> : 1</div>")
      content must contain("<td>22/04/2015 00:00:00</td>")
      content must contain("<td>23/04/2015 00:00:00</td>")
      content must contain("<td>typeMod / idMod</td>")
      content must contain("<td>loc (1.2 - 3.4)</td>")

      there was one(f.campaignManagerMock).doIfCampaignFound(org.mockito.Matchers.eq(bson))(any[Campagne=>Future[Result]])(any[Unit=>Future[Result]])
      there was one(f.conditionDaoMock).findAll(any[JsObject],any[JsObject])(any[ExecutionContext])
      there was one(f.moduleDaoMock).findAll(org.mockito.Matchers.eq(Json.obj("_id"->Json.obj("$in"->JsArray(idMod)))),any[JsObject])(any[ExecutionContext])
      there was one(f.localisationDaoMock).findAll(org.mockito.Matchers.eq(Json.obj("condition"->Json.obj("$in"->JsArray(idCond)))),any[JsObject])(any[ExecutionContext])
    }

    "send 200 Ok page with 2 result" in new WithApplication {
      val f=fixture
      val idCond=List(Json.toJson(bson2),Json.toJson(bson3))
      val camp=Campagne(bson,"camp","Test",List(bson2,bson3))
      val cond=List(Condition(bson2,date,Some(date2),Some("unCom"),bson3),Condition(bson3,date,Some(date2),Some("unCom"),bson4))
      val module=List(Module(bson3,"idMod","typeMod",date,List(),List(),None),Module(bson4,"idMod2","typeMod2",date,List(),List(),None))

      f.campaignManagerMock.doIfCampaignFound(org.mockito.Matchers.eq(bson))(any[Campagne=>Future[Result]])(any[Unit=>Future[Result]]) answers {(params,_) => params match{
        case Array(_,p:(Campagne=>Future[Result]),_) => p.apply(camp)
      }}

      f.conditionDaoMock.findAll(any[JsObject],any[JsObject])(any[ExecutionContext]) returns future{cond}
      f.moduleDaoMock.findAll(any[JsObject],any[JsObject])(any[ExecutionContext]) returns future{module}

      val r=f.controller.listConditions(bson.stringify,"finish").apply(FakeRequest(GET,"/campaigns/campaign/"+bson.stringify).withSession("user" -> """{"login":"test"}"""))

      status(r) must equalTo(OK)
      val content=contentAsString(r)
      content must contain("<h4>camp</h4>")
      content must contain("<div class=\"row\"><span class=\"bold\">Type</span> : Test</div>")
      content must contain("<div class=\"row\"><span class=\"bold\">Installations</span> : 2</div>")
      content must contains("<td>22/04/2015 00:00:00</td>",2)
      content must contains("<td>23/04/2015 00:00:00</td>",2)
      content must contain("<td>typeMod / idMod</td>")
      content must contain("<td>typeMod2 / idMod2</td>")

      there was one(f.campaignManagerMock).doIfCampaignFound(org.mockito.Matchers.eq(bson))(any[Campagne=>Future[Result]])(any[Unit=>Future[Result]])
      there was one(f.conditionDaoMock).findAll(any[JsObject],any[JsObject])(any[ExecutionContext])
      there was one(f.moduleDaoMock).findAll(any[JsObject],any[JsObject])(any[ExecutionContext])
      there was no(f.localisationDaoMock).findAll(org.mockito.Matchers.eq(Json.obj("condition"->Json.obj("$in"->JsArray(idCond)))),any[JsObject])(any[ExecutionContext])
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

    "send 200 Ok page with an empty form" in new WithApplication{
      val f=fixture
      val camp=Campagne(nom="camp",types="Terrain",conditions=List())

      f.campaignManagerMock.doIfCampaignFound(org.mockito.Matchers.eq(bson))(any[Campagne=>Future[Result]])(any[Unit=>Future[Result]]) answers {(params,_) => params match{
        case Array(_,p:(Campagne=>Future[Result]),_) => p.apply(camp)
      }}

      val r=f.controller.formLocalisation(bson.stringify).apply(FakeRequest(GET,"/campaigns/campaign/"+bson.stringify+"/form/localisation").withSession("user" -> """{"login":"test"}"""))

      status(r) must equalTo(OK)
      val content=contentAsString(r)
      content must contain("<input type=\"text\" id=\"nom\" name=\"nom\" value=\"\" class=\"form-control\"/>")
      content must contain("<input type=\"text\" id=\"lat\" name=\"lat\" value=\"\" class=\"form-control\"/>")
      content must contain("<input type=\"text\" id=\"lon\" name=\"lon\" value=\"\" class=\"form-control\"/>")
      content must contain("<input name=\"photo[]\" id=\"photo\" type=\"file\" accept=\"image/*\"/>")
      content must contain("<textarea id=\"commentaire\" name=\"commentaire\" class=\"form-control\"></textarea>")

      there was one(f.campaignManagerMock).doIfCampaignFound(org.mockito.Matchers.eq(bson))(any[Campagne=>Future[Result]])(any[Unit=>Future[Result]])
    }

    "send redirect if campaign not found" in new WithApplication{
      val f=fixture
      val formData=MultipartFormData[TemporaryFile](dataParts = Map(), files = Seq(), badParts = Seq(), missingFileParts = Seq())

      f.campaignManagerMock.doIfCampaignFound(org.mockito.Matchers.eq(bson))(any[Campagne=>Future[Result]])(any[Unit=>Future[Result]]) answers {(params,_) => params match{
        case Array(_,_,p:(Unit=>Future[Result])) => p.apply()
      }}

      val req=FakeRequest(POST,"/campaigns/campaign/"+bson.stringify+"/form/localisation",FakeHeaders(),formData).withSession("user" -> """{"login":"test"}""")
      val r=f.controller.addLocalisation(bson.stringify).apply(req)

      status(r) must equalTo(SEE_OTHER)
      header("Location",r) must beSome("/campaigns")

      there was one(f.campaignManagerMock).doIfCampaignFound(org.mockito.Matchers.eq(bson))(any[Campagne=>Future[Result]])(any[Unit=>Future[Result]])
    }

    "send redirect if campaign type is not equal to 'Terrain'" in new WithApplication{
      val f=fixture
      val camp=Campagne(nom="camp",types="Test",conditions=List())
      val formData=MultipartFormData[TemporaryFile](dataParts = Map(), files = Seq(), badParts = Seq(), missingFileParts = Seq())

      f.campaignManagerMock.doIfCampaignFound(org.mockito.Matchers.eq(bson))(any[Campagne=>Future[Result]])(any[Unit=>Future[Result]]) answers {(params,_) => params match{
        case Array(_,p:(Campagne=>Future[Result]),_) => p.apply(camp)
      }}

      val req=FakeRequest(POST,"/campaigns/campaign/"+bson.stringify+"/form/localisation",FakeHeaders(),formData).withSession("user" -> """{"login":"test"}""")
      val r=f.controller.addLocalisation(bson.stringify).apply(req)

      status(r) must equalTo(SEE_OTHER)
      header("Location",r) must beSome("/campaigns/campaign/"+bson.stringify)

      there was one(f.campaignManagerMock).doIfCampaignFound(org.mockito.Matchers.eq(bson))(any[Campagne=>Future[Result]])(any[Unit=>Future[Result]])
    }

    "send bad request if form is submit with empty field" in new WithApplication{
      val f=fixture
      val formData=MultipartFormData[TemporaryFile](dataParts = Map(), files = Seq(), badParts = Seq(), missingFileParts = Seq())
      val camp=Campagne(nom="camp",types="Terrain",conditions=List())

      f.campaignManagerMock.doIfCampaignFound(org.mockito.Matchers.eq(bson))(any[Campagne=>Future[Result]])(any[Unit=>Future[Result]]) answers {(params,_) => params match{
        case Array(_,p:(Campagne=>Future[Result]),_) => p.apply(camp)
      }}

      val req=FakeRequest(POST,"/campaigns/campaign/"+bson.stringify+"/form/localisation",FakeHeaders(),formData).withSession("user" -> """{"login":"test"}""")
      val r=f.controller.addLocalisation(bson.stringify).apply(req)

      status(r) must equalTo(BAD_REQUEST)
      contentAsString(r) must contain("<span class=\"control-label errors\">This field is required</span>")

      there was one(f.campaignManagerMock).doIfCampaignFound(org.mockito.Matchers.eq(bson))(any[Campagne=>Future[Result]])(any[Unit=>Future[Result]])
    }

    "send redirect to localisation list after insert data received" in new WithApplication{
      val f=fixture
      val file=mock[MultipartFormData.FilePart[TemporaryFile]]
      val tempFile=mock[TemporaryFile]
      val captor = ArgumentCaptor.forClass(classOf[File])
      val param=Map("nom"->Seq("unNom"),"lat"->Seq("2.3"),"lon"->Seq("3.5"),"commentaire"->Seq("un com"))
      val formData=MultipartFormData[TemporaryFile](dataParts = param, files = Seq(file), badParts = Seq(), missingFileParts = Seq())
      val camp=Campagne(nom="camp",types="Terrain",conditions=List())

      file.filename returns "file.jpg"
      f.appMock.path returns (new File("/route"))
      file.ref returns tempFile
      org.mockito.Mockito.doNothing.when(tempFile).moveTo(any[File],any[Boolean])
      f.campaignManagerMock.doIfCampaignFound(org.mockito.Matchers.eq(bson))(any[Campagne=>Future[Result]])(any[Unit=>Future[Result]]) answers {(params,_) => params match{
        case Array(_,p:(Campagne=>Future[Result]),_) => p.apply(camp)
      }}

      val req=FakeRequest(POST,"/campaigns/campaign/"+bson.stringify+"/form/localisation",FakeHeaders(),formData).withSession("user" -> """{"login":"test"}""")
      val r=f.controller.addLocalisation(bson.stringify).apply(req)

      Await.result(r,Duration.Inf)

      there was one(tempFile).moveTo(captor.capture,any[Boolean])
      val arg=captor.getValue
      arg.getAbsolutePath must matchRegex("/route/public/images/campaign/tmp/[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}\\.jpg")

      status(r) must equalTo(SEE_OTHER)
      header("Location",r) must beSome("/campaigns/campaign/"+bson.stringify+"/form/validate")

      there was one(file).filename
      there was one(f.appMock).path
      there was 2.times(file).ref
      there was one(f.campaignManagerMock).doIfCampaignFound(org.mockito.Matchers.eq(bson))(any[Campagne=>Future[Result]])(any[Unit=>Future[Result]])
    }
  }

  "When user is on resource /campaigns/campaign/:id/form/validate, ConditionsManager" should{
    "send redirect if campaign not found" in new WithApplication{
      val f=fixture
      val formData=MultipartFormData[TemporaryFile](dataParts = Map(), files = Seq(), badParts = Seq(), missingFileParts = Seq())

      f.campaignManagerMock.doIfCampaignFound(org.mockito.Matchers.eq(bson))(any[Campagne=>Future[Result]])(any[Unit=>Future[Result]]) answers {(params,_) => params match{
        case Array(_,_,p:(Unit=>Future[Result])) => p.apply()
      }}

      val req=FakeRequest(GET,"/campaigns/campaign/"+bson.stringify+"/form/validate").withSession("user" -> """{"login":"test"}""")
      val r=f.controller.formValidate(bson.stringify).apply(req)

      status(r) must equalTo(SEE_OTHER)
      header("Location",r) must beSome("/campaigns")

      there was one(f.campaignManagerMock).doIfCampaignFound(org.mockito.Matchers.eq(bson))(any[Campagne=>Future[Result]])(any[Unit=>Future[Result]])
    }

    "send 200 Ok page with all condition informations" in new WithApplication{
      val f = fixture
      val locObj=Localisation(bson2,bson3,"loc",Some(1.2f),Some(3.4f),Some("un com"),List("img.jpg"))
      val session=Json.obj("debut"->date,"fin"->date2,"commentaire"->"unCom","module"->bson4.stringify,"localisation"->Json.toJson(locObj))
      val module=Module(bson4,"idMod","typeMod",date,List(),List(),Some("moduleCom"))
      val camp=Campagne(nom="camp",types="Terrain",conditions=List())
      val queryBegin=Json.obj("dateDebut"->Json.obj("$gte"->date,"$lte"->date2))
      val queryEnd=Json.obj("$or"->JsArray(Seq(Json.obj("dateFin"->Json.obj("$gte"->date,"$lte"->date2),"dateFin"->Json.obj("$exists"->false)))))
      val query=Json.obj("modules"->bson4,"$or"->JsArray(Seq(queryBegin,queryEnd)))

      f.campaignManagerMock.doIfCampaignFound(org.mockito.Matchers.eq(bson))(any[Campagne=>Future[Result]])(any[Unit=>Future[Result]]) answers {(params,_) => params match{
        case Array(_,p:(Campagne=>Future[Result]),_) => p.apply(camp)
      }}

      f.moduleDaoMock.findOne(org.mockito.Matchers.eq(Json.obj("delete"->false,"_id"->bson4)))(any[ExecutionContext]) returns future{Some(module)}

      f.conditionDaoMock.findOne(org.mockito.Matchers.eq(query))(any[ExecutionContext]) returns future{None}

      val req=FakeRequest(GET,"/campaigns/campaign/"+bson.stringify+"/form/validate").withSession("user" -> """{"login":"test"}""").withSession("condition"->Json.stringify(session))
      val r=f.controller.formValidate(bson.stringify)(req)

      status(r) must equalTo(OK)
      val content=contentAsString(r)
      content must contain("<span class=\"bold\">Date de début</span> : 22/04/2015 00:00:00")
      content must contain("<span class=\"bold\">Date de fin</span> : 23/04/2015 00:00:00")
      content must contain("<span class=\"bold\">Commentaire</span> : unCom")
      content must contain("typeMod")
      content must contain("idMod")
      content must contain("<span class=\"bold\">Commentaires</span> : moduleCom")
      content must contain("<span class=\"bold\">Date d&#x27;assemblage</span> : 22/04/2015")
      content must contain("<span class=\"bold\">Cartes</span> : 0")
      content must contain("<span class=\"bold\">Capteurs</span> : 0")
      content must contain("<span class=\"bold\">Localisation</span> : loc")
      content must contain("<span class=\"bold\">Latitude</span> : 1.2")
      content must contain("<span class=\"bold\">Longitude</span> : 3.4")
      content must contain("<span class=\"bold\">Commentaire</span> : un com")
      content must contain("<img src=\"/assets/images/campaign/tmp/img.jpg\" class=\"img-responsive\">")

      there was one(f.moduleDaoMock).findOne(org.mockito.Matchers.eq(Json.obj("delete"->false,"_id"->bson4)))(any[ExecutionContext])
      there was one(f.campaignManagerMock).doIfCampaignFound(org.mockito.Matchers.eq(bson))(any[Campagne=>Future[Result]])(any[Unit=>Future[Result]])
      there was one(f.conditionDaoMock).findOne(org.mockito.Matchers.eq(query))(any[ExecutionContext])
    }

    "send Bad request with error message if condition have an error" in new WithApplication{
      val f = fixture
      val camp=Campagne(nom="camp",types="Terrain",conditions=List())

      f.campaignManagerMock.doIfCampaignFound(org.mockito.Matchers.eq(bson))(any[Campagne=>Future[Result]])(any[Unit=>Future[Result]]) answers {(params,_) => params match{
        case Array(_,p:(Campagne=>Future[Result]),_) => p.apply(camp)
      }}

      f.moduleDaoMock.findOne(any[JsObject])(any[ExecutionContext]) returns future{None}

      val req=FakeRequest(GET,"/campaigns/campaign/"+bson.stringify+"/form/validate").withSession("user" -> """{"login":"test"}""")
      val r=f.controller.formValidate(bson.stringify)(req)

      status(r) must equalTo(BAD_REQUEST)
      contentAsString(r) must contain("<div class=\"alert alert-danger\" role=\"alert\">Aucun module n'a été sélectionné</div>")

      there was one(f.moduleDaoMock).findOne(any[JsObject])(any[ExecutionContext])
      there was one(f.campaignManagerMock).doIfCampaignFound(org.mockito.Matchers.eq(bson))(any[Campagne=>Future[Result]])(any[Unit=>Future[Result]])
    }

    "send redirect if campaign not found" in new WithApplication{
      val f=fixture
      val formData=MultipartFormData[TemporaryFile](dataParts = Map(), files = Seq(), badParts = Seq(), missingFileParts = Seq())

      f.campaignManagerMock.doIfCampaignFound(org.mockito.Matchers.eq(bson))(any[Campagne=>Future[Result]])(any[Unit=>Future[Result]]) answers {(params,_) => params match{
        case Array(_,_,p:(Unit=>Future[Result])) => p.apply()
      }}

      val req=FakeRequest(POST,"/campaigns/campaign/"+bson.stringify+"/form/validate").withSession("user" -> """{"login":"test"}""")
      val r=f.controller.validate(bson.stringify).apply(req)

      status(r) must equalTo(SEE_OTHER)
      header("Location",r) must beSome("/campaigns")

      there was one(f.campaignManagerMock).doIfCampaignFound(org.mockito.Matchers.eq(bson))(any[Campagne=>Future[Result]])(any[Unit=>Future[Result]])
    }

    "send redirect after insert condition and localisation into the database" in new WithApplication{
      val f = fixture
      val locObj=Localisation(bson2,bson3,"loc",Some(1.2f),Some(3.4f),Some("un com"),List("img.jpg"))
      val session=Json.obj("debut"->date,"fin"->date2,"commentaire"->"unCom","module"->bson4.stringify,"localisation"->Json.toJson(locObj))
      val module=Module(bson4,"idMod","typeMod",date,List(),List(),Some("moduleCom"))
      val camp=Campagne(nom="camp",types="Terrain",conditions=List())
      val queryBegin=Json.obj("dateDebut"->Json.obj("$gte"->date,"$lte"->date2))
      val queryEnd=Json.obj("$or"->JsArray(Seq(Json.obj("dateFin"->Json.obj("$gte"->date,"$lte"->date2),"dateFin"->Json.obj("$exists"->false)))))
      val query=Json.obj("modules"->bson4,"$or"->JsArray(Seq(queryBegin,queryEnd)))
      val tempFile=mock[TemporaryFile]
      val f1=new File("/route/public/images/campaign/img.jpg")
      val lastError=mock[LastError]

      f.appMock.path returns (new File("/route"))
      f.tempFileBuilderMock.createTemporaryFile(org.mockito.Matchers.eq(new File("/route/public/images/campaign/tmp/img.jpg"))) returns tempFile
      org.mockito.Mockito.doNothing.when(tempFile).moveTo(org.mockito.Matchers.eq(f1),any[Boolean])
      tempFile.clean() returns true
      f.localisationDaoMock.insert(any[Localisation],any[GetLastError])(any[ExecutionContext]) returns future{lastError}
      f.conditionDaoMock.insert(any[Condition],any[GetLastError])(any[ExecutionContext]) returns future{lastError}
      f.campaignDaoMock.updateById(org.mockito.Matchers.eq(camp._id),any[Campagne],any[GetLastError])(any[Writes[Campagne]],any[ExecutionContext]) returns future{lastError}
      f.campaignManagerMock.doIfCampaignFound(org.mockito.Matchers.eq(bson))(any[Campagne=>Future[Result]])(any[Unit=>Future[Result]]) answers {(params,_) => params match{
        case Array(_,p:(Campagne=>Future[Result]),_) => p.apply(camp)
      }}

      f.moduleDaoMock.findOne(org.mockito.Matchers.eq(Json.obj("delete"->false,"_id"->bson4)))(any[ExecutionContext]) returns future{Some(module)}

      f.conditionDaoMock.findOne(org.mockito.Matchers.eq(query))(any[ExecutionContext]) returns future{None}

      val req=FakeRequest(POST,"/campaigns/campaign/"+bson.stringify+"/form/validate").withSession("user" -> """{"login":"test"}""").withSession("condition"->Json.stringify(session))
      val r=f.controller.validate(bson.stringify)(req)

      status(r) must equalTo(SEE_OTHER)
      header("Location",r) must beSome("/campaigns/campaign/"+bson.stringify)

      there was one(f.moduleDaoMock).findOne(org.mockito.Matchers.eq(Json.obj("delete"->false,"_id"->bson4)))(any[ExecutionContext])
      there was one(f.campaignManagerMock).doIfCampaignFound(org.mockito.Matchers.eq(bson))(any[Campagne=>Future[Result]])(any[Unit=>Future[Result]])
      there was one(f.conditionDaoMock).findOne(org.mockito.Matchers.eq(query))(any[ExecutionContext])
      there was 2.times(f.appMock).path
      there was one(f.tempFileBuilderMock).createTemporaryFile(any[File])
      there was one(tempFile).moveTo(org.mockito.Matchers.eq(f1),any[Boolean])
      there was one(tempFile).clean()
      there was one(f.localisationDaoMock).insert(any[Localisation],any[GetLastError])(any[ExecutionContext])
      there was one(f.conditionDaoMock).insert(any[Condition],any[GetLastError])(any[ExecutionContext])
    }

    "send Bad request with error message if condition have an error" in new WithApplication{
      val f = fixture
      val camp=Campagne(nom="camp",types="Terrain",conditions=List())

      f.campaignManagerMock.doIfCampaignFound(org.mockito.Matchers.eq(bson))(any[Campagne=>Future[Result]])(any[Unit=>Future[Result]]) answers {(params,_) => params match{
        case Array(_,p:(Campagne=>Future[Result]),_) => p.apply(camp)
      }}

      f.moduleDaoMock.findOne(any[JsObject])(any[ExecutionContext]) returns future{None}

      val req=FakeRequest(POST,"/campaigns/campaign/"+bson.stringify+"/form/validate").withSession("user" -> """{"login":"test"}""")
      val r=f.controller.validate(bson.stringify)(req)

      status(r) must equalTo(BAD_REQUEST)
      contentAsString(r) must contain("<div class=\"alert alert-danger\" role=\"alert\">Aucun module n'a été sélectionné</div>")

      there was one(f.moduleDaoMock).findOne(any[JsObject])(any[ExecutionContext])
      there was one(f.campaignManagerMock).doIfCampaignFound(org.mockito.Matchers.eq(bson))(any[Campagne=>Future[Result]])(any[Unit=>Future[Result]])
    }
  }

  "When method insertImage is called, ConditionsManager" should{
    "return an empty list if not have file" in new WithApplication{
      val f=fixture

      val r=f.controller.insertImage(List())

      r must equalTo(List())
    }

    "return list with new name file" in new WithApplication{
      val f=fixture
      val file=mock[MultipartFormData.FilePart[TemporaryFile]]
      val tempFile=mock[TemporaryFile]
      val captor = ArgumentCaptor.forClass(classOf[File])

      file.filename returns "file.jpg"
      f.appMock.path returns (new File("/route"))
      file.ref returns tempFile
      org.mockito.Mockito.doNothing.when(tempFile).moveTo(any[File],any[Boolean])

      val r=f.controller.insertImage(List(file))

      there was one(tempFile).moveTo(captor.capture,any[Boolean])
      val arg=captor.getValue

      arg.getAbsolutePath must matchRegex("/route/public/images/campaign/tmp/[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}\\.jpg")
      r must equalTo(List(arg.getAbsolutePath.split("/").last))

      there was one(file).filename
      there was one(f.appMock).path
      there was 2.times(file).ref
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
      content must not contain("Localisation")
    }

    "display 3 links with 'general information', 'modules', 'parameter' and 'validate' for Calibrations conditions" in new WithApplication{
      val f=fixture

      val r=f.controller.printStateForm("infoConditions","Calibration",bson.stringify)

      val content=contentAsString(r)
      content must contain("Informations générales")
      content must contain("Module")
      content must contain("Validation")
      content must not contain("Localisation")
    }

    "display 3 links with 'general information', 'modules', 'localisation' and 'validate' for Ground conditions" in new WithApplication{
      val f=fixture

      val r=f.controller.printStateForm("infoConditions","Terrain",bson.stringify)

      val content=contentAsString(r)
      content must contain("Informations générales")
      content must contain("Module")
      content must contain("Validation")
      content must contain("Localisation")
    }
  }

  "When method verifyDate is called, ConditionsManager" should{
    "return true if end date is empty" in new WithApplication{
      val f=fixture

      f.controller.verifyDate(date,None) must beTrue
    }

    "return true if end date is after begin date" in new WithApplication{
      val f=fixture

      f.controller.verifyDate(date,Some(date2)) must beTrue
    }

    "return false if end date is before begin date" in new WithApplication{
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
    "apply valid function if begin date and end date are valid" in new WithApplication{
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

    "apply valid function if begin date is valid and end date is empty" in new WithApplication{
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

    "apply error function if end date is before begin date" in new WithApplication{
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

  "When method findDateDebut is called, ConditionsManager" should{
    "return the current date if key 'debut' not found" in new WithApplication{
      val f=fixture
      val before=new Date

      val r=f.controller.findDateDebut(Json.obj())

      val after=new Date

      r.compareTo(before) must greaterThanOrEqualTo(0)
      r.compareTo(after) must lessThanOrEqualTo(0)
    }

    "return the date define with the key 'debut'" in new WithApplication{
      val f=fixture
      val d=new Date

      val r=f.controller.findDateDebut(Json.obj("debut"->d.getTime))

      r must equalTo(d)
    }
  }

  "When method findDateFin is called, ConditionsManager" should{
    "return None if key 'fin' not found" in new WithApplication{
      val f=fixture

      val r=f.controller.findDateFin(Json.obj())

      r must beNone
    }

    "return the date define with the key 'fin'" in new WithApplication{
      val f=fixture
      val d=new Date

      val r=f.controller.findDateFin(Json.obj("fin"->d.getTime))

      r must beSome(d)
    }

    "return None if value with key 'fin' is null" in new WithApplication() {
      val f=fixture
      val d=new Date

      val r=f.controller.findDateFin(Json.obj("fin"->JsNull))

      r must beNone
    }

    "return None if value with key 'fin' is None" in new WithApplication() {
      val f=fixture
      val d=new Date

      val r=f.controller.findDateFin(Json.obj("fin"->None))

      r must beNone
    }
  }

  "When method findModule is called, ConditionsManager" should{
    "return an generated BSONObjectID if key 'module' not found" in new WithApplication{
      val f=fixture

      val r=f.controller.findModule(Json.obj())

      r.getClass.getSimpleName must equalTo("BSONObjectID")
    }

    "return the BSONObjectID define with the key 'module'" in new WithApplication{
      val f=fixture

      val r=f.controller.findModule(Json.obj("module"->bson.stringify))

      r must equalTo(bson)
    }

    "return generate BSONObjectID if value with key 'module' is null" in new WithApplication() {
      val f=fixture

      val r=f.controller.findModule(Json.obj("module"->JsNull))

      r.getClass.getSimpleName must equalTo("BSONObjectID")
    }
  }

  "When method toLocalisation is called, ConditionsManager" should{
    "return None if key 'localisation' not found" in new WithApplication{
      val f=fixture

      val r=f.controller.toLocalisation(Json.obj())

      r must beNone
    }

    "return the Localisation define with the key 'module'" in new WithApplication{
      val f=fixture
      val loc=Localisation(bson,bson2,"loc",Some(1.2f),Some(3.4f),Some("un com"),List("img.jpg"))

      val r=f.controller.toLocalisation(Json.obj("localisation"->loc))

      r must beSome(loc)
    }

    "return None if value with key 'localisation' is null" in new WithApplication() {
      val f=fixture

      val r=f.controller.toLocalisation(Json.obj("localisation"->JsNull))

      r must beNone
    }
  }

  "When method toCondition is called, ConditionsManager" should{
    "return condition with data in the JsObject" in new WithApplication {
      val f = fixture

      val r = f.controller.toCondition(Json.obj("debut" -> date, "fin" -> date2, "commentaire" -> "unCom","module"->bson.stringify))

      r.dateDebut must equalTo(date)
      r.dateFin must beSome(date2)
      r.commentaire must beSome("unCom")
      r.modules must equalTo(bson)
    }

    "return condition with generate data if data not found in the JsObject" in new WithApplication{
      val f = fixture
      val before=new Date

      val r = f.controller.toCondition(Json.obj())

      val after=new Date

      r.dateDebut.compareTo(before) must greaterThanOrEqualTo(0)
      r.dateDebut.compareTo(after) must lessThanOrEqualTo(0)
      r.dateFin must beNone
      r.commentaire must beNone
      r.modules.getClass.getSimpleName must equalTo("BSONObjectID")
    }
  }

  "When method findSessionData is called, ConditionsManager" should{
    "return condition with generate data and None for the localisation if key 'condition' not found" in new WithApplication {
      val f = fixture
      val before=new Date

      val req = FakeRequest(GET, "url")
      val r=f.controller.findSessionData(req)
      val cond=r._1
      val loc=r._2

      val after=new Date

      cond.dateDebut.compareTo(before) must greaterThanOrEqualTo(0)
      cond.dateDebut.compareTo(after) must lessThanOrEqualTo(0)
      cond.dateFin must beNone
      cond.commentaire must beNone
      cond.modules.getClass.getSimpleName must equalTo("BSONObjectID")
      loc must beNone
    }

    "return condition with data in session and None for localisation if localisation data not found" in new WithApplication{
      val f = fixture
      val session=Json.obj("debut"->date,"fin"->date2,"commentaire"->"unCom","module"->bson.stringify)

      val req = FakeRequest(GET, "url").withSession("condition"->Json.stringify(session))
      val r=f.controller.findSessionData(req)
      val cond=r._1
      val loc=r._2

      cond.dateDebut must equalTo(date)
      cond.dateFin must beSome(date2)
      cond.commentaire must beSome("unCom")
      cond.modules must equalTo(bson)
      loc must beNone
    }

    "return condition with generate data if condition data not found and localisation with data in session" in new WithApplication{
      val f=fixture
      val locObj=Localisation(bson,bson2,"loc",Some(1.2f),Some(3.4f),Some("un com"),List("img.jpg"))
      val session=Json.obj("localisation"->Json.toJson(locObj))
      val before=new Date

      val req = FakeRequest(GET, "url").withSession("condition"->Json.stringify(session))
      val r=f.controller.findSessionData(req)
      val cond=r._1
      val loc=r._2

      val after=new Date

      cond.dateDebut.compareTo(before) must greaterThanOrEqualTo(0)
      cond.dateDebut.compareTo(after) must lessThanOrEqualTo(0)
      cond.dateFin must beNone
      cond.commentaire must beNone
      cond.modules.getClass.getSimpleName must equalTo("BSONObjectID")
      loc must beSome(locObj)
    }

    "return condition and localisation with data in the session" in new WithApplication{
      val f = fixture
      val locObj=Localisation(bson,bson2,"loc",Some(1.2f),Some(3.4f),Some("un com"),List("img.jpg"))
      val session=Json.obj("debut"->date,"fin"->date2,"commentaire"->"unCom","module"->bson.stringify,"localisation"->Json.toJson(locObj))

      val req = FakeRequest(GET, "url").withSession("condition"->Json.stringify(session))
      val r=f.controller.findSessionData(req)
      val cond=r._1
      val loc=r._2

      cond.dateDebut must equalTo(date)
      cond.dateFin must beSome(date2)
      cond.commentaire must beSome("unCom")
      cond.modules must equalTo(bson)
      loc must beSome(locObj)
    }
  }

  "When method verifyLocalisation is called, ConditionsManager" should{
    "return false if Localisation not found" in new WithApplication{
      val f=fixture

      f.controller.verifyLocalisation(None) must beFalse
    }

    "return false if Localisation name is empty" in new WithApplication{
      val f=fixture
      val loc=Localisation(nom="",lat=None,lon=None,commentaire=None,condition=bson,photo=List())

      f.controller.verifyLocalisation(Some(loc)) must beFalse
    }

    "return true if Localisation name is not empty" in new WithApplication{
      val f=fixture
      val loc=Localisation(nom="unNom",lat=None,lon=None,commentaire=None,condition=bson,photo=List())

      f.controller.verifyLocalisation(Some(loc)) must beTrue
    }
  }

  "When method verifyAllData is called, ConditionsManager" should{
    "execute notCorrect function if module not found" in new WithApplication {
      val f = fixture
      val correct = mock[(Condition, Option[Localisation], Module) => Future[Result]]
      val notCorrect = mock[(String, Condition, Option[Localisation], Option[Module]) => Future[Result]]
      val locObj=Localisation(bson,bson2,"loc",Some(1.2f),Some(3.4f),Some("un com"),List("img.jpg"))
      val session=Json.obj("debut"->date,"fin"->date2,"commentaire"->"unCom","module"->bson.stringify,"localisation"->Json.toJson(locObj))

      f.moduleDaoMock.findOne(org.mockito.Matchers.eq(Json.obj("delete"->false,"_id"->bson)))(any[ExecutionContext]) returns future{None}
      notCorrect.apply(org.mockito.Matchers.eq("Aucun module n'a été sélectionné"),any[Condition],any[Option[Localisation]],org.mockito.Matchers.eq(None)) returns future{Results.Ok("notCorrect func")}

      val req=FakeRequest(GET,"url").withSession("condition"->Json.stringify(session))
      val r=f.controller.verifyAllData("Test")(correct)(notCorrect)(req)

      status(r) must equalTo(OK)
      contentAsString(r) must equalTo("notCorrect func")

      there was one(f.moduleDaoMock).findOne(org.mockito.Matchers.eq(Json.obj("delete"->false,"_id"->bson)))(any[ExecutionContext])
      there was one(notCorrect).apply(org.mockito.Matchers.eq("Aucun module n'a été sélectionné"),any[Condition],any[Option[Localisation]],org.mockito.Matchers.eq(None))
      there was no(correct).apply(any[Condition],any[Option[Localisation]],any[Module])
    }

    "execute notCorrect function if date are not valid" in new WithApplication {
      val f = fixture
      val correct = mock[(Condition, Option[Localisation], Module) => Future[Result]]
      val notCorrect = mock[(String, Condition, Option[Localisation], Option[Module]) => Future[Result]]
      val locObj=Localisation(bson,bson2,"loc",Some(1.2f),Some(3.4f),Some("un com"),List("img.jpg"))
      val session=Json.obj("debut"->date2,"fin"->date,"commentaire"->"unCom","module"->bson.stringify,"localisation"->Json.toJson(locObj))
      val module=mock[Module]

      f.moduleDaoMock.findOne(org.mockito.Matchers.eq(Json.obj("delete"->false,"_id"->bson)))(any[ExecutionContext]) returns future{Some(module)}
      notCorrect.apply(org.mockito.Matchers.eq("La date de début doit être inférieur à la date de fin"),any[Condition],any[Option[Localisation]],org.mockito.Matchers.eq(Some(module))) returns future{Results.Ok("notCorrect func")}

      val req=FakeRequest(GET,"url").withSession("condition"->Json.stringify(session))
      val r=f.controller.verifyAllData("Test")(correct)(notCorrect)(req)

      status(r) must equalTo(OK)
      contentAsString(r) must equalTo("notCorrect func")

      there was one(f.moduleDaoMock).findOne(org.mockito.Matchers.eq(Json.obj("delete"->false,"_id"->bson)))(any[ExecutionContext])
      there was one(notCorrect).apply(org.mockito.Matchers.eq("La date de début doit être inférieur à la date de fin"),any[Condition],any[Option[Localisation]],org.mockito.Matchers.eq(Some(module)))
      there was no(correct).apply(any[Condition],any[Option[Localisation]],any[Module])
    }

    "execute notCorrect function if localisation not found for ground condition" in new WithApplication {
      val f = fixture
      val correct = mock[(Condition, Option[Localisation], Module) => Future[Result]]
      val notCorrect = mock[(String, Condition, Option[Localisation], Option[Module]) => Future[Result]]
      val session=Json.obj("debut"->date,"fin"->date2,"commentaire"->"unCom","module"->bson.stringify)
      val module=mock[Module]

      f.moduleDaoMock.findOne(org.mockito.Matchers.eq(Json.obj("delete"->false,"_id"->bson)))(any[ExecutionContext]) returns future{Some(module)}
      notCorrect.apply(org.mockito.Matchers.eq("Erreur de localisation"),any[Condition],any[Option[Localisation]],org.mockito.Matchers.eq(Some(module))) returns future{Results.Ok("notCorrect func")}

      val req=FakeRequest(GET,"url").withSession("condition"->Json.stringify(session))
      val r=f.controller.verifyAllData("Terrain")(correct)(notCorrect)(req)

      status(r) must equalTo(OK)
      contentAsString(r) must equalTo("notCorrect func")

      there was one(f.moduleDaoMock).findOne(org.mockito.Matchers.eq(Json.obj("delete"->false,"_id"->bson)))(any[ExecutionContext])
      there was one(notCorrect).apply(org.mockito.Matchers.eq("Erreur de localisation"),any[Condition],any[Option[Localisation]],org.mockito.Matchers.eq(Some(module)))
      there was no(correct).apply(any[Condition],any[Option[Localisation]],any[Module])
    }

    "execute notCorrect function if module already used" in new WithApplication {
      val f = fixture
      val correct = mock[(Condition, Option[Localisation], Module) => Future[Result]]
      val notCorrect = mock[(String, Condition, Option[Localisation], Option[Module]) => Future[Result]]
      val session=Json.obj("debut"->date,"fin"->date2,"commentaire"->"unCom","module"->bson.stringify)
      val module=mock[Module]
      val cond=mock[Condition]
      val queryBegin=Json.obj("dateDebut"->Json.obj("$gte"->date,"$lte"->date2))
      val queryEnd=Json.obj("$or"->JsArray(Seq(Json.obj("dateFin"->Json.obj("$gte"->date,"$lte"->date2),"dateFin"->Json.obj("$exists"->false)))))
      val query=Json.obj("modules"->bson,"$or"->JsArray(Seq(queryBegin,queryEnd)))

      f.moduleDaoMock.findOne(org.mockito.Matchers.eq(Json.obj("delete"->false,"_id"->bson)))(any[ExecutionContext]) returns future{Some(module)}
      notCorrect.apply(org.mockito.Matchers.eq("Le module est déjà utilisé dans la même période sur une autre installation"),any[Condition],any[Option[Localisation]],org.mockito.Matchers.eq(Some(module))) returns future{Results.Ok("notCorrect func")}
      f.conditionDaoMock.findOne(org.mockito.Matchers.eq(query))(any[ExecutionContext]) returns future{Some(cond)}

      val req=FakeRequest(GET,"url").withSession("condition"->Json.stringify(session))
      val r=f.controller.verifyAllData("Test")(correct)(notCorrect)(req)

      status(r) must equalTo(OK)
      contentAsString(r) must equalTo("notCorrect func")

      there was one(f.moduleDaoMock).findOne(org.mockito.Matchers.eq(Json.obj("delete"->false,"_id"->bson)))(any[ExecutionContext])
      there was one(notCorrect).apply(org.mockito.Matchers.eq("Le module est déjà utilisé dans la même période sur une autre installation"),any[Condition],any[Option[Localisation]],org.mockito.Matchers.eq(Some(module)))
      there was no(correct).apply(any[Condition],any[Option[Localisation]],any[Module])
      there was one(f.conditionDaoMock).findOne(org.mockito.Matchers.eq(query))(any[ExecutionContext])
    }

    "execute correct function if data are correct" in new WithApplication {
      val f = fixture
      val correct = mock[(Condition, Option[Localisation], Module) => Future[Result]]
      val notCorrect = mock[(String, Condition, Option[Localisation], Option[Module]) => Future[Result]]
      val session=Json.obj("debut"->date,"fin"->date2,"commentaire"->"unCom","module"->bson.stringify)
      val module=mock[Module]
      val queryBegin=Json.obj("dateDebut"->Json.obj("$gte"->date,"$lte"->date2))
      val queryEnd=Json.obj("$or"->JsArray(Seq(Json.obj("dateFin"->Json.obj("$gte"->date,"$lte"->date2),"dateFin"->Json.obj("$exists"->false)))))
      val query=Json.obj("modules"->bson,"$or"->JsArray(Seq(queryBegin,queryEnd)))

      f.moduleDaoMock.findOne(org.mockito.Matchers.eq(Json.obj("delete"->false,"_id"->bson)))(any[ExecutionContext]) returns future{Some(module)}
      correct.apply(any[Condition],any[Option[Localisation]],org.mockito.Matchers.eq(module)) returns future{Results.Ok("correct func")}
      f.conditionDaoMock.findOne(org.mockito.Matchers.eq(query))(any[ExecutionContext]) returns future{None}

      val req=FakeRequest(GET,"url").withSession("condition"->Json.stringify(session))
      val r=f.controller.verifyAllData("Test")(correct)(notCorrect)(req)

      status(r) must equalTo(OK)
      contentAsString(r) must equalTo("correct func")

      there was one(f.moduleDaoMock).findOne(org.mockito.Matchers.eq(Json.obj("delete"->false,"_id"->bson)))(any[ExecutionContext])
      there was no(notCorrect).apply(anyString,any[Condition],any[Option[Localisation]],any[Option[Module]])
      there was one(correct).apply(any[Condition],any[Option[Localisation]],org.mockito.Matchers.eq(module))
      there was one(f.conditionDaoMock).findOne(org.mockito.Matchers.eq(query))(any[ExecutionContext])
    }
  }

  "When method createQueryIntersect is called, CondtionsManager" should{
    "return JsObject without $lte clause if end date is equal to None" in new WithApplication{
      val f=fixture

      val r=f.controller.createQueryIntersect("dateDebut",date,None)

      r must equalTo(Json.obj("dateDebut"->Json.obj("$gte"->date)))
    }

    "return JsObject with $lte clause" in new WithApplication{
      val f=fixture

      val r=f.controller.createQueryIntersect("dateDebut",date,Some(date2))

      r must equalTo(Json.obj("dateDebut"->Json.obj("$gte"->date,"$lte"->date2)))
    }
  }

  "When method verifyModuleAvailable is called, ConditionsManager" should{
    "Return true if condition not found" in new WithApplication {
      val f = fixture
      val queryBegin = Json.obj("dateDebut" -> Json.obj("$gte" -> date, "$lte" -> date2))
      val queryEnd = Json.obj("$or" -> JsArray(Seq(Json.obj("dateFin" -> Json.obj("$gte" -> date, "$lte" -> date2), "dateFin" -> Json.obj("$exists" -> false)))))
      val query = Json.obj("modules" -> bson, "$or" -> JsArray(Seq(queryBegin, queryEnd)))

      f.conditionDaoMock.findOne(org.mockito.Matchers.eq(query))(any[ExecutionContext]) returns future{None}

      f.controller.verifyModuleAvailable(bson,date,Some(date2)) must beTrue

      there was one(f.conditionDaoMock).findOne(org.mockito.Matchers.eq(query))(any[ExecutionContext])
    }

    "Return false if condition found" in new WithApplication {
      val f = fixture
      val queryBegin = Json.obj("dateDebut" -> Json.obj("$gte" -> date, "$lte" -> date2))
      val queryEnd = Json.obj("$or" -> JsArray(Seq(Json.obj("dateFin" -> Json.obj("$gte" -> date, "$lte" -> date2), "dateFin" -> Json.obj("$exists" -> false)))))
      val query = Json.obj("modules" -> bson, "$or" -> JsArray(Seq(queryBegin, queryEnd)))
      val cond=mock[Condition]

      f.conditionDaoMock.findOne(org.mockito.Matchers.eq(query))(any[ExecutionContext]) returns future{Some(cond)}

      f.controller.verifyModuleAvailable(bson,date,Some(date2)) must beFalse

      there was one(f.conditionDaoMock).findOne(org.mockito.Matchers.eq(query))(any[ExecutionContext])
    }
  }

  "When method moveImages is called, ConditionsManager" should{
    "Move files into an other directory" in new WithApplication{
      val f=fixture
      val tempFile=mock[TemporaryFile]
      val tempFile2=mock[TemporaryFile]
      val f1=new File("/route/public/images/campaign/img.jpg")
      val f2=new File("/route/public/images/campaign/img2.jpg")

      f.appMock.path returns (new File("/route"))
      f.tempFileBuilderMock.createTemporaryFile(org.mockito.Matchers.eq(new File("/route/public/images/campaign/tmp/img.jpg"))) returns tempFile
      f.tempFileBuilderMock.createTemporaryFile(org.mockito.Matchers.eq(new File("/route/public/images/campaign/tmp/img2.jpg"))) returns tempFile2

      org.mockito.Mockito.doNothing.when(tempFile).moveTo(org.mockito.Matchers.eq(f1),any[Boolean])
      org.mockito.Mockito.doNothing.when(tempFile2).moveTo(org.mockito.Matchers.eq(f2),any[Boolean])

      tempFile.clean() returns true
      tempFile2.clean() returns true

      f.controller.moveImages(List("img.jpg","img2.jpg"))

      there was 4.times(f.appMock).path
      there was one(f.tempFileBuilderMock).createTemporaryFile(org.mockito.Matchers.eq(new File("/route/public/images/campaign/tmp/img.jpg")))
      there was one(f.tempFileBuilderMock).createTemporaryFile(org.mockito.Matchers.eq(new File("/route/public/images/campaign/tmp/img2.jpg")))

      there was one(tempFile).moveTo(org.mockito.Matchers.eq(f1),any[Boolean])
      there was one(tempFile2).moveTo(org.mockito.Matchers.eq(f2),any[Boolean])

      there was one(tempFile).clean()
      there was one(tempFile2).clean()
    }

    "Not find files" in new WithApplication{
      val f=fixture

      f.controller.moveImages(List())

      there was no(f.appMock).path
      there was no(f.tempFileBuilderMock).createTemporaryFile(any[File])
    }
  }

  "When method insertLocalisation is called, ConditionsManager" should{
    "do nothing if not have localisation" in new WithApplication{
      val f=fixture
      val cond=mock[Condition]

      val r=f.controller.insertLocalisation(None,cond)

      Await.result(r,Duration.Inf) must equalTo(LastError(true,None,None,None,None,0,false))

      there was no(f.appMock).path
      there was no(f.tempFileBuilderMock).createTemporaryFile(any[File])
      there was no(f.localisationDaoMock).insert(any[Localisation],any[GetLastError])(any[ExecutionContext])
    }

    "move picture and insert the localisation" in new WithApplication{
      val f=fixture
      val localisation=Localisation(bson,bson2,"Test",Some(1.2f),Some(3.4f),Some("un com"),List("img.jpg"))
      val tempFile=mock[TemporaryFile]
      val f1=new File("/route/public/images/campaign/img.jpg")
      val lastError=mock[LastError]
      val cond=Condition(bson2,date,None,None,bson3)

      f.appMock.path returns (new File("/route"))
      f.tempFileBuilderMock.createTemporaryFile(org.mockito.Matchers.eq(new File("/route/public/images/campaign/tmp/img.jpg"))) returns tempFile
      org.mockito.Mockito.doNothing.when(tempFile).moveTo(org.mockito.Matchers.eq(f1),any[Boolean])
      tempFile.clean() returns true
      f.localisationDaoMock.insert(org.mockito.Matchers.eq(localisation),any[GetLastError])(any[ExecutionContext]) returns future{lastError}

      val r=f.controller.insertLocalisation(Some(localisation),cond)

      there was 2.times(f.appMock).path
      there was one(f.tempFileBuilderMock).createTemporaryFile(any[File])
      there was one(tempFile).moveTo(org.mockito.Matchers.eq(f1),any[Boolean])
      there was one(tempFile).clean()
      there was one(f.localisationDaoMock).insert(org.mockito.Matchers.eq(localisation),any[GetLastError])(any[ExecutionContext])
    }
  }

  "When method findLocalisation is called, ConditionsManager" should{
    "return an empty list if is not a ground campaign" in new WithApplication{
      val f=fixture
      val cond=mock[List[JsValue]]

      val r=f.controller.findLocalisation("Test",cond)

      Await.result(r,Duration.Inf) must equalTo(List[Localisation]())

      there was no(f.localisationDaoMock).findAll(any[JsObject],any[JsObject])(any[ExecutionContext])
    }

    "return the list of localisation associat to conditions" in new WithApplication{
      val f=fixture
      val cond=List(Json.toJson(bson),Json.toJson(bson2))
      val localisation=List(mock[Localisation],mock[Localisation])

      f.localisationDaoMock.findAll(org.mockito.Matchers.eq(Json.obj("condition"->Json.obj("$in"->JsArray(cond.toSeq)))),any[JsObject])(any[ExecutionContext]) returns future{localisation}

      val r=f.controller.findLocalisation("Terrain",cond)

      Await.result(r,Duration.Inf) must equalTo(localisation)

      there was one(f.localisationDaoMock).findAll(org.mockito.Matchers.eq(Json.obj("condition"->Json.obj("$in"->JsArray(cond.toSeq)))),any[JsObject])(any[ExecutionContext])
    }
  }

  "When method createQueryConditionDate is called, ConditionsManager" should{
    "Return a JsObject for get all conditions" in new WithApplication{
      val f=fixture
      val id=List(Json.toJson(bson))

      val r=f.controller.createQueryConditionDate("",id)

      r must equalTo(Json.obj("_id" -> Json.obj("$in" -> JsArray(id.toSeq))))
    }

    "Return a JsObject for get conditions finish" in new WithApplication{
      val f=fixture
      val id=List(Json.toJson(bson))
      val before=new Date

      val r=f.controller.createQueryConditionDate("finish",id)

      r.\("_id") must equalTo(Json.obj("$in" -> JsArray(id.toSeq)))
      val date=r.\("dateFin").\("$lt").as[Date]
      date.compareTo(new Date) must lessThanOrEqualTo(0)
      date.compareTo(before) must greaterThanOrEqualTo(0)
    }

    "Return a JsObject for get conditions ongoing" in new WithApplication{
      val f=fixture
      val id=List(Json.toJson(bson))
      val before=new Date

      val r=f.controller.createQueryConditionDate("ongoing",id)

      r.\("_id") must equalTo(Json.obj("$in" -> JsArray(id.toSeq)))
      val seq=r.\("$or").as[List[JsValue]]
      seq(0) must equalTo(Json.obj("dateFin"->Json.obj("$exists"->false)))
      val date1=seq(1).\("dateDebut").\("$lt").as[Date]
      val date2=seq(1).\("dateFin").\("$gt").as[Date]
      date1.compareTo(new Date) must lessThanOrEqualTo(0)
      date1.compareTo(before) must greaterThanOrEqualTo(0)
      date2.compareTo(new Date) must lessThanOrEqualTo(0)
      date2.compareTo(before) must greaterThanOrEqualTo(0)
    }
  }
}