import java.util.Date

import controllers._
import models.{Module, Campagne, CampagneDao, ConditionDao}
import org.junit.runner.RunWith
import org.specs2.matcher.{MatchResult, Expectable, Matcher}
import org.specs2.mock.Mockito
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner
import play.api.mvc.Result
import play.api.test.Helpers._
import play.api.test.{FakeRequest, WithApplication}
import reactivemongo.bson.{BSONDocument, BSONObjectID}

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

  val bson = BSONObjectID.generate
  val bson2 = BSONObjectID.generate
  val bson3 = BSONObjectID.generate
  val bson4 = BSONObjectID.generate

  def fixture=new {
    val conditionDaoMock=mock[ConditionDao]
    val campaignManagerMock=mock[CampagneManagerLike]
    val moduleManagerMock=mock[ModuleManagerLike]
    val controller=new ConditionsManagerTest{
      override val conditionsDao=conditionDaoMock
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
      content must contain("<input type=\"text\" id=\"debut\" name=\"debut\" value=\"\" class=\"form-control\"/>")
      content must contain("<input type=\"text\" id=\"fin\" name=\"fin\" value=\"\" class=\"form-control\"/>")
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
}