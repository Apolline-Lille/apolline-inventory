import java.util.Date

import controllers._
import models._
import org.junit.runner.RunWith
import org.specs2.matcher.{MatchResult, Expectable, Matcher}
import org.specs2.mock.Mockito
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner
import play.api.Configuration
import play.api.data.Form
import play.api.libs.json.{Writes, JsObject, Json}
import play.api.mvc.{Call, Results, Action, Result}
import play.api.test.Helpers._
import play.api.test.{FakeRequest, WithApplication}
import play.modules.reactivemongo.json.BSONFormats.BSONObjectIDFormat
import reactivemongo.bson.{BSONDocument, BSONObjectID}
import reactivemongo.core.commands.{LastError, GetLastError}

import scala.concurrent._

@RunWith(classOf[JUnitRunner])
class CardsManagerSpec extends Specification with Mockito {

  class CardsManagerTest extends CardsManagerLike

  class TypeCardsManagerTest extends TypeCardsManagerLike

  case class matchRegex(a: String) extends Matcher[String]() {
    override def apply[S <: String](t: Expectable[S]): MatchResult[S] = result(a.r.findFirstIn(t.value).nonEmpty, "okMessage", t.value + "not found " + a, t)
  }

  case class contains(a: String, b: Int) extends Matcher[String]() {
    override def apply[S <: String](t: Expectable[S]): MatchResult[S] = result(a.r.findAllMatchIn(t.value).size == b, "okMessage", "not found " + b + " times but " + a.r.findAllMatchIn(t.value).size + " times " + a, t)
  }

  val date = new Date(115, 3, 22)
  val date2 = new Date(115, 3, 23)

  val bson = BSONObjectID.generate
  val bson2 = BSONObjectID.generate
  val bson3 = BSONObjectID.generate
  val bson4 = BSONObjectID.generate

  def fixture = new {
    val typeCardsDaoMock=mock[TypeCardsDao]
    val cardDaoMock=mock[CardsDao]
    val firmwareDaoMock=mock[FirmwareDao]
    val typeCardsManagerMock=mock[TypeCardsManagerLike]
    val configMock=mock[Configuration]
    val controller=new CardsManagerTest{
      override val typeCardsDao:TypeCardsDao=typeCardsDaoMock
      override val cardDao:CardsDao=cardDaoMock
      override val firmwareDao:FirmwareDao=firmwareDaoMock
      override val typeCardsManager:TypeCardsManagerLike=typeCardsManagerMock
      override val config=configMock
    }

    def applyFoundFunction() {
      typeCardsManagerMock.doIfTypeCardsFound(org.mockito.Matchers.eq(bson))(any[TypeCards => Future[Result]])(any[Unit => Future[Result]]) answers { (params, _) => params match {
        case Array(_, p: (TypeCards => Future[Result]), _) => p.apply(TypeCards(bson,"mod_TypeCards","typ_TypeCards"))
      }}
    }

    def applyNotFoundFunction() {
      typeCardsManagerMock.doIfTypeCardsFound(org.mockito.Matchers.eq(bson))(any[TypeCards => Future[Result]])(any[Unit => Future[Result]]) answers { (params, _) => params match {
        case Array(_,_, p: (Unit => Future[Result])) => p.apply()
      }}
    }
  }

  "When user is not connected, CardsManager" should {
    "redirect to login for resource /inventary/cards/:id" in new WithApplication {
      route(FakeRequest(GET, "/inventary/cards/" + bson.stringify)).map(
        r => {
          status(r) must equalTo(SEE_OTHER)
          header("Location", r) must equalTo(Some("/login"))
        }
      ).getOrElse(
          failure("Pas de retour de la fonction")
        )
    }

    "redirect to login for resource /inventary/cards/:id/card" in new WithApplication{
      route(FakeRequest(GET, "/inventary/cards/"+bson.stringify+"/card")).map(
        r=> {

          status(r) must equalTo(SEE_OTHER)
          header("Location", r) must equalTo(Some("/login"))
        }
      ).getOrElse(
          failure("Pas de retour de la fonction")
        )
    }

    "redirect to login for resource /inventary/cards/:id/card" in new WithApplication{
      route(FakeRequest(POST, "/inventary/cards/"+bson.stringify+"/card")).map(
        r=> {

          status(r) must equalTo(SEE_OTHER)
          header("Location", r) must equalTo(Some("/login"))
        }
      ).getOrElse(
          failure("Pas de retour de la fonction")
        )
    }
  }

  "When user is on resource /inventary/cards/:id, CardsManager" should {

    "send 200 OK page with result" in new WithApplication {
      val f = fixture
      val typeCards = TypeCards(bson, "mod", "type")
      val list_card = List[Cards](
        Cards(bson2, "Id", bson, bson3, date, None, true, Some("v01"), true, None)
      )

      f.configMock.getString(org.mockito.Matchers.eq("hostname"),any[Option[Set[String]]]) returns Some("http://hostname/")
      f.cardDaoMock.findAll(any[JsObject], org.mockito.Matchers.eq(Json.obj("acquisition" -> -1)))(any[ExecutionContext]) returns future {
        list_card
      }
      f.typeCardsDaoMock.findById(org.mockito.Matchers.eq(bson))(any[ExecutionContext]) returns future {
        Some(typeCards)
      }
      f.firmwareDaoMock.findAll(any[JsObject], any[JsObject])(any[ExecutionContext]) returns future {
        List[Firmware](Firmware(bson3, "firm", "v02"))
      }
      f.cardDaoMock.countUsedCards(org.mockito.Matchers.eq(List(typeCards))) returns future{List((bson,0))}

      val r = f.controller.inventary(bson.stringify, "acquisition", -1).apply(FakeRequest(GET, "/inventary/cards/" + bson.stringify).withSession("user" -> """{"login":"test"}"""))

      status(r) must equalTo(OK)
      contentType(r) must beSome.which(_ == "text/html")
      val content = contentAsString(r)
      content must contain("<title>Inventaire des cartes</title>")
      content must contain("<td>Id</td>")
      content must contain("<td>22/04/2015</td>")
      content must contain("<td>-</td>")
      content must contain("<td> Oui </td>")
      content must contain("<td>firm (v02)</td>")
      content must contain("<td>v01</td>")
      content must contain("<td>Hors service</td>")

      there was one(f.cardDaoMock).findAll(any[JsObject], org.mockito.Matchers.eq(Json.obj("acquisition" -> -1)))(any[ExecutionContext])
      there was one(f.typeCardsDaoMock).findById(org.mockito.Matchers.eq(bson))(any[ExecutionContext])
      there was one(f.firmwareDaoMock).findAll(any[JsObject], any[JsObject])(any[ExecutionContext])
      there was one(f.cardDaoMock).countUsedCards(org.mockito.Matchers.eq(List(typeCards)))
      there was one(f.configMock).getString(org.mockito.Matchers.eq("hostname"),any[Option[Set[String]]])
    }

    "send 200 OK page with the message 'Aucun résultat trouvé', if not have cards" in new WithApplication {
      val f = fixture
      val typeCards=TypeCards(bson, "mod", "type")
      val url="http://hostname/inventary/cards?types=typ"

      f.configMock.getString(org.mockito.Matchers.eq("hostname"),any[Option[Set[String]]]) returns Some("http://hostname/")

      f.typeCardsDaoMock.findById(org.mockito.Matchers.eq(bson))(any[ExecutionContext]) returns future {
        Some(typeCards)
      }
      f.cardDaoMock.findAll(any[JsObject], any[JsObject])(any[ExecutionContext]) returns future {
        List[Cards]()
      }
      f.firmwareDaoMock.findAll(any[JsObject], any[JsObject])(any[ExecutionContext]) returns future {
        List[Firmware]()
      }
      f.cardDaoMock.countUsedCards(org.mockito.Matchers.eq(List(typeCards))) returns future{List((bson,0))}

      val r = f.controller.inventary(bson.stringify).apply(FakeRequest(GET, "/inventary/cards/" + bson.stringify).withHeaders(("Referer"->url)).withSession("user" -> """{"login":"test"}"""))

      status(r) must equalTo(OK)
      contentType(r) must beSome.which(_ == "text/html")
      val content = contentAsString(r)
      content must contain("<title>Inventaire des cartes</title>")
      content must matchRegex("type\\s*/\\s*mod")
      content must contain("<h3 style=\"text-align:center\">Aucun résultat trouvé</h3>")
      content must matchRegex("<span class=\"bold\">\\s*Stock\\s*</span>\\s*:\\s*0 / 0")
      session(r).get("previous") must beSome(url)

      there was one(f.typeCardsDaoMock).findById(org.mockito.Matchers.eq(bson))(any[ExecutionContext])
      there was one(f.cardDaoMock).findAll(any[JsObject], any[JsObject])(any[ExecutionContext])
      there was one(f.firmwareDaoMock).findAll(any[JsObject], any[JsObject])(any[ExecutionContext])
      there was one(f.cardDaoMock).countUsedCards(org.mockito.Matchers.eq(List(typeCards)))
      there was one(f.configMock).getString(org.mockito.Matchers.eq("hostname"),any[Option[Set[String]]])
    }

    "send 200 OK page with 2 result" in new WithApplication {
      val f = fixture
      val typeCards = TypeCards(bson, "mod", "type")
      val list_card = List[Cards](
        Cards(bson2, "Id", bson, bson3, date, None, true, Some("v01"), true, None),
        Cards(bson4, "Id2", bson, bson3, date, Some(date2), false, Some("v03"), false, None)
      )

      f.configMock.getString(org.mockito.Matchers.eq("hostname"),any[Option[Set[String]]]) returns Some("http://hostname/")
      f.cardDaoMock.findAll(any[JsObject], org.mockito.Matchers.eq(Json.obj("acquisition" -> -1)))(any[ExecutionContext]) returns future {
        list_card
      }
      f.typeCardsDaoMock.findById(org.mockito.Matchers.eq(bson))(any[ExecutionContext]) returns future {
        Some(typeCards)
      }
      f.firmwareDaoMock.findAll(any[JsObject], any[JsObject])(any[ExecutionContext]) returns future {
        List[Firmware](Firmware(bson3, "firm", "v02"))
      }
      f.cardDaoMock.countUsedCards(org.mockito.Matchers.eq(List(typeCards))) returns future{List((bson,1))}

      val r = f.controller.inventary(bson.stringify, "acquisition", -1).apply(FakeRequest(GET, "/inventary/cards/" + bson.stringify).withSession("user" -> """{"login":"test"}"""))

      status(r) must equalTo(OK)
      contentType(r) must beSome.which(_ == "text/html")
      val content = contentAsString(r)
      content must contain("<title>Inventaire des cartes</title>")
      content must contain("<td>Id</td>")
      content must contain("<td>22/04/2015</td>")
      content must contain("<td>-</td>")
      content must contain("<td> Oui </td>")
      content must contain("<td>firm (v02)</td>")
      content must contain("<td>v01</td>")
      content must contain("<td>Hors service</td>")

      content must contain("<td>Id2</td>")
      content must contain("<td>22/04/2015</td>")
      content must contain("<td>23/04/2015</td>")
      content must contain("<td> Non </td>")
      content must contain("<td>firm (v02)</td>")
      content must contain("<td>v03</td>")

      there was one(f.cardDaoMock).findAll(any[JsObject], org.mockito.Matchers.eq(Json.obj("acquisition" -> -1)))(any[ExecutionContext])
      there was one(f.typeCardsDaoMock).findById(org.mockito.Matchers.eq(bson))(any[ExecutionContext])
      there was one(f.firmwareDaoMock).findAll(any[JsObject], any[JsObject])(any[ExecutionContext])
      there was one(f.configMock).getString(org.mockito.Matchers.eq("hostname"),any[Option[Set[String]]])
    }

    "send redirect if card type not found" in new WithApplication {
      val f = fixture

      f.typeCardsDaoMock.findById(org.mockito.Matchers.eq(bson))(any[ExecutionContext]) returns future {
        None
      }
      f.configMock.getString(org.mockito.Matchers.eq("hostname"),any[Option[Set[String]]]) returns Some("http://hostname/")

      val r = f.controller.inventary(bson.stringify).apply(FakeRequest(GET, "/inventary/cards/" + bson.stringify).withSession("user" -> """{"login":"test"}"""))

      status(r) must equalTo(SEE_OTHER)
      header("Location", r) must beSome("/inventary/cards")

      there was one(f.typeCardsDaoMock).findById(org.mockito.Matchers.eq(bson))(any[ExecutionContext])
      there was one(f.configMock).getString(org.mockito.Matchers.eq("hostname"),any[Option[Set[String]]])
    }

  }

  "WHen user is on resource /inventary/cards/:id/:id2/moreInformation, CardsManager" should{
    "send 200 Ok page with all information about the card" in new WithApplication{
      val f=fixture
      val cards=Cards(bson2,"idCard",bson,bson3,date,Some(date2),true,Some("v01"),true,Some("un com"))
      val firmware=Firmware(bson3,"firmware","v02")

      f.applyFoundFunction()
      f.cardDaoMock.findOne(org.mockito.Matchers.eq(Json.obj("delete"->false,"_id"->bson2)))(any[ExecutionContext]) returns future{Some(cards)}
      f.firmwareDaoMock.findById(org.mockito.Matchers.eq(bson3))(any[ExecutionContext]) returns future{Some(firmware)}

      val r=f.controller.moreInformation(bson.stringify,bson2.stringify).apply(FakeRequest(GET,"/inventary/cards/"+bson.stringify+"/"+bson2.stringify).withSession("user" -> """{"login":"test"}"""))

      status(r) must equalTo(OK)
      val content=contentAsString(r)
      content must contain("<h4>typ_TypeCards / mod_TypeCards</h4>")
      content must contain("<span class=\"bold\">Identifiant</span> : idCard")
      content must contain("<span class=\"bold\">Date d&#x27;acquisition</span> : 22/04/2015")
      content must contain("<span class=\"bold\">Date de première utilisation</span> : 23/04/2015")
      content must contain("<span class=\"bold\">Agrégateur</span> : Oui")
      content must contain("<span class=\"bold\">Firmware</span> : firmware")
      content must contain("<span class=\"bold\">Version du firmware</span> : v02")
      content must contain("<span class=\"bold\">Version d&#x27;apolline</span> : v01")
      content must contain("<span class=\"bold\">Etat</span> : Hors service")
      content must contain("<span class=\"bold\">Commentaire</span> : <br/>un com")

      there was one(f.typeCardsManagerMock).doIfTypeCardsFound(org.mockito.Matchers.eq(bson))(any[TypeCards=>Future[Result]])(any[Unit=>Future[Result]])
      there was one(f.cardDaoMock).findOne(org.mockito.Matchers.eq(Json.obj("delete"->false,"_id"->bson2)))(any[ExecutionContext])
      there was one(f.firmwareDaoMock).findById(org.mockito.Matchers.eq(bson3))(any[ExecutionContext])
    }

    "Send redirect if firmware not found" in new WithApplication{
      val f=fixture
      val cards=Cards(bson2,"idCard",bson,bson3,date,Some(date2),true,Some("v01"),true,Some("un com"))

      f.applyFoundFunction()
      f.cardDaoMock.findOne(org.mockito.Matchers.eq(Json.obj("delete"->false,"_id"->bson2)))(any[ExecutionContext]) returns future{Some(cards)}
      f.firmwareDaoMock.findById(org.mockito.Matchers.eq(bson3))(any[ExecutionContext]) returns future{None}

      val r=f.controller.moreInformation(bson.stringify,bson2.stringify).apply(FakeRequest(GET,"/inventary/cards/"+bson.stringify+"/"+bson2.stringify).withSession("user" -> """{"login":"test"}"""))

      status(r) must equalTo(SEE_OTHER)
      header("Location",r) must beSome("/inventary/cards/"+bson.stringify)

      there was one(f.typeCardsManagerMock).doIfTypeCardsFound(org.mockito.Matchers.eq(bson))(any[TypeCards=>Future[Result]])(any[Unit=>Future[Result]])
      there was one(f.cardDaoMock).findOne(org.mockito.Matchers.eq(Json.obj("delete"->false,"_id"->bson2)))(any[ExecutionContext])
      there was one(f.firmwareDaoMock).findById(org.mockito.Matchers.eq(bson3))(any[ExecutionContext])
    }

    "Send redirect if card not found" in new WithApplication{
      val f=fixture

      f.applyFoundFunction()
      f.cardDaoMock.findOne(org.mockito.Matchers.eq(Json.obj("delete"->false,"_id"->bson2)))(any[ExecutionContext]) returns future{None}

      val r=f.controller.moreInformation(bson.stringify,bson2.stringify).apply(FakeRequest(GET,"/inventary/cards/"+bson.stringify+"/"+bson2.stringify).withSession("user" -> """{"login":"test"}"""))

      status(r) must equalTo(SEE_OTHER)
      header("Location",r) must beSome("/inventary/cards/"+bson.stringify)

      there was one(f.typeCardsManagerMock).doIfTypeCardsFound(org.mockito.Matchers.eq(bson))(any[TypeCards=>Future[Result]])(any[Unit=>Future[Result]])
      there was one(f.cardDaoMock).findOne(org.mockito.Matchers.eq(Json.obj("delete"->false,"_id"->bson2)))(any[ExecutionContext])
      there was no(f.firmwareDaoMock).findById(org.mockito.Matchers.eq(bson3))(any[ExecutionContext])
    }

    "send redirect if type card not found" in new WithApplication{
      val f=fixture

      f.applyNotFoundFunction()

      val r=f.controller.moreInformation(bson.stringify,bson2.stringify).apply(FakeRequest(GET,"/inventary/cards/"+bson.stringify+"/"+bson2.stringify).withSession("user" -> """{"login":"test"}"""))

      status(r) must equalTo(SEE_OTHER)
      header("Location",r) must beSome("/inventary/cards")

      there was one(f.typeCardsManagerMock).doIfTypeCardsFound(org.mockito.Matchers.eq(bson))(any[TypeCards=>Future[Result]])(any[Unit=>Future[Result]])
      there was no(f.cardDaoMock).findOne(org.mockito.Matchers.eq(Json.obj("delete"->false,"_id"->bson2)))(any[ExecutionContext])
      there was no(f.firmwareDaoMock).findById(org.mockito.Matchers.eq(bson3))(any[ExecutionContext])
    }
  }

  "When method getinventaryCards is called, CardsManager" should{

    "Call function for print result" in new WithApplication {
      val f = fixture
      val typeCards = TypeCards(bson, "mod", "type")
      val func=mock[(TypeCards,List[Cards],List[Firmware],List[(BSONObjectID,Int)])=>Result]

      f.cardDaoMock.findAll(any[JsObject], org.mockito.Matchers.eq(Json.obj("acquisition" -> -1)))(any[ExecutionContext]) returns future {List[Cards]()}
      f.typeCardsDaoMock.findById(org.mockito.Matchers.eq(bson))(any[ExecutionContext]) returns future {Some(typeCards)}
      f.firmwareDaoMock.findAll(any[JsObject], any[JsObject])(any[ExecutionContext]) returns future {List[Firmware]()}
      f.cardDaoMock.countUsedCards(org.mockito.Matchers.eq(List(typeCards))) returns future{List((bson,0))}
      func.apply(org.mockito.Matchers.eq(typeCards),any[List[Cards]],any[List[Firmware]],any[List[(BSONObjectID,Int)]]) returns Results.Ok("call function")

      val req=FakeRequest(GET, "/inventary/cards/" + bson.stringify).withSession("user" -> """{"login":"test"}""")
      val action = Action.async{f.controller.getInventaryCards(Json.obj(),Json.obj("acquisition" -> -1),bson,Results.Redirect("/inventary/cards"))(func)}
      val r=call(action,req)

      status(r) must equalTo(OK)
      contentAsString(r) must equalTo("call function")

      there was one(f.cardDaoMock).findAll(any[JsObject], org.mockito.Matchers.eq(Json.obj("acquisition" -> -1)))(any[ExecutionContext])
      there was one(f.typeCardsDaoMock).findById(org.mockito.Matchers.eq(bson))(any[ExecutionContext])
      there was one(f.firmwareDaoMock).findAll(any[JsObject], any[JsObject])(any[ExecutionContext])
      there was one(f.cardDaoMock).countUsedCards(org.mockito.Matchers.eq(List(typeCards)))
      there was one(func).apply(org.mockito.Matchers.eq(typeCards),any[List[Cards]],any[List[Firmware]],any[List[(BSONObjectID,Int)]])
    }

    "send internal server error if mongoDB error when find card type" in new WithApplication {
      val f = fixture
      val futureMock=mock[Future[Option[TypeCards]]]
      val throwable=mock[Throwable]
      val func=mock[(TypeCards,List[Cards],List[Firmware],List[(BSONObjectID,Int)])=>Result]

      f.typeCardsDaoMock.findById(org.mockito.Matchers.eq(bson))(any[ExecutionContext]) returns futureMock
      futureMock.flatMap(any[Option[TypeCards]=>Future[Option[TypeCards]]])(any[ExecutionContext]) returns futureMock
      futureMock.recover(any[PartialFunction[Throwable,Result]])(any[ExecutionContext]) answers {value=>future{value.asInstanceOf[PartialFunction[Throwable,Result]](throwable)}}

      val req=FakeRequest(GET, "/inventary/cards/" + bson.stringify).withSession("user" -> """{"login":"test"}""")
      val action = Action.async{f.controller.getInventaryCards(Json.obj(),Json.obj(),bson,Results.Redirect("/inventary/cards"))(func)}
      val r=call(action,req)

      status(r) must equalTo(INTERNAL_SERVER_ERROR)

      there was one(f.typeCardsDaoMock).findById(org.mockito.Matchers.eq(bson))(any[ExecutionContext])
      there was one(futureMock).flatMap(any[Option[TypeCards]=>Future[Option[TypeCards]]])(any[ExecutionContext])
      there was one(futureMock).recover(any[PartialFunction[Throwable,Result]])(any[ExecutionContext])
      there was no(func).apply(any[TypeCards],any[List[Cards]],any[List[Firmware]],any[List[(BSONObjectID,Int)]])
    }

    "send internal server error if mongoDB error when find card" in new WithApplication{
      val f=fixture
      val typeCards=TypeCards(bson, "mod", "type")
      val futureMock=mock[Future[List[Cards]]]
      val throwable=mock[Throwable]
      val func=mock[(TypeCards,List[Cards],List[Firmware],List[(BSONObjectID,Int)])=>Result]

      f.cardDaoMock.findAll(any[JsObject],org.mockito.Matchers.eq(Json.obj("acquisition" -> -1)))(any[ExecutionContext]) returns futureMock
      f.typeCardsDaoMock.findById(org.mockito.Matchers.eq(bson))(any[ExecutionContext]) returns future{Some(typeCards)}
      f.firmwareDaoMock.findAll(any[JsObject],any[JsObject])(any[ExecutionContext]) returns  future{List[Firmware](Firmware(bson3,"firm","v02"))}
      futureMock.flatMap(any[List[Cards]=>Future[List[Cards]]])(any[ExecutionContext]) returns futureMock
      futureMock.recover(any[PartialFunction[Throwable,Result]])(any[ExecutionContext]) answers {value=>future{value.asInstanceOf[PartialFunction[Throwable,Result]](throwable)}}

      val req=FakeRequest(GET, "/inventary/cards/" + bson.stringify).withSession("user" -> """{"login":"test"}""")
      val action = Action.async{f.controller.getInventaryCards(Json.obj(),Json.obj("acquisition" -> -1),bson,Results.Redirect("/inventary/cards"))(func)}
      val r=call(action,req)

      status(r) must equalTo(INTERNAL_SERVER_ERROR)

      there was one(f.cardDaoMock).findAll(any[JsObject],org.mockito.Matchers.eq(Json.obj("acquisition" -> -1)))(any[ExecutionContext])
      there was one(f.typeCardsDaoMock).findById(org.mockito.Matchers.eq(bson))(any[ExecutionContext])
      there was one(f.firmwareDaoMock).findAll(any[JsObject],any[JsObject])(any[ExecutionContext])
      there was one(futureMock).flatMap(any[List[Cards]=>Future[List[Cards]]])(any[ExecutionContext])
      there was one(futureMock).recover(any[PartialFunction[Throwable,Result]])(any[ExecutionContext])
      there was no(func).apply(org.mockito.Matchers.eq(typeCards),any[List[Cards]],any[List[Firmware]],any[List[(BSONObjectID,Int)]])
    }

    "send internal server error if mongoDB error when find firmware" in new WithApplication{
      val f=fixture
      val typeCards=TypeCards(bson, "mod", "type")
      val futureMock=mock[Future[List[Firmware]]]
      val throwable=mock[Throwable]
      val func=mock[(TypeCards,List[Cards],List[Firmware],List[(BSONObjectID,Int)])=>Result]

      f.cardDaoMock.findAll(any[JsObject],org.mockito.Matchers.eq(Json.obj("acquisition" -> -1)))(any[ExecutionContext]) returns future{List[Cards]()}
      f.typeCardsDaoMock.findById(org.mockito.Matchers.eq(bson))(any[ExecutionContext]) returns future{Some(typeCards)}
      f.firmwareDaoMock.findAll(any[JsObject],any[JsObject])(any[ExecutionContext]) returns  futureMock
      futureMock.flatMap(any[List[Firmware]=>Future[List[Firmware]]])(any[ExecutionContext]) returns futureMock
      futureMock.recover(any[PartialFunction[Throwable,Result]])(any[ExecutionContext]) answers {value=>future{value.asInstanceOf[PartialFunction[Throwable,Result]](throwable)}}

      val req=FakeRequest(GET, "/inventary/cards/" + bson.stringify).withSession("user" -> """{"login":"test"}""")
      val action = Action.async{f.controller.getInventaryCards(Json.obj(),Json.obj("acquisition" -> -1),bson,Results.Redirect("/inventary/cards"))(func)}
      val r=call(action,req)

      status(r) must equalTo(INTERNAL_SERVER_ERROR)

      there was one(f.cardDaoMock).findAll(any[JsObject],org.mockito.Matchers.eq(Json.obj("acquisition" -> -1)))(any[ExecutionContext])
      there was one(f.typeCardsDaoMock).findById(org.mockito.Matchers.eq(bson))(any[ExecutionContext])
      there was one(f.firmwareDaoMock).findAll(any[JsObject],any[JsObject])(any[ExecutionContext])
      there was one(futureMock).flatMap(any[List[Firmware]=>Future[List[Firmware]]])(any[ExecutionContext])
      there was one(futureMock).recover(any[PartialFunction[Throwable,Result]])(any[ExecutionContext])
      there was no(func).apply(org.mockito.Matchers.eq(typeCards),any[List[Cards]],any[List[Firmware]],any[List[(BSONObjectID,Int)]])
    }
  }

  "When method printForm is called, CardsManager" should{
    "print an empty form" in new WithApplication{
      val f=fixture

      f.cardDaoMock.findApolline() returns future{Stream[BSONDocument]()}
      f.firmwareDaoMock.findFirmware() returns future{Stream[BSONDocument]()}
      f.firmwareDaoMock.findVersionFirmware() returns future{Stream[BSONDocument]()}

      val req=FakeRequest(GET, "/inventary/cards/"+bson.stringify+"/card").withSession("user" -> """{"login":"test"}""")
      val action = Action.async{implicit request=>f.controller.printForm(Results.Ok,bson.stringify,CardsManager.form,mock[Call])}
      val r=call(action,req)

      status(r) must equalTo(OK)
      contentType(r) must beSome.which(_ == "text/html")
      val content = contentAsString(r)
      content must contain("<title>Inventaire des cartes</title>")
      content must contain("<input type=\"text\" id=\"id\" name=\"id\" value=\"\" class=\"form-control\"/>")
      content must contain("<input type=\"date\" id=\"acquisition\" name=\"acquisition\" value=\"\" class=\"form-control\"/>")
      content must contain("<input type=\"date\" id=\"firstUse\" name=\"firstUse\" value=\"\" class=\"form-control\"/>")
      content must contain("<input type=\"checkbox\" id=\"agregateur\" name=\"agregateur\" value=\"true\"  />")
      content must contain("<input type=\"text\" id=\"apolline\" name=\"apolline\" class=\"form-control\" value=\"\" autocomplete=\"off\" list=\"list_apolline\"/>")
      content must contain("<input type=\"text\" id=\"firmware\" name=\"firmware\" class=\"form-control\" value=\"\" autocomplete=\"off\" list=\"list_firmware\"/>")
      content must contain("<input type=\"text\" id=\"versionFirmware\" name=\"versionFirmware\" class=\"form-control\" value=\"\" autocomplete=\"off\" list=\"list_versionFirmware\"/>")
      content must contain("<input type=\"checkbox\" id=\"hs\" name=\"hs\" value=\"true\"  />")
      content must contain("<textarea id=\"commentaire\" name=\"commentaire\" class=\"form-control\"></textarea>")

      there was one(f.cardDaoMock).findApolline()
      there was one(f.firmwareDaoMock).findFirmware()
      there was one(f.firmwareDaoMock).findVersionFirmware()
    }

    "Send 500 internal error if mongoDB Error when find apolline data" in new WithApplication {
      val f = fixture
      val futureMock=mock[Future[Stream[BSONDocument]]]
      val throwable=mock[Throwable]

      f.cardDaoMock.findApolline() returns futureMock
      f.firmwareDaoMock.findFirmware() returns future{Stream[BSONDocument]()}
      f.firmwareDaoMock.findVersionFirmware() returns future {Stream[BSONDocument]()}
      futureMock.map(any[Stream[BSONDocument]=>Stream[BSONDocument]])(any[ExecutionContext]) returns futureMock
      futureMock.recover(any[PartialFunction[Throwable,Result]])(any[ExecutionContext]) answers {value => future{value.asInstanceOf[PartialFunction[Throwable,Result]](throwable)}}

      val req = FakeRequest(GET, "/inventary/cards/" + bson.stringify + "/card").withSession("user" -> """{"login":"test"}""")
      val action = Action.async { implicit request => f.controller.printForm(Results.Ok, bson.stringify, CardsManager.form, mock[Call]) }
      val r = call(action, req)

      status(r) must equalTo(INTERNAL_SERVER_ERROR)

      there was one(f.cardDaoMock).findApolline()
      there was one(f.firmwareDaoMock).findFirmware()
      there was one(f.firmwareDaoMock).findVersionFirmware()
      there was one(futureMock).map(any[Stream[BSONDocument]=>Stream[BSONDocument]])(any[ExecutionContext])
      there was one(futureMock).recover(any[PartialFunction[Throwable,Result]])(any[ExecutionContext])
    }

    "Send 500 internal error if mongoDB Error when find all firmware" in new WithApplication {
      val f = fixture
      val futureMock=mock[Future[Stream[BSONDocument]]]
      val throwable=mock[Throwable]

      f.cardDaoMock.findApolline() returns future{Stream[BSONDocument]()}
      f.firmwareDaoMock.findFirmware() returns futureMock
      f.firmwareDaoMock.findVersionFirmware() returns future {Stream[BSONDocument]()}
      futureMock.flatMap(any[Stream[BSONDocument]=>Future[Stream[BSONDocument]]])(any[ExecutionContext]) returns futureMock
      futureMock.recover(any[PartialFunction[Throwable,Result]])(any[ExecutionContext]) answers {value => future{value.asInstanceOf[PartialFunction[Throwable,Result]](throwable)}}

      val req = FakeRequest(GET, "/inventary/cards/" + bson.stringify + "/card").withSession("user" -> """{"login":"test"}""")
      val action = Action.async { implicit request => f.controller.printForm(Results.Ok, bson.stringify, CardsManager.form, mock[Call]) }
      val r = call(action, req)

      status(r) must equalTo(INTERNAL_SERVER_ERROR)

      there was one(f.cardDaoMock).findApolline()
      there was one(f.firmwareDaoMock).findFirmware()
      there was one(f.firmwareDaoMock).findVersionFirmware()
      there was one(futureMock).flatMap(any[Stream[BSONDocument]=>Future[Stream[BSONDocument]]])(any[ExecutionContext])
      there was one(futureMock).recover(any[PartialFunction[Throwable,Result]])(any[ExecutionContext])
    }

    "Send 500 internal error if mongoDB Error when find all firmware version" in new WithApplication {
      val f = fixture
      val futureMock=mock[Future[Stream[BSONDocument]]]
      val throwable=mock[Throwable]

      f.cardDaoMock.findApolline() returns future{Stream[BSONDocument]()}
      f.firmwareDaoMock.findFirmware() returns future{Stream[BSONDocument]()}
      f.firmwareDaoMock.findVersionFirmware() returns futureMock
      futureMock.flatMap(any[Stream[BSONDocument]=>Future[Stream[BSONDocument]]])(any[ExecutionContext]) returns futureMock
      futureMock.recover(any[PartialFunction[Throwable,Result]])(any[ExecutionContext]) answers {value => future{value.asInstanceOf[PartialFunction[Throwable,Result]](throwable)}}

      val req = FakeRequest(GET, "/inventary/cards/" + bson.stringify + "/card").withSession("user" -> """{"login":"test"}""")
      val action = Action.async { implicit request => f.controller.printForm(Results.Ok, bson.stringify, CardsManager.form, mock[Call]) }
      val r = call(action, req)

      status(r) must equalTo(INTERNAL_SERVER_ERROR)

      there was one(f.cardDaoMock).findApolline()
      there was one(f.firmwareDaoMock).findFirmware()
      there was one(f.firmwareDaoMock).findVersionFirmware()
      there was one(futureMock).flatMap(any[Stream[BSONDocument]=>Future[Stream[BSONDocument]]])(any[ExecutionContext])
      there was one(futureMock).recover(any[PartialFunction[Throwable,Result]])(any[ExecutionContext])
    }
  }

  "When method printFormWIthData is called, CardsManager" should{

    "send bad_request with the form if method type not exist" in new WithApplication{
      val f=fixture
      val func=mock[(Cards,Firmware)=>CardsForm]
      val routeCall=mock[Call]

      f.applyNotFoundFunction()
      f.cardDaoMock.findApolline() returns future{Stream[BSONDocument]()}
      f.firmwareDaoMock.findFirmware() returns future{Stream[BSONDocument]()}
      f.firmwareDaoMock.findVersionFirmware() returns future{Stream[BSONDocument]()}

      val req=FakeRequest(GET,"/inventary/cards/"+bson.stringify+"/"+bson2.stringify).withSession("user" -> """{"login":"test"}""")
      val action=Action.async(implicit request => f.controller.printFormWithData(bson.stringify,bson2.stringify,routeCall)(func))
      val r=call(action,req)

      status(r) must equalTo(BAD_REQUEST)
      contentType(r) must beSome.which(_ == "text/html")
      val content = contentAsString(r)
      content must contain("<title>Inventaire des cartes</title>")
      content must contain("Ce type de carte n'existe pas")

      there was one(f.typeCardsManagerMock).doIfTypeCardsFound(org.mockito.Matchers.eq(bson))(any[TypeCards=>Future[Result]])(any[Unit=>Future[Result]])
      there was one(f.cardDaoMock).findApolline()
      there was one(f.firmwareDaoMock).findFirmware()
      there was one(f.firmwareDaoMock).findVersionFirmware()
    }

    "send 200 Ok page with a prefilled form" in new WithApplication{
      val f=fixture
      val func=mock[(Cards,Firmware)=>CardsForm]
      val card=Cards(bson2,"Id",bson,bson3,date,Some(date2),true,Some("v01"),true,Some("un com"))
      val firmware=Firmware(bson3,"firm","v02")
      val routeCall=mock[Call]

      f.applyFoundFunction()
      f.cardDaoMock.findById(org.mockito.Matchers.eq(bson2))(any[ExecutionContext]) returns future{Some(card)}
      f.firmwareDaoMock.findById(org.mockito.Matchers.eq(bson3))(any[ExecutionContext]) returns future{Some(firmware)}
      func.apply(org.mockito.Matchers.eq(card),org.mockito.Matchers.eq(firmware)) returns CardsForm("Id",date,Some(date2),true,Some("v01"),"firm","v02",true,Some("un com"),"")
      f.cardDaoMock.findApolline() returns future{Stream[BSONDocument]()}
      f.firmwareDaoMock.findFirmware() returns future{Stream[BSONDocument]()}
      f.firmwareDaoMock.findVersionFirmware() returns future{Stream[BSONDocument]()}

      val req=FakeRequest(GET,"/inventary/cards/"+bson.stringify+"/"+bson2.stringify).withSession("user" -> """{"login":"test"}""")
      val action=Action.async(implicit request => f.controller.printFormWithData(bson.stringify,bson2.stringify,routeCall)(func))
      val r=call(action,req)

      status(r) must equalTo(OK)
      contentType(r) must beSome.which(_ == "text/html")
      val content = contentAsString(r)
      content must contain("<title>Inventaire des cartes</title>")
      content must contain("<input type=\"text\" id=\"id\" name=\"id\" value=\"Id\" class=\"form-control\"/>")
      content must contain("<input type=\"date\" id=\"acquisition\" name=\"acquisition\" value=\"2015-04-22\" class=\"form-control\"/>")
      content must contain("<input type=\"date\" id=\"firstUse\" name=\"firstUse\" value=\"2015-04-23\" class=\"form-control\"/>")
      content must contain("<input type=\"checkbox\" id=\"agregateur\" name=\"agregateur\" value=\"true\" checked=\"checked\" />")
      content must contain("<input type=\"text\" id=\"apolline\" name=\"apolline\" class=\"form-control\" value=\"v01\" autocomplete=\"off\" list=\"list_apolline\"/>")
      content must contain("<input type=\"text\" id=\"firmware\" name=\"firmware\" class=\"form-control\" value=\"firm\" autocomplete=\"off\" list=\"list_firmware\"/>")
      content must contain("<input type=\"text\" id=\"versionFirmware\" name=\"versionFirmware\" class=\"form-control\" value=\"v02\" autocomplete=\"off\" list=\"list_versionFirmware\"/>")
      content must contain("<input type=\"checkbox\" id=\"hs\" name=\"hs\" value=\"true\" checked=\"checked\" />")
      content must contain("<textarea id=\"commentaire\" name=\"commentaire\" class=\"form-control\">un com</textarea>")

      there was one(f.typeCardsManagerMock).doIfTypeCardsFound(org.mockito.Matchers.eq(bson))(any[TypeCards=>Future[Result]])(any[Unit=>Future[Result]])
      there was one(f.cardDaoMock).findById(org.mockito.Matchers.eq(bson2))(any[ExecutionContext])
      there was one(f.firmwareDaoMock).findById(org.mockito.Matchers.eq(bson3))(any[ExecutionContext])
      there was one(func).apply(org.mockito.Matchers.eq(card),org.mockito.Matchers.eq(firmware))
      there was one(f.cardDaoMock).findApolline()
      there was one(f.firmwareDaoMock).findFirmware()
      there was one(f.firmwareDaoMock).findVersionFirmware()
    }

    "send redirect if card not found" in new WithApplication{
      val f=fixture
      val func=mock[(Cards,Firmware)=>CardsForm]
      val routeCall=mock[Call]

      f.applyFoundFunction()
      f.cardDaoMock.findById(org.mockito.Matchers.eq(bson2))(any[ExecutionContext]) returns future{None}

      val req=FakeRequest(GET,"/inventary/cards/"+bson.stringify+"/"+bson2.stringify).withSession("user" -> """{"login":"test"}""")
      val action=Action.async(implicit request => f.controller.printFormWithData(bson.stringify,bson2.stringify,routeCall)(func))
      val r=call(action,req)

      status(r) must equalTo(SEE_OTHER)
      header("Location",r) must beSome("/inventary/cards/"+bson.stringify)

      there was one(f.typeCardsManagerMock).doIfTypeCardsFound(org.mockito.Matchers.eq(bson))(any[TypeCards=>Future[Result]])(any[Unit=>Future[Result]])
      there was one(f.cardDaoMock).findById(org.mockito.Matchers.eq(bson2))(any[ExecutionContext])
    }

    "send redirect if firmware not found" in new WithApplication{
      val f=fixture
      val func=mock[(Cards,Firmware)=>CardsForm]
      val card=Cards(bson2,"Id",bson,bson3,date,Some(date2),true,Some("v01"),true,Some("un com"))
      val routeCall=mock[Call]

      f.applyFoundFunction()
      f.cardDaoMock.findById(org.mockito.Matchers.eq(bson2))(any[ExecutionContext]) returns future{Some(card)}
      f.firmwareDaoMock.findById(org.mockito.Matchers.eq(bson3))(any[ExecutionContext]) returns future{None}

      val req=FakeRequest(GET,"/inventary/cards/"+bson.stringify+"/"+bson2.stringify).withSession("user" -> """{"login":"test"}""")
      val action=Action.async(implicit request => f.controller.printFormWithData(bson.stringify,bson2.stringify,routeCall)(func))
      val r=call(action,req)

      status(r) must equalTo(SEE_OTHER)
      header("Location",r) must beSome("/inventary/cards/"+bson.stringify)

      there was one(f.typeCardsManagerMock).doIfTypeCardsFound(org.mockito.Matchers.eq(bson))(any[TypeCards=>Future[Result]])(any[Unit=>Future[Result]])
      there was one(f.cardDaoMock).findById(org.mockito.Matchers.eq(bson2))(any[ExecutionContext])
      there was one(f.firmwareDaoMock).findById(org.mockito.Matchers.eq(bson3))(any[ExecutionContext])
    }

    "send 500 internal server error if have mongoDB error when find card" in new WithApplication{
      val f=fixture
      val func=mock[(Cards,Firmware)=>CardsForm]
      val futureMock=mock[Future[Option[Cards]]]
      val routeCall=mock[Call]
      val throwable=mock[Throwable]

      f.applyFoundFunction()
      f.cardDaoMock.findById(org.mockito.Matchers.eq(bson2))(any[ExecutionContext]) returns futureMock
      futureMock.flatMap(any[Option[Cards]=>Future[Option[Cards]]])(any[ExecutionContext]) returns futureMock
      futureMock.recover(any[PartialFunction[Throwable,Result]])(any[ExecutionContext]) answers {value=>future{value.asInstanceOf[PartialFunction[Throwable,Result]](throwable)}}

      val req=FakeRequest(GET,"/inventary/cards/"+bson.stringify+"/"+bson2.stringify).withSession("user" -> """{"login":"test"}""")
      val action=Action.async(implicit request => f.controller.printFormWithData(bson.stringify,bson2.stringify,routeCall)(func))
      val r=call(action,req)

      status(r) must equalTo(INTERNAL_SERVER_ERROR)

      there was one(f.typeCardsManagerMock).doIfTypeCardsFound(org.mockito.Matchers.eq(bson))(any[TypeCards=>Future[Result]])(any[Unit=>Future[Result]])
      there was one(f.cardDaoMock).findById(org.mockito.Matchers.eq(bson2))(any[ExecutionContext])
      there was one(futureMock).flatMap(any[Option[Cards]=>Future[Option[Cards]]])(any[ExecutionContext])
      there was one(futureMock).recover(any[PartialFunction[Throwable,Result]])(any[ExecutionContext])
    }

    "send 500 internal server error if have mongoDB error when find firmware" in new WithApplication{
      val f=fixture
      val func=mock[(Cards,Firmware)=>CardsForm]
      val card=Cards(bson2,"Id",bson,bson3,date,Some(date2),true,Some("v01"),true,Some("un com"))
      val futureMock=mock[Future[Option[Firmware]]]
      val routeCall=mock[Call]
      val throwable=mock[Throwable]

      f.applyFoundFunction()
      f.cardDaoMock.findById(org.mockito.Matchers.eq(bson2))(any[ExecutionContext]) returns future{Some(card)}
      f.firmwareDaoMock.findById(org.mockito.Matchers.eq(bson3))(any[ExecutionContext]) returns futureMock
      futureMock.flatMap(any[Option[Firmware]=>Future[Option[Firmware]]])(any[ExecutionContext]) returns futureMock
      futureMock.recover(any[PartialFunction[Throwable,Result]])(any[ExecutionContext]) answers {value=>future{value.asInstanceOf[PartialFunction[Throwable,Result]](throwable)}}

      val req=FakeRequest(GET,"/inventary/cards/"+bson.stringify+"/"+bson2.stringify).withSession("user" -> """{"login":"test"}""")
      val action=Action.async(implicit request => f.controller.printFormWithData(bson.stringify,bson2.stringify,routeCall)(func))
      val r=call(action,req)

      status(r) must equalTo(INTERNAL_SERVER_ERROR)

      there was one(f.typeCardsManagerMock).doIfTypeCardsFound(org.mockito.Matchers.eq(bson))(any[TypeCards=>Future[Result]])(any[Unit=>Future[Result]])
      there was one(f.cardDaoMock).findById(org.mockito.Matchers.eq(bson2))(any[ExecutionContext])
      there was one(f.firmwareDaoMock).findById(org.mockito.Matchers.eq(bson3))(any[ExecutionContext])
      there was one(futureMock).flatMap(any[Option[Firmware]=>Future[Option[Firmware]]])(any[ExecutionContext])
      there was one(futureMock).recover(any[PartialFunction[Throwable,Result]])(any[ExecutionContext])
    }
  }

  "When method insertFirmwareIfNotFound is called, CardsManager" should{
    "applied function in parameter if firmware exist" in new WithApplication{
      val f=fixture
      val cardForm=mock[CardsForm]
      val func=mock[(CardsForm,BSONObjectID)=>Future[Result]]
      val firmware=Firmware(bson,"firm","v01")

      cardForm.firmware returns "firm"
      cardForm.versionFirmware returns "v01"
      f.firmwareDaoMock.findOne(org.mockito.Matchers.eq(Json.obj("nom"->"firm","version"->"v01")))(any[ExecutionContext]) returns future{Some(firmware)}
      func.apply(org.mockito.Matchers.eq(cardForm),org.mockito.Matchers.eq(bson)) returns future{Results.Ok("exec func")}

      val req=FakeRequest(GET, "/inventary/cards/" + bson.stringify).withSession("user" -> """{"login":"test"}""")
      val action=Action.async{f.controller.insertFirmwareIfNotFound(cardForm,func)}
      val r=call(action,req)

      status(r) must beEqualTo(OK)
      contentAsString(r) must beEqualTo("exec func")

      there was one(cardForm).firmware
      there was one(cardForm).versionFirmware
      there was one(f.firmwareDaoMock).findOne(org.mockito.Matchers.eq(Json.obj("nom"->"firm","version"->"v01")))(any[ExecutionContext])
      there was one(func).apply(org.mockito.Matchers.eq(cardForm),org.mockito.Matchers.eq(bson))
    }

    "insert firmware into the database and applied function in parameter if firmware not exist" in new WithApplication{
      val f=fixture
      val cardForm=mock[CardsForm]
      val func=mock[(CardsForm,BSONObjectID)=>Future[Result]]
      val lastError=mock[LastError]

      cardForm.firmware returns "firm"
      cardForm.versionFirmware returns "v01"
      f.firmwareDaoMock.findOne(org.mockito.Matchers.eq(Json.obj("nom"->"firm","version"->"v01")))(any[ExecutionContext]) returns future{None}
      f.firmwareDaoMock.insert(any[Firmware],any[GetLastError])(any[ExecutionContext]) returns future{lastError}
      func.apply(org.mockito.Matchers.eq(cardForm),any[BSONObjectID]) returns future{Results.Ok("exec func")}

      val req=FakeRequest(GET, "/inventary/cards/" + bson.stringify).withSession("user" -> """{"login":"test"}""")
      val action=Action.async{f.controller.insertFirmwareIfNotFound(cardForm,func)}
      val r=call(action,req)

      status(r) must beEqualTo(OK)
      contentAsString(r) must beEqualTo("exec func")

      there was 2.times(cardForm).firmware
      there was 2.times(cardForm).versionFirmware
      there was one(f.firmwareDaoMock).findOne(org.mockito.Matchers.eq(Json.obj("nom"->"firm","version"->"v01")))(any[ExecutionContext])
      there was one(f.firmwareDaoMock).insert(any[Firmware],any[GetLastError])(any[ExecutionContext])
      there was one(func).apply(org.mockito.Matchers.eq(cardForm),any[BSONObjectID])
    }

    "send 500 internal error if mongoDB error when find firmware" in new WithApplication{
      val f=fixture
      val cardForm=mock[CardsForm]
      val func=mock[(CardsForm,BSONObjectID)=>Future[Result]]
      val futureMock=mock[Future[Option[Firmware]]]
      val throwable=mock[Throwable]

      cardForm.firmware returns "firm"
      cardForm.versionFirmware returns "v01"
      f.firmwareDaoMock.findOne(org.mockito.Matchers.eq(Json.obj("nom"->"firm","version"->"v01")))(any[ExecutionContext]) returns futureMock
      futureMock.flatMap(any[Option[Firmware]=>Future[Option[Firmware]]])(any[ExecutionContext]) returns futureMock
      futureMock.recover(any[PartialFunction[Throwable,Result]])(any[ExecutionContext]) answers {value => future{value.asInstanceOf[PartialFunction[Throwable,Result]](throwable)}}

      val req=FakeRequest(GET, "/inventary/cards/" + bson.stringify).withSession("user" -> """{"login":"test"}""")
      val action=Action.async{f.controller.insertFirmwareIfNotFound(cardForm,func)}
      val r=call(action,req)

      status(r) must beEqualTo(INTERNAL_SERVER_ERROR)

      there was one(cardForm).firmware
      there was one(cardForm).versionFirmware
      there was one(f.firmwareDaoMock).findOne(org.mockito.Matchers.eq(Json.obj("nom"->"firm","version"->"v01")))(any[ExecutionContext])
      there was one(futureMock).flatMap(any[Option[Firmware]=>Future[Option[Firmware]]])(any[ExecutionContext])
      there was one(futureMock).recover(any[PartialFunction[Throwable,Result]])(any[ExecutionContext])
    }

    "send 500 internal error if mongoDB error when insert firmware" in new WithApplication{
      val f=fixture
      val cardForm=mock[CardsForm]
      val func=mock[(CardsForm,BSONObjectID)=>Future[Result]]
      val futureMock=mock[Future[LastError]]
      val throwable=mock[Throwable]

      cardForm.firmware returns "firm"
      cardForm.versionFirmware returns "v01"
      f.firmwareDaoMock.findOne(org.mockito.Matchers.eq(Json.obj("nom"->"firm","version"->"v01")))(any[ExecutionContext]) returns future{None}
      f.firmwareDaoMock.insert(any[Firmware],any[GetLastError])(any[ExecutionContext]) returns futureMock
      futureMock.flatMap(any[LastError=>Future[LastError]])(any[ExecutionContext]) returns futureMock
      futureMock.recover(any[PartialFunction[Throwable,Result]])(any[ExecutionContext]) answers {value => future{value.asInstanceOf[PartialFunction[Throwable,Result]](throwable)}}

      val req=FakeRequest(GET, "/inventary/cards/" + bson.stringify).withSession("user" -> """{"login":"test"}""")
      val action=Action.async{f.controller.insertFirmwareIfNotFound(cardForm,func)}
      val r=call(action,req)

      status(r) must beEqualTo(INTERNAL_SERVER_ERROR)

      there was 2.times(cardForm).firmware
      there was 2.times(cardForm).versionFirmware
      there was one(f.firmwareDaoMock).findOne(org.mockito.Matchers.eq(Json.obj("nom"->"firm","version"->"v01")))(any[ExecutionContext])
      there was one(f.firmwareDaoMock).insert(any[Firmware],any[GetLastError])(any[ExecutionContext])
      there was one(futureMock).flatMap(any[LastError=>Future[LastError]])(any[ExecutionContext])
      there was one(futureMock).recover(any[PartialFunction[Throwable,Result]])(any[ExecutionContext])
    }
  }

  "When user submit a form, CardsManager" should{
    "send bad_request with the form if card type not exist" in new WithApplication{
      val f=fixture
      val func=mock[(CardsForm,BSONObjectID)=>Future[Result]]
      val funcVerif=mock[CardsForm=>JsObject]
      val routeCall=mock[Call]

      f.applyNotFoundFunction()
      f.cardDaoMock.findApolline() returns future{Stream[BSONDocument]()}
      f.firmwareDaoMock.findFirmware() returns future{Stream[BSONDocument]()}
      f.firmwareDaoMock.findVersionFirmware() returns future{Stream[BSONDocument]()}

      val req=FakeRequest(POST,"/inventary/cards/"+bson.stringify+"/card").withSession("user" -> """{"login":"test"}""")
      val action=Action.async(implicit request => f.controller.submitForm("error",bson.stringify,routeCall)(funcVerif)(func))
      val r=call(action,req)

      status(r) must equalTo(BAD_REQUEST)
      contentType(r) must beSome.which(_ == "text/html")
      val content = contentAsString(r)
      content must contain("<title>Inventaire des cartes</title>")
      content must contain("Ce type de carte n'existe pas")

      there was one(f.typeCardsManagerMock).doIfTypeCardsFound(org.mockito.Matchers.eq(bson))(any[TypeCards=>Future[Result]])(any[Unit=>Future[Result]])
      there was one(f.cardDaoMock).findApolline()
      there was one(f.firmwareDaoMock).findFirmware()
      there was one(f.firmwareDaoMock).findVersionFirmware()
    }

    "send bad request when the form was submit with empty fields" in new WithApplication{
      val f=fixture
      val routeCall=mock[Call]
      val funcVerif=mock[CardsForm=>JsObject]
      val func=mock[(CardsForm,BSONObjectID)=>Future[Result]]

      f.applyFoundFunction()
      f.cardDaoMock.findApolline() returns future{Stream[BSONDocument]()}
      f.firmwareDaoMock.findFirmware() returns future{Stream[BSONDocument]()}
      f.firmwareDaoMock.findVersionFirmware() returns future{Stream[BSONDocument]()}

      val req=FakeRequest(POST,"/inventary/cards/"+bson.stringify+"/card").withSession("user" -> """{"login":"test"}""")
      val action=Action.async(implicit request => f.controller.submitForm("error",bson.stringify,routeCall)(funcVerif)(func))
      val r=call(action,req)

      status(r) must equalTo(BAD_REQUEST)
      contentType(r) must beSome.which(_ == "text/html")
      val content = contentAsString(r)
      content must contain("<title>Inventaire des cartes</title>")
      content must contains("<span class=\"control-label errors\">This field is required</span>",4)
      content must not contain("<span class=\"control-label errors\">Valid date required</span>")

      there was one(f.typeCardsManagerMock).doIfTypeCardsFound(org.mockito.Matchers.eq(bson))(any[TypeCards=>Future[Result]])(any[Unit=>Future[Result]])
      there was one(f.cardDaoMock).findApolline()
      there was one(f.firmwareDaoMock).findFirmware()
      there was one(f.firmwareDaoMock).findVersionFirmware()
    }

    "send bad request when the form was submit with not valid date" in new WithApplication{
      val f=fixture
      val routeCall=mock[Call]
      val funcVerif=mock[CardsForm=>JsObject]
      val func=mock[(CardsForm,BSONObjectID)=>Future[Result]]
      val data=Json.parse("""{"id":"Id","acquisition":"a","firstUse":"a","firmware":"firm","versionFirmware":"v01","send":"submit"}""")

      f.applyFoundFunction()
      f.cardDaoMock.findApolline() returns future{Stream[BSONDocument]()}
      f.firmwareDaoMock.findFirmware() returns future{Stream[BSONDocument]()}
      f.firmwareDaoMock.findVersionFirmware() returns future{Stream[BSONDocument]()}

      val req=FakeRequest(POST,"/inventary/cards/"+bson.stringify+"/card").withJsonBody(data).withSession("user" -> """{"login":"test"}""")
      val action=Action.async(implicit request => f.controller.submitForm("error",bson.stringify,routeCall)(funcVerif)(func))
      val r=call(action,req)

      status(r) must equalTo(BAD_REQUEST)
      contentType(r) must beSome.which(_ == "text/html")
      val content = contentAsString(r)
      content must contain("<title>Inventaire des cartes</title>")
      content must not contain("<span class=\"control-label errors\">This field is required</span>")
      content must contains("<span class=\"control-label errors\">Valid date required</span>",2)

      there was one(f.typeCardsManagerMock).doIfTypeCardsFound(org.mockito.Matchers.eq(bson))(any[TypeCards=>Future[Result]])(any[Unit=>Future[Result]])
      there was one(f.cardDaoMock).findApolline()
      there was one(f.firmwareDaoMock).findFirmware()
      there was one(f.firmwareDaoMock).findVersionFirmware()
    }

    "Verify if the acquisition date is before first use date" in new WithApplication{
      val f=fixture
      val formMock=mock[Form[CardsForm]]
      val formMock2=mock[Form[CardsForm]]
      val cardForm=CardsForm("",date2,Some(date),false,None,"","",false,None,"")

      formMock.withError(org.mockito.Matchers.eq("firstUse"),org.mockito.Matchers.eq("La date de première utilisation doit être supèrieur à la date d'acquisition"),any[Array[Any]]) returns formMock2

      val ret=f.controller.verifyErrorAcquisitionAfterFirstUse(cardForm,formMock)

      ret must equalTo(formMock2)

      there was one(formMock).withError(org.mockito.Matchers.eq("firstUse"),org.mockito.Matchers.eq("La date de première utilisation doit être supèrieur à la date d'acquisition"),any[Array[Any]])
    }

    "send bad request when the form was submit with date error" in new WithApplication{
      val f=fixture
      val routeCall=mock[Call]
      val funcVerif=mock[CardsForm=>JsObject]
      val func=mock[(CardsForm,BSONObjectID)=>Future[Result]]
      val data=Json.parse("""{"id":"Id","acquisition":"2015-04-23","firstUse":"2015-04-22","firmware":"firm","versionFirmware":"v01","send":"submit"}""")

      f.applyFoundFunction()
      f.cardDaoMock.findApolline() returns future{Stream[BSONDocument]()}
      f.firmwareDaoMock.findFirmware() returns future{Stream[BSONDocument]()}
      f.firmwareDaoMock.findVersionFirmware() returns future{Stream[BSONDocument]()}

      val req=FakeRequest(POST,"/inventary/cards/"+bson.stringify+"/card").withJsonBody(data).withSession("user" -> """{"login":"test"}""")
      val action=Action.async(implicit request => f.controller.submitForm("error",bson.stringify,routeCall)(funcVerif)(func))
      val r=call(action,req)

      status(r) must equalTo(BAD_REQUEST)
      contentType(r) must beSome.which(_ == "text/html")
      val content = contentAsString(r)
      content must contain("<title>Inventaire des cartes</title>")
      content must not contain("<span class=\"control-label errors\">This field is required</span>")
      content must contain("<span class=\"control-label errors\">La date de première utilisation doit être supèrieur à la date d&#x27;acquisition</span>")

      there was one(f.typeCardsManagerMock).doIfTypeCardsFound(org.mockito.Matchers.eq(bson))(any[TypeCards=>Future[Result]])(any[Unit=>Future[Result]])
      there was one(f.cardDaoMock).findApolline()
      there was one(f.firmwareDaoMock).findFirmware()
      there was one(f.firmwareDaoMock).findVersionFirmware()
    }

    "send 500 internal error if mongoDB error when find card" in new WithApplication{
      val f=fixture
      val routeCall=mock[Call]
      val funcVerif=mock[CardsForm=>JsObject]
      val func=mock[(CardsForm,BSONObjectID)=>Future[Result]]
      val data=Json.parse("""{"id":"Id","acquisition":"2015-04-22","firmware":"firm","versionFirmware":"v01","send":"submit"}""")
      val futureMock=mock[Future[List[Cards]]]
      val throwable=mock[Throwable]

      f.applyFoundFunction()
      f.cardDaoMock.findAll(any[JsObject],any[JsObject])(any[ExecutionContext]) returns futureMock
      funcVerif.apply(any[CardsForm]) returns Json.obj()
      futureMock.flatMap(any[List[Cards]=>Future[List[Cards]]])(any[ExecutionContext]) returns futureMock
      futureMock.recover(any[PartialFunction[Throwable,Result]])(any[ExecutionContext]) answers (value=>future{value.asInstanceOf[PartialFunction[Throwable,Result]](throwable)})

      val req=FakeRequest(POST,"/inventary/cards/"+bson.stringify+"/card").withJsonBody(data).withSession("user" -> """{"login":"test"}""")
      val action=Action.async(implicit request => f.controller.submitForm("error",bson.stringify,routeCall)(funcVerif)(func))
      val r=call(action,req)

      status(r) must equalTo(INTERNAL_SERVER_ERROR)

      there was one(f.typeCardsManagerMock).doIfTypeCardsFound(org.mockito.Matchers.eq(bson))(any[TypeCards=>Future[Result]])(any[Unit=>Future[Result]])
      there was one(f.cardDaoMock).findAll(any[JsObject],any[JsObject])(any[ExecutionContext])
      there was one(funcVerif).apply(any[CardsForm])
      there was one(futureMock).flatMap(any[List[Cards]=>Future[List[Cards]]])(any[ExecutionContext])
      there was one(futureMock).recover(any[PartialFunction[Throwable,Result]])(any[ExecutionContext])
    }

    "send bad_request if card exist" in new WithApplication{
      val f=fixture
      val routeCall=mock[Call]
      val funcVerif=mock[CardsForm=>JsObject]
      val func=mock[(CardsForm,BSONObjectID)=>Future[Result]]
      val data=Json.parse("""{"id":"Id","acquisition":"2015-04-22","firmware":"firm","versionFirmware":"v01","send":"submit"}""")
      val futureMock=mock[Future[Option[Cards]]]
      val card=mock[Cards]

      f.applyFoundFunction()
      f.cardDaoMock.findAll(any[JsObject],any[JsObject])(any[ExecutionContext]) returns future{List(card)}
      funcVerif.apply(any[CardsForm]) returns Json.obj()
      f.cardDaoMock.findApolline() returns future{Stream[BSONDocument]()}
      f.firmwareDaoMock.findFirmware() returns future{Stream[BSONDocument]()}
      f.firmwareDaoMock.findVersionFirmware() returns future{Stream[BSONDocument]()}

      val req=FakeRequest(POST,"/inventary/cards/"+bson.stringify+"/card").withJsonBody(data).withSession("user" -> """{"login":"test"}""")
      val action=Action.async(implicit request => f.controller.submitForm("error",bson.stringify,routeCall)(funcVerif)(func))
      val r=call(action,req)

      status(r) must equalTo(BAD_REQUEST)
      val content=contentAsString(r)
      content must contain("<div class=\"alert alert-danger\" role=\"alert\">Cette carte existe déjà</div>")

      there was one(f.typeCardsManagerMock).doIfTypeCardsFound(org.mockito.Matchers.eq(bson))(any[TypeCards=>Future[Result]])(any[Unit=>Future[Result]])
      there was one(f.cardDaoMock).findAll(any[JsObject],any[JsObject])(any[ExecutionContext])
      there was one(funcVerif).apply(any[CardsForm])
      there was one(f.cardDaoMock).findApolline()
      there was one(f.firmwareDaoMock).findFirmware()
      there was one(f.firmwareDaoMock).findVersionFirmware()
    }
  }

  "When user is on the resource /inventary/cards/:id/card , CardsManager" should {
    "send 200 on OK with an empty form" in new WithApplication {
      val f=fixture

      f.applyFoundFunction()
      f.cardDaoMock.findApolline() returns future{Stream[BSONDocument]()}
      f.firmwareDaoMock.findFirmware() returns future{Stream[BSONDocument]()}
      f.firmwareDaoMock.findVersionFirmware() returns future{Stream[BSONDocument]()}

      val req=FakeRequest(GET, "/inventary/cards/"+bson.stringify+"/card").withSession("user" -> """{"login":"test"}""")
      val r = f.controller.cardPage(bson.stringify).apply(req)

      status(r) must equalTo(OK)
      contentType(r) must beSome.which(_ == "text/html")
      val content = contentAsString(r)
      content must contain("<title>Inventaire des cartes</title>")
      content must contain("<input type=\"text\" id=\"id\" name=\"id\" value=\"\" class=\"form-control\"/>")
      content must matchRegex("<input type=\"date\" id=\"acquisition\" name=\"acquisition\" value=\"\\d{4}-\\d{2}-\\d{2}\" class=\"form-control\"/>")
      content must contain("<input type=\"date\" id=\"firstUse\" name=\"firstUse\" value=\"\" class=\"form-control\"/>")
      content must contain("<input type=\"checkbox\" id=\"agregateur\" name=\"agregateur\" value=\"true\"  />")
      content must contain("<input type=\"text\" id=\"apolline\" name=\"apolline\" class=\"form-control\" value=\"\" autocomplete=\"off\" list=\"list_apolline\"/>")
      content must contain("<input type=\"text\" id=\"firmware\" name=\"firmware\" class=\"form-control\" value=\"\" autocomplete=\"off\" list=\"list_firmware\"/>")
      content must contain("<input type=\"text\" id=\"versionFirmware\" name=\"versionFirmware\" class=\"form-control\" value=\"\" autocomplete=\"off\" list=\"list_versionFirmware\"/>")
      content must contain("<input type=\"checkbox\" id=\"hs\" name=\"hs\" value=\"true\"  />")
      content must contain("<textarea id=\"commentaire\" name=\"commentaire\" class=\"form-control\"></textarea>")

      there was one(f.cardDaoMock).findApolline()
      there was one(f.firmwareDaoMock).findFirmware()
      there was one(f.firmwareDaoMock).findVersionFirmware()
      there was one(f.typeCardsManagerMock).doIfTypeCardsFound(org.mockito.Matchers.eq(bson))(any[TypeCards => Future[Result]])(any[Unit => Future[Result]])
    }

    "send bad request with an empty form if card type not found" in new WithApplication {
      val f=fixture

      f.applyNotFoundFunction()
      f.cardDaoMock.findApolline() returns future{Stream[BSONDocument]()}
      f.firmwareDaoMock.findFirmware() returns future{Stream[BSONDocument]()}
      f.firmwareDaoMock.findVersionFirmware() returns future{Stream[BSONDocument]()}

      val req=FakeRequest(GET, "/inventary/cards/"+bson.stringify+"/card").withSession("user" -> """{"login":"test"}""")
      val r = f.controller.cardPage(bson.stringify).apply(req)

      status(r) must equalTo(BAD_REQUEST)
      contentType(r) must beSome.which(_ == "text/html")
      val content = contentAsString(r)
      content must contain("<title>Inventaire des cartes</title>")
      content must contain("<div class=\"alert alert-danger\" role=\"alert\">Ce type de carte n'existe pas</div>")
      content must contain("<input type=\"text\" id=\"id\" name=\"id\" value=\"\" class=\"form-control\"/>")
      content must matchRegex("<input type=\"date\" id=\"acquisition\" name=\"acquisition\" value=\"\\d{4}-\\d{2}-\\d{2}\" class=\"form-control\"/>")
      content must contain("<input type=\"date\" id=\"firstUse\" name=\"firstUse\" value=\"\" class=\"form-control\"/>")
      content must contain("<input type=\"checkbox\" id=\"agregateur\" name=\"agregateur\" value=\"true\"  />")
      content must contain("<input type=\"text\" id=\"apolline\" name=\"apolline\" class=\"form-control\" value=\"\" autocomplete=\"off\" list=\"list_apolline\"/>")
      content must contain("<input type=\"text\" id=\"firmware\" name=\"firmware\" class=\"form-control\" value=\"\" autocomplete=\"off\" list=\"list_firmware\"/>")
      content must contain("<input type=\"text\" id=\"versionFirmware\" name=\"versionFirmware\" class=\"form-control\" value=\"\" autocomplete=\"off\" list=\"list_versionFirmware\"/>")
      content must contain("<input type=\"checkbox\" id=\"hs\" name=\"hs\" value=\"true\"  />")
      content must contain("<textarea id=\"commentaire\" name=\"commentaire\" class=\"form-control\"></textarea>")

      there was one(f.cardDaoMock).findApolline()
      there was one(f.firmwareDaoMock).findFirmware()
      there was one(f.firmwareDaoMock).findVersionFirmware()
      there was one(f.typeCardsManagerMock).doIfTypeCardsFound(org.mockito.Matchers.eq(bson))(any[TypeCards => Future[Result]])(any[Unit => Future[Result]])
    }

    "send 500 internal error if have mongoDB error when insert card" in new WithApplication{
      val f=fixture
      val data=Json.parse("""{"id":"Id","acquisition":"2015-04-22","firmware":"firm","versionFirmware":"v01","send":"Envoyer"}""")
      val futureMock=mock[Future[LastError]]
      val throwable=mock[Throwable]

      f.applyFoundFunction()
      f.cardDaoMock.findAll(any[JsObject],any[JsObject])(any[ExecutionContext]) returns future{List()}
      f.firmwareDaoMock.findOne(org.mockito.Matchers.eq(Json.obj("nom"->"firm","version"->"v01")))(any[ExecutionContext]) returns future{Some(Firmware(bson,"firm","v01"))}
      f.cardDaoMock.insert(any[Cards],any[GetLastError])(any[ExecutionContext]) returns futureMock
      futureMock.flatMap(any[LastError=>Future[LastError]])(any[ExecutionContext]) returns futureMock
      futureMock.recover(any[PartialFunction[Throwable,Result]])(any[ExecutionContext]) answers {value=>future{value.asInstanceOf[PartialFunction[Throwable,Result]](throwable)}}

      val req=FakeRequest(POST,"/inventary/cards/"+bson.stringify+"/card").withJsonBody(data).withSession("user" -> """{"login":"test"}""")
      val r=f.controller.cardInsert(bson.stringify).apply(req)

      status(r) must equalTo(INTERNAL_SERVER_ERROR)

      there was one(f.typeCardsManagerMock).doIfTypeCardsFound(org.mockito.Matchers.eq(bson))(any[TypeCards=>Future[Result]])(any[Unit=>Future[Result]])
      there was one(f.cardDaoMock).findAll(any[JsObject],any[JsObject])(any[ExecutionContext])
      there was one(f.firmwareDaoMock).findOne(org.mockito.Matchers.eq(Json.obj("nom"->"firm","version"->"v01")))(any[ExecutionContext])
      there was one(f.cardDaoMock).insert(any[Cards],any[GetLastError])(any[ExecutionContext])
      there was one(futureMock).flatMap(any[LastError=>Future[LastError]])(any[ExecutionContext])
      there was one(futureMock).recover(any[PartialFunction[Throwable,Result]])(any[ExecutionContext])
    }

    "send redirect if user not would be continue to insert card" in new WithApplication{
      val f=fixture
      val data=Json.parse("""{"id":"Id","acquisition":"2015-04-22","firmware":"firm","versionFirmware":"v01","send":"Envoyer"}""")
      val lastError=mock[LastError]

      f.applyFoundFunction()
      f.cardDaoMock.findAll(any[JsObject],any[JsObject])(any[ExecutionContext]) returns future{List()}
      f.firmwareDaoMock.findOne(org.mockito.Matchers.eq(Json.obj("nom"->"firm","version"->"v01")))(any[ExecutionContext]) returns future{Some(Firmware(bson,"firm","v01"))}
      f.cardDaoMock.insert(any[Cards],any[GetLastError])(any[ExecutionContext]) returns future{lastError}

      val req=FakeRequest(POST,"/inventary/cards/"+bson.stringify+"/card").withJsonBody(data).withSession("user" -> """{"login":"test"}""")
      val r=f.controller.cardInsert(bson.stringify).apply(req)

      status(r) must equalTo(SEE_OTHER)
      header("Location",r) must equalTo(Some("/inventary/cards/"+bson.stringify))

      there was one(f.typeCardsManagerMock).doIfTypeCardsFound(org.mockito.Matchers.eq(bson))(any[TypeCards=>Future[Result]])(any[Unit=>Future[Result]])
      there was one(f.cardDaoMock).findAll(any[JsObject],any[JsObject])(any[ExecutionContext])
      there was one(f.firmwareDaoMock).findOne(org.mockito.Matchers.eq(Json.obj("nom"->"firm","version"->"v01")))(any[ExecutionContext])
      there was one(f.cardDaoMock).insert(any[Cards],any[GetLastError])(any[ExecutionContext])
    }

    "send redirect after reactivat delete card" in new WithApplication{
      val f=fixture
      val data=Json.parse("""{"id":"Id","acquisition":"2015-04-22","firmware":"firm","versionFirmware":"v01","send":"Réactiver"}""")
      val lastError=mock[LastError]
      val cards=Cards(bson2,"Id",bson,bson3,date,Some(date2),true,Some("v01"),true,Some("un com"),true)

      f.applyFoundFunction()
      f.cardDaoMock.findAll(any[JsObject],any[JsObject])(any[ExecutionContext]) returns future{List(cards)}
      f.firmwareDaoMock.findOne(org.mockito.Matchers.eq(Json.obj("nom"->"firm","version"->"v01")))(any[ExecutionContext]) returns future{Some(Firmware(bson,"firm","v01"))}
      f.cardDaoMock.findOne(any[JsObject])(any[ExecutionContext]) returns future{Some(cards)}
      f.cardDaoMock.updateById(org.mockito.Matchers.eq(bson2), any[Cards],any[GetLastError])(any[Writes[Cards]],any[ExecutionContext]) returns future{lastError}

      val req=FakeRequest(POST,"/inventary/cards/"+bson.stringify+"/card").withJsonBody(data).withSession("user" -> """{"login":"test"}""")
      val r=f.controller.cardInsert(bson.stringify).apply(req)

      status(r) must equalTo(SEE_OTHER)
      header("Location",r) must equalTo(Some("/inventary/cards/"+bson.stringify))

      there was one(f.typeCardsManagerMock).doIfTypeCardsFound(org.mockito.Matchers.eq(bson))(any[TypeCards=>Future[Result]])(any[Unit=>Future[Result]])
      there was one(f.cardDaoMock).findAll(any[JsObject],any[JsObject])(any[ExecutionContext])
      there was one(f.firmwareDaoMock).findOne(org.mockito.Matchers.eq(Json.obj("nom"->"firm","version"->"v01")))(any[ExecutionContext])
      there was one(f.cardDaoMock).findOne(any[JsObject])(any[ExecutionContext])
      there was one(f.cardDaoMock).updateById(org.mockito.Matchers.eq(bson2), any[Cards],any[GetLastError])(any[Writes[Cards]],any[ExecutionContext])
    }

    "send 200 Ok page after insert card" in new WithApplication{
      val f=fixture
      val data=Json.parse("""{"id":"Id","acquisition":"2015-04-22","firstUse":"2015-04-22","apolline":"vapolline","firmware":"firm","versionFirmware":"v01","commentaire":"un com","send":"Envoyer et continuer"}""")
      val lastError=mock[LastError]

      f.applyFoundFunction()
      f.cardDaoMock.findAll(any[JsObject],any[JsObject])(any[ExecutionContext]) returns future{List()}
      f.firmwareDaoMock.findOne(org.mockito.Matchers.eq(Json.obj("nom"->"firm","version"->"v01")))(any[ExecutionContext]) returns future{Some(Firmware(bson,"firm","v01"))}
      f.cardDaoMock.insert(any[Cards],any[GetLastError])(any[ExecutionContext]) returns future{lastError}
      f.cardDaoMock.findApolline() returns future{Stream[BSONDocument]()}
      f.firmwareDaoMock.findFirmware() returns future{Stream[BSONDocument]()}
      f.firmwareDaoMock.findVersionFirmware() returns future{Stream[BSONDocument]()}

      val req=FakeRequest(POST,"/inventary/cards/"+bson.stringify+"/card").withJsonBody(data).withSession("user" -> """{"login":"test"}""")
      val r=f.controller.cardInsert(bson.stringify).apply(req)

      status(r) must equalTo(OK)
      contentType(r) must beSome.which(_ == "text/html")
      val content=contentAsString(r)
      content must contain("<title>Inventaire des cartes</title>")
      content must contain("<input type=\"text\" id=\"id\" name=\"id\" value=\"\" class=\"form-control\"/>")
      content must contain("<input type=\"date\" id=\"acquisition\" name=\"acquisition\" value=\"2015-04-22\" class=\"form-control\"/>")
      content must contain("<input type=\"date\" id=\"firstUse\" name=\"firstUse\" value=\"2015-04-22\" class=\"form-control\"/>")
      content must contain("<input type=\"checkbox\" id=\"agregateur\" name=\"agregateur\" value=\"true\"  />")
      content must contain("<input type=\"text\" id=\"apolline\" name=\"apolline\" class=\"form-control\" value=\"vapolline\" autocomplete=\"off\" list=\"list_apolline\"/>")
      content must contain("<input type=\"text\" id=\"firmware\" name=\"firmware\" class=\"form-control\" value=\"firm\" autocomplete=\"off\" list=\"list_firmware\"/>")
      content must contain("<input type=\"text\" id=\"versionFirmware\" name=\"versionFirmware\" class=\"form-control\" value=\"v01\" autocomplete=\"off\" list=\"list_versionFirmware\"/>")
      content must contain("<input type=\"checkbox\" id=\"hs\" name=\"hs\" value=\"true\"  />")
      content must contain("<textarea id=\"commentaire\" name=\"commentaire\" class=\"form-control\">un com</textarea>")

      there was one(f.typeCardsManagerMock).doIfTypeCardsFound(org.mockito.Matchers.eq(bson))(any[TypeCards=>Future[Result]])(any[Unit=>Future[Result]])
      there was one(f.cardDaoMock).findAll(any[JsObject],any[JsObject])(any[ExecutionContext])
      there was one(f.firmwareDaoMock).findOne(org.mockito.Matchers.eq(Json.obj("nom"->"firm","version"->"v01")))(any[ExecutionContext])
      there was one(f.cardDaoMock).insert(any[Cards],any[GetLastError])(any[ExecutionContext])
      there was one(f.cardDaoMock).findApolline()
      there was one(f.firmwareDaoMock).findFirmware()
      there was one(f.firmwareDaoMock).findVersionFirmware()
    }
  }

  "When user is on the page /inventary/cards/:id/:id2, CardsManager" should{
    "send 200 Ok page with a prefilled form" in new WithApplication{
      val f=fixture
      val card=Cards(bson2,"Id",bson,bson3,date,Some(date2),true,Some("v01"),true,Some("un com"))
      val firmware=Firmware(bson3,"firm","v02")
      val routeCall=mock[Call]

      f.applyFoundFunction()
      f.cardDaoMock.findById(org.mockito.Matchers.eq(bson2))(any[ExecutionContext]) returns future{Some(card)}
      f.firmwareDaoMock.findById(org.mockito.Matchers.eq(bson3))(any[ExecutionContext]) returns future{Some(firmware)}
      f.cardDaoMock.findApolline() returns future{Stream[BSONDocument]()}
      f.firmwareDaoMock.findFirmware() returns future{Stream[BSONDocument]()}
      f.firmwareDaoMock.findVersionFirmware() returns future{Stream[BSONDocument]()}

      val req=FakeRequest(GET,"/inventary/cards/"+bson.stringify+"/"+bson2.stringify).withSession("user" -> """{"login":"test"}""")
      val r=f.controller.cardUpdatePage(bson.stringify,bson2.stringify).apply(req)

      status(r) must equalTo(OK)
      contentType(r) must beSome.which(_ == "text/html")
      val content = contentAsString(r)
      content must contain("<title>Inventaire des cartes</title>")
      content must contain("<input type=\"text\" id=\"id\" name=\"id\" value=\"Id\" class=\"form-control\"/>")
      content must contain("<input type=\"date\" id=\"acquisition\" name=\"acquisition\" value=\"2015-04-22\" class=\"form-control\"/>")
      content must contain("<input type=\"date\" id=\"firstUse\" name=\"firstUse\" value=\"2015-04-23\" class=\"form-control\"/>")
      content must contain("<input type=\"checkbox\" id=\"agregateur\" name=\"agregateur\" value=\"true\" checked=\"checked\" />")
      content must contain("<input type=\"text\" id=\"apolline\" name=\"apolline\" class=\"form-control\" value=\"v01\" autocomplete=\"off\" list=\"list_apolline\"/>")
      content must contain("<input type=\"text\" id=\"firmware\" name=\"firmware\" class=\"form-control\" value=\"firm\" autocomplete=\"off\" list=\"list_firmware\"/>")
      content must contain("<input type=\"text\" id=\"versionFirmware\" name=\"versionFirmware\" class=\"form-control\" value=\"v02\" autocomplete=\"off\" list=\"list_versionFirmware\"/>")
      content must contain("<input type=\"checkbox\" id=\"hs\" name=\"hs\" value=\"true\" checked=\"checked\" />")
      content must contain("<textarea id=\"commentaire\" name=\"commentaire\" class=\"form-control\">un com</textarea>")

      there was one(f.typeCardsManagerMock).doIfTypeCardsFound(org.mockito.Matchers.eq(bson))(any[TypeCards=>Future[Result]])(any[Unit=>Future[Result]])
      there was one(f.cardDaoMock).findById(org.mockito.Matchers.eq(bson2))(any[ExecutionContext])
      there was one(f.firmwareDaoMock).findById(org.mockito.Matchers.eq(bson3))(any[ExecutionContext])
      there was one(f.cardDaoMock).findApolline()
      there was one(f.firmwareDaoMock).findFirmware()
      there was one(f.firmwareDaoMock).findVersionFirmware()
    }

    "send redirect after update card" in new WithApplication{
      val f=fixture
      val data=Json.parse("""{"id":"Id","acquisition":"2015-04-22","firstUse":"2015-04-22","firmware":"firm","versionFirmware":"v01","commentaire":"un com","send":"Envoyer"}""")
      val lastError=mock[LastError]

      f.applyFoundFunction()
      f.cardDaoMock.findAll(any[JsObject],any[JsObject])(any[ExecutionContext]) returns future{List()}
      f.firmwareDaoMock.findOne(any[JsObject])(any[ExecutionContext]) returns future{Some(Firmware(bson3,"firm","v01"))}
      f.cardDaoMock.updateById(org.mockito.Matchers.eq(bson2), any[Cards],any[GetLastError])(any[Writes[Cards]],any[ExecutionContext]) returns future{lastError}

      val req=FakeRequest(POST,"/inventary/cards/"+bson.stringify+"/card").withJsonBody(data).withSession("user" -> """{"login":"test"}""")
      val r=f.controller.cardUpdate(bson.stringify,bson2.stringify).apply(req)

      status(r) must equalTo(SEE_OTHER)
      header("Location",r) must equalTo(Some("/inventary/cards/"+bson.stringify))

      there was one(f.typeCardsManagerMock).doIfTypeCardsFound(org.mockito.Matchers.eq(bson))(any[TypeCards=>Future[Result]])(any[Unit=>Future[Result]])
      there was one(f.cardDaoMock).findAll(any[JsObject],any[JsObject])(any[ExecutionContext])
      there was one(f.firmwareDaoMock).findOne(any[JsObject])(any[ExecutionContext])
      there was one(f.cardDaoMock).updateById(org.mockito.Matchers.eq(bson2), any[Cards],any[GetLastError])(any[Writes[Cards]],any[ExecutionContext])
    }

    "send 500 internal error if have mongoDB error when update card" in new WithApplication{
      val f=fixture
      val data=Json.parse("""{"id":"Id","acquisition":"2015-04-22","firstUse":"2015-04-22","firmware":"firm","versionFirmware":"v01","commentaire":"un com","send":"Envoyer"}""")
      val futureMock=mock[Future[LastError]]
      val throwable=mock[Throwable]

      f.applyFoundFunction()
      f.cardDaoMock.findAll(any[JsObject],any[JsObject])(any[ExecutionContext]) returns future{List()}
      f.firmwareDaoMock.findOne(any[JsObject])(any[ExecutionContext]) returns future{Some(Firmware(bson3,"firm","v01"))}
      f.cardDaoMock.updateById(org.mockito.Matchers.eq(bson2), any[Cards],any[GetLastError])(any[Writes[Cards]],any[ExecutionContext]) returns futureMock
      futureMock.map(any[LastError=>LastError])(any[ExecutionContext]) returns futureMock
      futureMock.recover(any[PartialFunction[Throwable,Result]])(any[ExecutionContext]) answers {value=>future{value.asInstanceOf[PartialFunction[Throwable,Result]](throwable)}}

      val req=FakeRequest(POST,"/inventary/cards/"+bson.stringify+"/card").withJsonBody(data).withSession("user" -> """{"login":"test"}""")
      val r=f.controller.cardUpdate(bson.stringify,bson2.stringify).apply(req)

      status(r) must equalTo(INTERNAL_SERVER_ERROR)

      there was one(f.typeCardsManagerMock).doIfTypeCardsFound(org.mockito.Matchers.eq(bson))(any[TypeCards=>Future[Result]])(any[Unit=>Future[Result]])
      there was one(f.cardDaoMock).findAll(any[JsObject],any[JsObject])(any[ExecutionContext])
      there was one(f.firmwareDaoMock).findOne(any[JsObject])(any[ExecutionContext])
      there was one(f.cardDaoMock).updateById(org.mockito.Matchers.eq(bson2), any[Cards],any[GetLastError])(any[Writes[Cards]],any[ExecutionContext])
      there was one(futureMock).map(any[LastError=>LastError])(any[ExecutionContext])
      there was one(futureMock).recover(any[PartialFunction[Throwable,Result]])(any[ExecutionContext])
    }
  }

  "When user is on the page /inventary/cards/:id/:id2/clone, CardsManager" should{
    "send 200 Ok page with a prefilled form" in new WithApplication{
      val f=fixture
      val card=Cards(bson2,"Id",bson,bson3,date,Some(date2),true,Some("v01"),true,Some("un com"))
      val firmware=Firmware(bson3,"firm","v02")
      val routeCall=mock[Call]

      f.applyFoundFunction()
      f.cardDaoMock.findById(org.mockito.Matchers.eq(bson2))(any[ExecutionContext]) returns future{Some(card)}
      f.firmwareDaoMock.findById(org.mockito.Matchers.eq(bson3))(any[ExecutionContext]) returns future{Some(firmware)}
      f.cardDaoMock.findApolline() returns future{Stream[BSONDocument]()}
      f.firmwareDaoMock.findFirmware() returns future{Stream[BSONDocument]()}
      f.firmwareDaoMock.findVersionFirmware() returns future{Stream[BSONDocument]()}

      val req=FakeRequest(GET,"/inventary/cards/"+bson.stringify+"/"+bson2.stringify).withSession("user" -> """{"login":"test"}""")
      val r=f.controller.cardClonePage(bson.stringify,bson2.stringify).apply(req)

      status(r) must equalTo(OK)
      contentType(r) must beSome.which(_ == "text/html")
      val content = contentAsString(r)
      content must contain("<title>Inventaire des cartes</title>")
      content must contain("<input type=\"text\" id=\"id\" name=\"id\" value=\"\" class=\"form-control\"/>")
      content must matchRegex("<input type=\"date\" id=\"acquisition\" name=\"acquisition\" value=\"\\d{4}-\\d{2}-\\d{2}\" class=\"form-control\"/>")
      content must contain("<input type=\"date\" id=\"firstUse\" name=\"firstUse\" value=\"\" class=\"form-control\"/>")
      content must contain("<input type=\"checkbox\" id=\"agregateur\" name=\"agregateur\" value=\"true\" checked=\"checked\" />")
      content must contain("<input type=\"text\" id=\"apolline\" name=\"apolline\" class=\"form-control\" value=\"v01\" autocomplete=\"off\" list=\"list_apolline\"/>")
      content must contain("<input type=\"text\" id=\"firmware\" name=\"firmware\" class=\"form-control\" value=\"firm\" autocomplete=\"off\" list=\"list_firmware\"/>")
      content must contain("<input type=\"text\" id=\"versionFirmware\" name=\"versionFirmware\" class=\"form-control\" value=\"v02\" autocomplete=\"off\" list=\"list_versionFirmware\"/>")
      content must contain("<input type=\"checkbox\" id=\"hs\" name=\"hs\" value=\"true\" checked=\"checked\" />")
      content must contain("<textarea id=\"commentaire\" name=\"commentaire\" class=\"form-control\">un com</textarea>")

      there was one(f.typeCardsManagerMock).doIfTypeCardsFound(org.mockito.Matchers.eq(bson))(any[TypeCards=>Future[Result]])(any[Unit=>Future[Result]])
      there was one(f.cardDaoMock).findById(org.mockito.Matchers.eq(bson2))(any[ExecutionContext])
      there was one(f.firmwareDaoMock).findById(org.mockito.Matchers.eq(bson3))(any[ExecutionContext])
      there was one(f.cardDaoMock).findApolline()
      there was one(f.firmwareDaoMock).findFirmware()
      there was one(f.firmwareDaoMock).findVersionFirmware()
    }
  }

  "When method updateWithDeleteColumn is called, CardsManager" should{
    "send 500 internal error if mongoDB error when find card" in new WithApplication{
      val f=fixture
      val futureMock=mock[Future[Option[Cards]]]
      val throwable=mock[Throwable]

      f.cardDaoMock.findOne(any[JsObject])(any[ExecutionContext]) returns futureMock
      futureMock.flatMap(any[Option[Cards]=>Future[Option[Cards]]])(any[ExecutionContext]) returns futureMock
      futureMock.recover(any[PartialFunction[Throwable,Result]])(any[ExecutionContext]) answers {value=>future{value.asInstanceOf[PartialFunction[Throwable,Result]](throwable)}}

      val r=f.controller.updateWithColumnDelete(bson.stringify,Json.obj(),true)

      status(r) must equalTo(INTERNAL_SERVER_ERROR)

      there was one(f.cardDaoMock).findOne(any[JsObject])(any[ExecutionContext])
      there was one(futureMock).flatMap(any[Option[Cards]=>Future[Option[Cards]]])(any[ExecutionContext])
      there was one(futureMock).recover(any[PartialFunction[Throwable,Result]])(any[ExecutionContext])
    }

    "send redirect if card not found" in new WithApplication{
      val f=fixture

      f.cardDaoMock.findOne(any[JsObject])(any[ExecutionContext]) returns future{None}

      val r=f.controller.updateWithColumnDelete(bson.stringify,Json.obj(),true)

      status(r) must equalTo(SEE_OTHER)
      header("Location",r) must equalTo(Some("/inventary/cards/"+bson.stringify))

      there was one(f.cardDaoMock).findOne(any[JsObject])(any[ExecutionContext])
    }

    "send redirect after delete card" in new WithApplication{
      val f=fixture
      val cardIn=Cards(bson2,"Id",bson,bson3,date,Some(date2),true,Some("v01"),true,Some("un com"))
      val cardOut=Cards(bson2,"Id",bson,bson3,date,Some(date2),true,Some("v01"),true,Some("un com"),true)
      val lastError=mock[LastError]

      f.cardDaoMock.findOne(any[JsObject])(any[ExecutionContext]) returns future{Some(cardIn)}
      f.cardDaoMock.updateById(org.mockito.Matchers.eq(bson2), org.mockito.Matchers.eq(cardOut),any[GetLastError])(any[Writes[Cards]],any[ExecutionContext]) returns future{lastError}

      val r=f.controller.updateWithColumnDelete(bson.stringify,Json.obj(),true)

      status(r) must equalTo(SEE_OTHER)
      header("Location",r) must equalTo(Some("/inventary/cards/"+bson.stringify))

      there was one(f.cardDaoMock).findOne(any[JsObject])(any[ExecutionContext])
      there was one(f.cardDaoMock).updateById(org.mockito.Matchers.eq(bson2), org.mockito.Matchers.eq(cardOut),any[GetLastError])(any[Writes[Cards]],any[ExecutionContext])
    }

    "send 500 internal error if mongoDB error when update card" in new WithApplication{
      val f=fixture
      val cardIn=Cards(bson2,"Id",bson,bson3,date,Some(date2),true,Some("v01"),true,Some("un com"))
      val cardOut=Cards(bson2,"Id",bson,bson3,date,Some(date2),true,Some("v01"),true,Some("un com"),true)
      val futureMock=mock[Future[LastError]]
      val throwable=mock[Throwable]

      f.cardDaoMock.findOne(any[JsObject])(any[ExecutionContext]) returns future{Some(cardIn)}
      f.cardDaoMock.updateById(org.mockito.Matchers.eq(bson2), org.mockito.Matchers.eq(cardOut),any[GetLastError])(any[Writes[Cards]],any[ExecutionContext]) returns futureMock
      futureMock.map(any[LastError=>LastError])(any[ExecutionContext]) returns futureMock
      futureMock.recover(any[PartialFunction[Throwable,Result]])(any[ExecutionContext]) answers {value=>future{value.asInstanceOf[PartialFunction[Throwable,Result]](throwable)}}

      val r=f.controller.updateWithColumnDelete(bson.stringify,Json.obj(),true)

      status(r) must equalTo(INTERNAL_SERVER_ERROR)

      there was one(f.cardDaoMock).findOne(any[JsObject])(any[ExecutionContext])
      there was one(f.cardDaoMock).updateById(org.mockito.Matchers.eq(bson2), org.mockito.Matchers.eq(cardOut),any[GetLastError])(any[Writes[Cards]],any[ExecutionContext])
      there was one(futureMock).map(any[LastError=>LastError])(any[ExecutionContext])
      there was one(futureMock).recover(any[PartialFunction[Throwable,Result]])(any[ExecutionContext])
    }
  }

  "When user is on the page /inventary/cards/:id/:id2/delete, CardsManager" should{
    "send bad_request with the form if card type not exist" in new WithApplication{
      val f=fixture

      f.applyNotFoundFunction()

      val req=FakeRequest(GET,"/inventary/cards/"+bson.stringify+"/"+bson2.stringify+"/delete").withSession("user" -> """{"login":"test"}""")
      val r=f.controller.delete(bson.stringify,bson2.stringify).apply(req)

      status(r) must equalTo(SEE_OTHER)
      header("Location",r) must equalTo(Some("/inventary/cards"))

      there was one(f.typeCardsManagerMock).doIfTypeCardsFound(org.mockito.Matchers.eq(bson))(any[TypeCards=>Future[Result]])(any[Unit=>Future[Result]])
    }

    "send redirect after delete card" in new WithApplication{
      val f=fixture
      val cardIn=Cards(bson2,"Id",bson,bson3,date,Some(date2),true,Some("v01"),true,Some("un com"))
      val cardOut=Cards(bson2,"Id",bson,bson3,date,Some(date2),true,Some("v01"),true,Some("un com"),true)
      val lastError=mock[LastError]

      f.applyFoundFunction()
      f.cardDaoMock.findOne(any[JsObject])(any[ExecutionContext]) returns future{Some(cardIn)}
      f.cardDaoMock.updateById(org.mockito.Matchers.eq(bson2), org.mockito.Matchers.eq(cardOut),any[GetLastError])(any[Writes[Cards]],any[ExecutionContext]) returns future{lastError}

      val req=FakeRequest(GET,"/inventary/cards/"+bson.stringify+"/"+bson2.stringify+"/delete").withSession("user" -> """{"login":"test"}""")
      val r=f.controller.delete(bson.stringify,bson2.stringify).apply(req)

      status(r) must equalTo(SEE_OTHER)
      header("Location",r) must equalTo(Some("/inventary/cards/"+bson.stringify))

      there was one(f.typeCardsManagerMock).doIfTypeCardsFound(org.mockito.Matchers.eq(bson))(any[TypeCards=>Future[Result]])(any[Unit=>Future[Result]])
      there was one(f.cardDaoMock).findOne(any[JsObject])(any[ExecutionContext])
      there was one(f.cardDaoMock).updateById(org.mockito.Matchers.eq(bson2), org.mockito.Matchers.eq(cardOut),any[GetLastError])(any[Writes[Cards]],any[ExecutionContext])
    }
  }

  "When method getPreviousPage is called, CardsManager" should{
    "return the url if the url is found and match with the pattern" in new WithApplication{
      val f=fixture
      val url="http://hostname/inventary/cards?types=typ"

      val req=FakeRequest(GET,"url")
      val r=f.controller.getPreviousPage("http://hostname/",Some(url))(req)

      r must equalTo(url)
    }

    "return the url in session if the url is found and start with /inventary/cards/" in new WithApplication{
      val f=fixture
      val url="http://hostname/inventary/cards/"+bson.stringify
      val urlSession="http://hostname/inventary/cards"

      val req=FakeRequest(GET,"url").withSession("previous"->urlSession)
      val r=f.controller.getPreviousPage("http://hostname/",Some(url))(req)

      r must equalTo(urlSession)
    }

    "return the url to /inventary/cards if the url is found and not start with /inventary/cards/" in new WithApplication{
      val f=fixture
      val url="http://hostname/inventary/cards/"+bson.stringify

      val req=FakeRequest(GET,"url")
      val r=f.controller.getPreviousPage("http://hostname/",Some(url))(req)

      r must equalTo("/inventary/cards")
    }

    "return the url to /inventary/cards if the url is not found" in new WithApplication{
      val f=fixture

      val req=FakeRequest(GET,"url")
      val r=f.controller.getPreviousPage("http://hostname/",None)(req)

      r must equalTo("/inventary/cards")
    }

    "return the url to /inventary/cards if the url is not start with the host name" in new WithApplication{
      val f=fixture
      val url="http://hostname2/inventary/cards"

      val req=FakeRequest(GET,"url")
      val r=f.controller.getPreviousPage("http://hostname/",Some(url))(req)

      r must equalTo("/inventary/cards")
    }
  }

  "When method previousPage is called, CardsManager" should{
    "return the url if the hostname and the url are found and the url match with the pattern" in new WithApplication{
      val f=fixture
      val url="http://hostname/inventary/cards?types=typ"

      f.configMock.getString(org.mockito.Matchers.eq("hostname"),any[Option[Set[String]]]) returns Some("http://hostname/")

      val req=FakeRequest(GET,"url").withHeaders(("Referer"->url))
      val r=f.controller.previousPage(req)

      r must equalTo(url)

      there was one(f.configMock).getString(org.mockito.Matchers.eq("hostname"),any[Option[Set[String]]])
    }

    "throw an exception if the hostname not found" in new WithApplication{
      val f=fixture

      f.configMock.getString(org.mockito.Matchers.eq("hostname"),any[Option[Set[String]]]) returns None

      val req=FakeRequest(GET,"url")
      f.controller.previousPage(req) must throwA[Exception]

      there was one(f.configMock).getString(org.mockito.Matchers.eq("hostname"),any[Option[Set[String]]])
    }
  }
}