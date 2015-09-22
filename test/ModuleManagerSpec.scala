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
import play.api.libs.json._
import play.api.mvc.{Call, Results, Action, Result}
import play.api.test.Helpers._
import play.api.test.{FakeApplication, FakeRequest, WithApplication}
import reactivemongo.bson.{BSONDocument, BSONObjectID}
import reactivemongo.core.commands.{LastError, GetLastError}
import play.modules.reactivemongo.json.BSONFormats.BSONObjectIDFormat

import scala.collection.immutable.HashSet
import scala.concurrent._
import scala.concurrent.duration.Duration

@RunWith(classOf[JUnitRunner])
class ModuleManagerSpec extends Specification with Mockito {

  class ModuleManagerTest extends ModuleManagerLike

  case class matchRegex(a: String) extends Matcher[String]() {
    override def apply[S <: String](t: Expectable[S]): MatchResult[S] = result(a.r.findFirstIn(t.value).nonEmpty, "okMessage", t.value + "not found " + a, t)
  }

  case class contains(a: String, b: Int) extends Matcher[String]() {
    override def apply[S <: String](t: Expectable[S]): MatchResult[S] = result(a.r.findAllMatchIn(t.value).size == b, "okMessage", "not found " + b + " times but " + a.r.findAllMatchIn(t.value).size + " times " + a, t)
  }

  def fixture = new {
    val typeCardsManagerMock:TypeCardsManagerLike = mock[TypeCardsManagerLike]
    val cardsManagerMock:CardsManagerLike = mock[CardsManagerLike]
    val typeSensorManagerMock:TypeSensorManagerLike = mock[TypeSensorManagerLike]
    val sensorsManagerMock:SensorManagerLike=mock[SensorManagerLike]
    val moduleDaoMock: ModuleDao = mock[ModuleDao]
    val cardsDaoMock: CardsDao = mock[CardsDao]
    val typeCardsDaoMock: TypeCardsDao = mock[TypeCardsDao]
    val firmwareDaoMock: FirmwareDao = mock[FirmwareDao]
    val sensorsDaoMock: SensorDao = mock[SensorDao]
    val typeSensorsDaoMock: TypeSensorDao = mock[TypeSensorDao]
    val typeMesureDaoMock: TypeMesureDao = mock[TypeMesureDao]
    val configurationDaoMock:ConfigurationDao=mock[ConfigurationDao]
    val configMock=mock[Configuration]
    val conditionDaoMock:ConditionDao = mock[ConditionDao]
    val campaignDaoMock:CampagneDao=mock[CampagneDao]
    val localisationDaoMock:LocalisationDao=mock[LocalisationDao]
    val especeDaoMock:EspeceDao=mock[EspeceDao]
    val controller=new ModuleManagerTest{
      override val typeCardsManager:TypeCardsManagerLike = typeCardsManagerMock
      override val cardsManager:CardsManagerLike = cardsManagerMock
      override val typeSensorManager:TypeSensorManagerLike = typeSensorManagerMock
      override val sensorsManager:SensorManagerLike = sensorsManagerMock
      override val moduleDao: ModuleDao = moduleDaoMock
      override val cardsDao: CardsDao = cardsDaoMock
      override val typeCardsDao: TypeCardsDao = typeCardsDaoMock
      override val firmwareDao: FirmwareDao = firmwareDaoMock
      override val sensorsDao: SensorDao = sensorsDaoMock
      override val typeSensorsDao: TypeSensorDao = typeSensorsDaoMock
      override val typeMesureDao: TypeMesureDao = typeMesureDaoMock
      override val configurationDao:ConfigurationDao=configurationDaoMock
      override val config=configMock
      override val conditionDao:ConditionDao=conditionDaoMock
      override val campaignDao:CampagneDao=campaignDaoMock
      override val localisationDao:LocalisationDao=localisationDaoMock
      override val especeDao:EspeceDao=especeDaoMock
    }
  }

  val bson=BSONObjectID.generate
  val bson2=BSONObjectID.generate
  val bson3=BSONObjectID.generate
  val bson4=BSONObjectID.generate

  val date=new Date(115, 3, 22)
  val date2=new Date(115,3,23)

  "When user is not connected, ModuleManager" should{
    "redirect to login for resource /inventary/modules" in new WithApplication {
      route(FakeRequest(GET, "/inventary/modules")).map(
        r => {
          status(r) must equalTo(SEE_OTHER)
          header("Location", r) must equalTo(Some("/login"))
        }
      ).getOrElse(
          failure("Pas de retour de la fonction")
      )
    }

    "redirect to login for resource /inventary/modules/:id" in new WithApplication {
      route(FakeRequest(GET, "/inventary/modules/"+bson.stringify)).map(
        r => {
          status(r) must equalTo(SEE_OTHER)
          header("Location", r) must equalTo(Some("/login"))
        }
      ).getOrElse(
          failure("Pas de retour de la fonction")
        )
    }

    "redirect to login for resource /inventary/modules/form" in new WithApplication {
      route(FakeRequest(GET, "/inventary/modules/form")).map(
        r => {
          status(r) must equalTo(SEE_OTHER)
          header("Location", r) must equalTo(Some("/login"))
        }
      ).getOrElse(
          failure("Pas de retour de la fonction")
      )
    }

    "redirect to login for resource /inventary/modules/form" in new WithApplication {
      route(FakeRequest(POST, "/inventary/modules/form")).map(
        r => {
          status(r) must equalTo(SEE_OTHER)
          header("Location", r) must equalTo(Some("/login"))
        }
      ).getOrElse(
          failure("Pas de retour de la fonction")
      )
    }

    "redirect to login for resource /inventary/modules/form/typeCards" in new WithApplication {
      route(FakeRequest(GET, "/inventary/modules/form/typeCards")).map(
        r => {
          status(r) must equalTo(SEE_OTHER)
          header("Location", r) must equalTo(Some("/login"))
        }
      ).getOrElse(
          failure("Pas de retour de la fonction")
      )
    }

    "redirect to login for resource /inventary/modules/form/cards/:id" in new WithApplication {
      route(FakeRequest(GET, "/inventary/modules/form/cards/"+bson.stringify)).map(
        r => {
          status(r) must equalTo(SEE_OTHER)
          header("Location", r) must equalTo(Some("/login"))
        }
      ).getOrElse(
          failure("Pas de retour de la fonction")
      )
    }

    "redirect to login for resource /inventary/modules/form/cards/:id" in new WithApplication {
      route(FakeRequest(POST, "/inventary/modules/form/cards/"+bson.stringify)).map(
        r => {
          status(r) must equalTo(SEE_OTHER)
          header("Location", r) must equalTo(Some("/login"))
        }
      ).getOrElse(
          failure("Pas de retour de la fonction")
      )
    }

    "redirect to login for resource /inventary/modules/form/typeSensors" in new WithApplication {
      route(FakeRequest(GET, "/inventary/modules/form/typeSensors")).map(
        r => {
          status(r) must equalTo(SEE_OTHER)
          header("Location", r) must equalTo(Some("/login"))
        }
      ).getOrElse(
          failure("Pas de retour de la fonction")
      )
    }

    "redirect to login for resource /inventary/modules/form/sensors/:id" in new WithApplication {
      route(FakeRequest(GET, "/inventary/modules/form/sensors/"+bson.stringify)).map(
        r => {
          status(r) must equalTo(SEE_OTHER)
          header("Location", r) must equalTo(Some("/login"))
        }
      ).getOrElse(
          failure("Pas de retour de la fonction")
      )
    }

    "redirect to login for resource /inventary/modules/form/sensors/:id" in new WithApplication {
      route(FakeRequest(POST, "/inventary/modules/form/sensors/"+bson.stringify)).map(
        r => {
          status(r) must equalTo(SEE_OTHER)
          header("Location", r) must equalTo(Some("/login"))
        }
      ).getOrElse(
          failure("Pas de retour de la fonction")
      )
    }

    "redirect to login for resource /inventary/modules/form/validate" in new WithApplication {
      route(FakeRequest(GET, "/inventary/modules/form/validate")).map(
        r => {
          status(r) must equalTo(SEE_OTHER)
          header("Location", r) must equalTo(Some("/login"))
        }
      ).getOrElse(
          failure("Pas de retour de la fonction")
      )
    }

    "redirect to login for resource /inventary/modules/form/validate" in new WithApplication {
      route(FakeRequest(GET, "/inventary/modules/form/validate")).map(
        r => {
          status(r) must equalTo(SEE_OTHER)
          header("Location", r) must equalTo(Some("/login"))
        }
      ).getOrElse(
          failure("Pas de retour de la fonction")
        )
    }
  }

  "When user is on resource /inventary/modules, ModuleManager" should{
    "send 200 Ok with the message 'Aucun résultat trouvé'" in new WithApplication{
      val f=fixture

      f.moduleDaoMock.findAll(any[JsObject],any[JsObject])(any[ExecutionContext]) returns future{List[Module]()}
      f.moduleDaoMock.findListType() returns future{Stream[BSONDocument]()}

      val r=f.controller.inventary().apply(FakeRequest(GET,"/inventary/modules").withSession("user" -> """{"login":"test"}"""))

      status(r) must equalTo(OK)
      contentAsString(r) must contain("<h3 style=\"text-align:center\">Aucun résultat trouvé</h3>")

      there was one(f.moduleDaoMock).findAll(any[JsObject],any[JsObject])(any[ExecutionContext])
      there was one(f.moduleDaoMock).findListType()
    }

    "send 200 Ok with 1 result" in new WithApplication{
      val f=fixture
      val modules=List(Module(bson,"id","types",date,List(bson),List(bson),Some("un com")))

      f.moduleDaoMock.findAll(any[JsObject],any[JsObject])(any[ExecutionContext]) returns future{modules}
      f.moduleDaoMock.findListType() returns future{Stream[BSONDocument]()}

      val r=f.controller.inventary().apply(FakeRequest(GET,"/inventary/modules").withSession("user" -> """{"login":"test"}"""))

      status(r) must equalTo(OK)
      val content=contentAsString(r)
      content must not contain("<h3 style=\"text-align:center\">Aucun résultat trouvé</h3>")
      content must contain("types")
      content must contain("id")
      content must contain("<div class=\"row\"><span class=\"bold\">Commentaires</span> : un com</div>")
      content must contain("<div class=\"row\"><span class=\"bold\">Date d&#x27;assemblage</span> : 22/04/2015</div>")
      content must contain("<div class=\"row\"><span class=\"bold\">Cartes</span> : 1</div>")
      content must contain("<div class=\"row\"><span class=\"bold\">Capteurs</span> : 1</div>")

      there was one(f.moduleDaoMock).findAll(any[JsObject],any[JsObject])(any[ExecutionContext])
      there was one(f.moduleDaoMock).findListType()
    }

    "send 200 Ok with 2 result" in new WithApplication{
      val f=fixture
      val modules=List(Module(bson,"id","types",date,List(bson),List(bson),Some("un com")),Module(bson,"id2","types2",date2,List(bson,bson2),List(bson,bson2,bson3),Some("un com2")))

      f.moduleDaoMock.findAll(any[JsObject],any[JsObject])(any[ExecutionContext]) returns future{modules}
      f.moduleDaoMock.findListType() returns future{Stream[BSONDocument]()}

      val r=f.controller.inventary().apply(FakeRequest(GET,"/inventary/modules").withSession("user" -> """{"login":"test"}"""))

      status(r) must equalTo(OK)
      val content=contentAsString(r)
      content must not contain("<h3 style=\"text-align:center\">Aucun résultat trouvé</h3>")
      content must contain("types")
      content must contain("id")
      content must contain("<div class=\"row\"><span class=\"bold\">Commentaires</span> : un com</div>")
      content must contain("<div class=\"row\"><span class=\"bold\">Date d&#x27;assemblage</span> : 22/04/2015</div>")
      content must contain("<div class=\"row\"><span class=\"bold\">Cartes</span> : 1</div>")
      content must contain("<div class=\"row\"><span class=\"bold\">Capteurs</span> : 1</div>")

      content must contain("types2")
      content must contain("id2")
      content must contain("<div class=\"row\"><span class=\"bold\">Commentaires</span> : un com2</div>")
      content must contain("<div class=\"row\"><span class=\"bold\">Date d&#x27;assemblage</span> : 23/04/2015</div>")
      content must contain("<div class=\"row\"><span class=\"bold\">Cartes</span> : 2</div>")
      content must contain("<div class=\"row\"><span class=\"bold\">Capteurs</span> : 3</div>")

      there was one(f.moduleDaoMock).findAll(any[JsObject],any[JsObject])(any[ExecutionContext])
      there was one(f.moduleDaoMock).findListType()
    }
  }

  "When user is on resource /inventary/modules/:id, ModuleManager" should{
    "send redirect if module not found" in new WithApplication{
      val f=fixture

      f.moduleDaoMock.findOne(org.mockito.Matchers.eq(Json.obj("_id"->bson,"delete"->false)))(any[ExecutionContext]) returns future{None}

      val r=f.controller.moreInformation(bson.stringify).apply(FakeRequest(GET,"/inventary/modules/"+bson.stringify).withSession("user" -> """{"login":"test"}"""))

      status(r) must equalTo(SEE_OTHER)
      header("Location",r) must beSome("/inventary/modules")

      there was one(f.moduleDaoMock).findOne(org.mockito.Matchers.eq(Json.obj("_id"->bson,"delete"->false)))(any[ExecutionContext])
    }

    "send 200 Ok with 1 result" in new WithApplication{
      val f=fixture
      val matcher=Json.obj("delete"->false,"_id"->Json.obj("$in"->JsArray(Seq(BSONObjectIDFormat.writes(bson)))))
      val matcher2=Json.obj("delete"->false,"_id"->Json.obj("$in"->JsArray(Seq(BSONObjectIDFormat.writes(bson2)))))
      val typeSensors=TypeSensor(bson2,"typeSensor","modeleSensor","fab",1,List(bson4),false)
      val typeMesure=TypeMesure(bson3,"tension","volt")
      val sensor=Sensor(bson, "Id", bson2, Some(date), date2, Some(date2), true, Some("un com"))
      val typeCards=TypeCards(bson2,"modeleCards","typeCards",false)
      val firmware=Firmware(bson3,"firm","version")
      val cards=Cards(bson, "Id2", bson2, bson3, date, Some(date2), false, Some("v03"), false, Some("un com"))
      val module=Module(bson4,"id","types",date,List(bson),List(bson),Some("un com"))
      val url="http://hostname/inventary/modules?types=typ"
      val config=List(models.Configuration(port="/dev/ttyUSB0",types="ADC",timeFilter=9000,infoMesure=List(bson3),numberOfValue=6),models.Configuration(port="/dev/ttyUSB1",types="wasp",infoMesure=List(bson4),numberOfValue=12))

      f.configMock.getString(org.mockito.Matchers.eq("hostname"),any[Option[Set[String]]]) returns Some("http://hostname/")
      f.sensorsDaoMock.fold(org.mockito.Matchers.eq(matcher),any[JsObject],org.mockito.Matchers.eq(HashSet[BSONObjectID]()))(any[(HashSet[BSONObjectID],Sensor)=>HashSet[BSONObjectID]])(any[ExecutionContext]) returns future{HashSet(bson2)}
      f.typeSensorsDaoMock.findAll(org.mockito.Matchers.eq(matcher2),any[JsObject])(any[ExecutionContext]) returns future{List(typeSensors)}
      f.typeMesureDaoMock.findAll(any[JsObject],any[JsObject])(any[ExecutionContext]) returns future{List(typeMesure)}
      f.especeDaoMock.findAll(org.mockito.Matchers.eq(Json.obj("_id"->Json.obj("$in"->List(bson4)))),any[JsObject])(any[ExecutionContext]) returns future{List(Espece(bson4,"esp1",bson3,2.3f,3.4f))}
      f.sensorsDaoMock.findAll(org.mockito.Matchers.eq(matcher),any[JsObject])(any[ExecutionContext]) returns future{List(sensor)}
      f.cardsDaoMock.fold(org.mockito.Matchers.eq(matcher),any[JsObject],org.mockito.Matchers.eq(HashSet[BSONObjectID]()))(any[(HashSet[BSONObjectID],Cards)=>HashSet[BSONObjectID]])(any[ExecutionContext]) returns future{HashSet(bson2)}
      f.typeCardsDaoMock.findAll(org.mockito.Matchers.eq(matcher2),any[JsObject])(any[ExecutionContext]) returns future{List(typeCards)}
      f.firmwareDaoMock.findAll(any[JsObject],any[JsObject])(any[ExecutionContext]) returns future{List(firmware)}
      f.cardsDaoMock.findAll(org.mockito.Matchers.eq(matcher),any[JsObject])(any[ExecutionContext]) returns future{List(cards)}
      f.moduleDaoMock.findOne(org.mockito.Matchers.eq(Json.obj("delete"->false,"_id"->bson4)))(any[ExecutionContext]) returns future{Some(module)}
      f.conditionDaoMock.findModulesState(org.mockito.Matchers.eq(List(bson4))) returns future{Map(bson4->"Terrain")}
      f.configurationDaoMock.findAll(any[JsObject],any[JsObject])(any[ExecutionContext]) returns future{config}

      val req=FakeRequest("GET","/inventary/modules/"+bson4.stringify).withHeaders(("Referer"->url)).withSession("user" -> """{"login":"test"}""")
      val r=f.controller.moreInformation(bson4.stringify).apply(req)

      status(r) must equalTo(OK)
      val content=contentAsString(r)
      content must not contain("<div class=\"alert alert-danger\" role=\"alert\">Attention certaines cartes ou capteurs ne sont plus disponibles</div>")
      content must contain("<h4>types / id</h4>")
      content must contain("<div class=\"row\"><span class=\"bold\">Commentaires</span> : un com</div>")
      content must contain("<div class=\"row\"><span class=\"bold\">Date d&#x27;assemblage</span> : 22/04/2015</div>")
      content must contain("<h5 class=\"bold\">typeCards / modeleCards</h5>")
      content must contain("<td>Id2</td>")
      content must contains("<td>22/04/2015</td>",2)
      content must contains("<td>23/04/2015</td>",3)
      content must contain("<td>firm (version)</td>")
      content must contain("<td> Non </td>")
      content must contain("<td>v03</td>")
      content must contain("<h5 class=\"bold\">typeSensor / modeleSensor</h5>")
      content must contain("<div class=\"row\"> <span class=\"bold\">Fabricant</span> : fab</div>")
      content must contain("<div class=\"row\"> <span class=\"bold\">Nombre de signaux</span> : 1</div>")
      content must contain("<td>esp1</td>")
      content must contain("<td>tension</td>")
      content must contain("<td>2.3 volt</td>")
      content must contain("<td>3.4 volt</td>")
      content must contain("<td>Id</td>")
      content must contain("<td>Hors service<br/>Terrain</td>")
      content must contain("<td>/dev/ttyUSB0</td>")
      content must contain("<td>/dev/ttyUSB1</td>")
      content must contain("<td>ADC</td>")
      content must contain("<td>wasp</td>")
      content must contain("<td>9000</td>")
      content must contain("<td>1000</td>")
      content must contain("<td>6</td>")
      content must contain("<td>12</td>")
      session(r).get("previous") must beSome(url)

      there was one(f.sensorsDaoMock).fold(org.mockito.Matchers.eq(matcher),any[JsObject],org.mockito.Matchers.eq(HashSet[BSONObjectID]()))(any[(HashSet[BSONObjectID],Sensor)=>HashSet[BSONObjectID]])(any[ExecutionContext])
      there was one(f.typeSensorsDaoMock).findAll(org.mockito.Matchers.eq(matcher2),any[JsObject])(any[ExecutionContext])
      there was one(f.typeMesureDaoMock).findAll(any[JsObject],any[JsObject])(any[ExecutionContext])
      there was one(f.sensorsDaoMock).findAll(org.mockito.Matchers.eq(matcher),any[JsObject])(any[ExecutionContext])
      there was one(f.cardsDaoMock).fold(org.mockito.Matchers.eq(matcher),any[JsObject],org.mockito.Matchers.eq(HashSet[BSONObjectID]()))(any[(HashSet[BSONObjectID],Cards)=>HashSet[BSONObjectID]])(any[ExecutionContext])
      there was one(f.typeCardsDaoMock).findAll(org.mockito.Matchers.eq(matcher2),any[JsObject])(any[ExecutionContext])
      there was one(f.firmwareDaoMock).findAll(any[JsObject],any[JsObject])(any[ExecutionContext])
      there was one(f.cardsDaoMock).findAll(org.mockito.Matchers.eq(matcher),any[JsObject])(any[ExecutionContext])
      there was one(f.moduleDaoMock).findOne(org.mockito.Matchers.eq(Json.obj("delete"->false,"_id"->bson4)))(any[ExecutionContext])
      there was one(f.configMock).getString(org.mockito.Matchers.eq("hostname"),any[Option[Set[String]]])
      there was one(f.configurationDaoMock).findAll(any[JsObject],any[JsObject])(any[ExecutionContext])
      there was one(f.especeDaoMock).findAll(org.mockito.Matchers.eq(Json.obj("_id"->Json.obj("$in"->List(bson4)))),any[JsObject])(any[ExecutionContext])
    }
  }

  "When user is on resource /inventary/modules/form/typeCards, ModuleManager" should{
    "send 200 on OK with the message 'Aucun résultat trouvé'" in new WithApplication {
      val f=fixture
      val module=Module(bson,"id","types",date,List(bson2),List(bson3),Some("un com"))
      val listCards=JsArray(Seq(BSONObjectIDFormat.writes(bson4)))

      f.moduleDaoMock.fold(org.mockito.Matchers.eq(Json.obj("delete"->false,"_id"->Json.obj("$ne"->bson))),any[JsObject],org.mockito.Matchers.eq(module.cartes))(any[(List[BSONObjectID],Module)=>List[BSONObjectID]])(any[ExecutionContext]) returns future{List[BSONObjectID](bson4)}
      f.cardsDaoMock.fold(org.mockito.Matchers.eq(Json.obj("delete"->false,"_id"->Json.obj("$nin"->listCards))),org.mockito.Matchers.eq(Json.obj()),org.mockito.Matchers.eq(HashSet[JsValue]()))(any[(HashSet[JsValue],Cards)=>HashSet[JsValue]])(any[ExecutionContext]) returns future{HashSet[JsValue]()}
      f.typeCardsManagerMock.getInventaryTypeCards(any[JsObject],anyString,anyString)(any[(List[TypeCards],List[BSONDocument],List[BSONDocument],List[(BSONObjectID,Int)])=>Result]) answers { (params, _) => params match {
        case Array(_,_,_, p: ((List[TypeCards],List[BSONDocument],List[BSONDocument],List[(BSONObjectID,Int)])=>Result))=>future{p.apply(List[TypeCards](),List[BSONDocument](),List[BSONDocument](),List[(BSONObjectID,Int)]())}
      }}

      val r = f.controller.formTypeCards().apply(FakeRequest(GET, "/inventary/modules/form/typeCards").withSession("user" -> """{"login":"test"}""","module"->Module.toStrings(module)))
      status(r) must equalTo(OK)
      contentType(r) must beSome.which(_ == "text/html")
      val content = contentAsString(r)
      content must contain("<title>Inventaire des modules</title>")
      content must contain("<h3 style=\"text-align:center\">Aucun résultat trouvé</h3>")

      there was one(f.moduleDaoMock).fold(org.mockito.Matchers.eq(Json.obj("delete"->false,"_id"->Json.obj("$ne"->bson))),any[JsObject],org.mockito.Matchers.eq(module.cartes))(any[(List[BSONObjectID],Module)=>List[BSONObjectID]])(any[ExecutionContext])
      there was one(f.cardsDaoMock).fold(org.mockito.Matchers.eq(Json.obj("delete"->false,"_id"->Json.obj("$nin"->listCards))),org.mockito.Matchers.eq(Json.obj()),org.mockito.Matchers.eq(HashSet[JsValue]()))(any[(HashSet[JsValue],Cards)=>HashSet[JsValue]])(any[ExecutionContext])
      there was one(f.typeCardsManagerMock).getInventaryTypeCards(any[JsObject],anyString,any)(any[(List[TypeCards],List[BSONDocument],List[BSONDocument],List[(BSONObjectID,Int)])=>Result])
    }

    "send 200 on OK with 1 result" in new WithApplication {
      val f=fixture
      val module=Module(bson,"id","types",date,List(bson2),List(bson3),Some("un com"))
      val listCards=JsArray(Seq(BSONObjectIDFormat.writes(bson4)))

      f.moduleDaoMock.fold(org.mockito.Matchers.eq(Json.obj("delete"->false,"_id"->Json.obj("$ne"->bson))),any[JsObject],org.mockito.Matchers.eq(module.cartes))(any[(List[BSONObjectID],Module)=>List[BSONObjectID]])(any[ExecutionContext]) returns future{List[BSONObjectID](bson4)}
      f.cardsDaoMock.fold(org.mockito.Matchers.eq(Json.obj("delete"->false,"_id"->Json.obj("$nin"->listCards))),org.mockito.Matchers.eq(Json.obj()),org.mockito.Matchers.eq(HashSet[JsValue]()))(any[(HashSet[JsValue],Cards)=>HashSet[JsValue]])(any[ExecutionContext]) returns future{HashSet[JsValue]()}
      f.typeCardsManagerMock.getInventaryTypeCards(any[JsObject],anyString,anyString)(any[(List[TypeCards],List[BSONDocument],List[BSONDocument],List[(BSONObjectID,Int)])=>Result]) answers { (params, _) => params match {
        case Array(_,_,_, p: ((List[TypeCards],List[BSONDocument],List[BSONDocument],List[(BSONObjectID,Int)])=>Result))=>future{p.apply(List[TypeCards](TypeCards(bson,"mod","type")),List[BSONDocument](),List[BSONDocument](BSONDocument("_id" -> bson, "count" -> 5)),List((bson,3)))}
      }}

      val r = f.controller.formTypeCards().apply(FakeRequest(GET, "/inventary/modules/form/typeCards").withSession("user" -> """{"login":"test"}""","module"->Module.toStrings(module)))
      status(r) must equalTo(OK)
      contentType(r) must beSome.which(_ == "text/html")
      val content = contentAsString(r)
      content must contain("<title>Inventaire des modules</title>")
      content must not contain("<h3 style=\"text-align:center\">Aucun résultat trouvé</h3>")
      content must contain("type")
      content must contain("mod")
      content must matchRegex("<span class=\"bold\">\\s*Stock\\s*</span>\\s*:\\s*2 / 5")

      there was one(f.moduleDaoMock).fold(org.mockito.Matchers.eq(Json.obj("delete"->false,"_id"->Json.obj("$ne"->bson))),any[JsObject],org.mockito.Matchers.eq(module.cartes))(any[(List[BSONObjectID],Module)=>List[BSONObjectID]])(any[ExecutionContext])
      there was one(f.cardsDaoMock).fold(org.mockito.Matchers.eq(Json.obj("delete"->false,"_id"->Json.obj("$nin"->listCards))),org.mockito.Matchers.eq(Json.obj()),org.mockito.Matchers.eq(HashSet[JsValue]()))(any[(HashSet[JsValue],Cards)=>HashSet[JsValue]])(any[ExecutionContext])
      there was one(f.typeCardsManagerMock).getInventaryTypeCards(any[JsObject],anyString,anyString)(any[(List[TypeCards],List[BSONDocument],List[BSONDocument],List[(BSONObjectID,Int)])=>Result])
    }

    "send 200 on OK with 2 results" in new WithApplication {
      val f=fixture
      val module=Module(bson,"id","types",date,List(bson2),List(bson3),Some("un com"))
      val listCards=JsArray(Seq(BSONObjectIDFormat.writes(bson4)))

      f.moduleDaoMock.fold(org.mockito.Matchers.eq(Json.obj("delete"->false,"_id"->Json.obj("$ne"->bson))),any[JsObject],org.mockito.Matchers.eq(module.cartes))(any[(List[BSONObjectID],Module)=>List[BSONObjectID]])(any[ExecutionContext]) returns future{List[BSONObjectID](bson4)}
      f.cardsDaoMock.fold(org.mockito.Matchers.eq(Json.obj("delete"->false,"_id"->Json.obj("$nin"->listCards))),org.mockito.Matchers.eq(Json.obj()),org.mockito.Matchers.eq(HashSet[JsValue]()))(any[(HashSet[JsValue],Cards)=>HashSet[JsValue]])(any[ExecutionContext]) returns future{HashSet[JsValue]()}
      f.typeCardsManagerMock.getInventaryTypeCards(any[JsObject],anyString,anyString)(any[(List[TypeCards],List[BSONDocument],List[BSONDocument],List[(BSONObjectID,Int)])=>Result]) answers { (params, _) => params match {
        case Array(_,_,_, p: ((List[TypeCards],List[BSONDocument],List[BSONDocument],List[(BSONObjectID,Int)])=>Result))=>future{p.apply(List[TypeCards](TypeCards(bson,"mod","type"),TypeCards(bson2,"mod2","type2")),List[BSONDocument](),List[BSONDocument](BSONDocument("_id" -> bson, "count" -> 5),BSONDocument("_id" -> bson2, "count" -> 3)),List((bson,3),(bson2,2)))}
      }}

      val r = f.controller.formTypeCards().apply(FakeRequest(GET, "/inventary/modules/form/typeCards").withSession("user" -> """{"login":"test"}""","module"->Module.toStrings(module)))

      status(r) must equalTo(OK)
      contentType(r) must beSome.which(_ == "text/html")
      val content = contentAsString(r)
      content must contain("<title>Inventaire des modules</title>")
      content must not contain("<h3 style=\"text-align:center\">Aucun résultat trouvé</h3>")
      content must contain("type")
      content must contain("mod")
      content must matchRegex("<span class=\"bold\">\\s*Stock\\s*</span>\\s*:\\s*2 / 5")

      content must contain("type2")
      content must contain("mod2")
      content must matchRegex("<span class=\"bold\">\\s*Stock\\s*</span>\\s*:\\s*1 / 3")

      there was one(f.moduleDaoMock).fold(org.mockito.Matchers.eq(Json.obj("delete"->false,"_id"->Json.obj("$ne"->bson))),any[JsObject],org.mockito.Matchers.eq(module.cartes))(any[(List[BSONObjectID],Module)=>List[BSONObjectID]])(any[ExecutionContext])
      there was one(f.cardsDaoMock).fold(org.mockito.Matchers.eq(Json.obj("delete"->false,"_id"->Json.obj("$nin"->listCards))),org.mockito.Matchers.eq(Json.obj()),org.mockito.Matchers.eq(HashSet[JsValue]()))(any[(HashSet[JsValue],Cards)=>HashSet[JsValue]])(any[ExecutionContext])
      there was one(f.typeCardsManagerMock).getInventaryTypeCards(any[JsObject],anyString,anyString)(any[(List[TypeCards],List[BSONDocument],List[BSONDocument],List[(BSONObjectID,Int)])=>Result])
    }
  }

  "When user is on resource /inventary/modules/form/cards/:id, ModuleManager" should{
    "send 200 on OK with the message 'Aucun résultat trouvé'" in new WithApplication {
      val f=fixture
      val module=Module(bson,"id","types",date,List(bson2),List(bson3),Some("un com"))
      val listCards=JsArray(Seq(BSONObjectIDFormat.writes(bson4)))

      f.moduleDaoMock.fold(org.mockito.Matchers.eq(Json.obj("delete"->false,"_id"->Json.obj("$ne"->bson))),any[JsObject],org.mockito.Matchers.eq(module.cartes))(any[(List[BSONObjectID],Module)=>List[BSONObjectID]])(any[ExecutionContext]) returns future{List[BSONObjectID](bson4)}
      f.cardsManagerMock.getInventaryCards(any[JsObject],any[JsObject],any[BSONObjectID],any[Result])(any[(TypeCards,List[Cards],List[Firmware],Int,List[(BSONObjectID,Int)],Map[BSONObjectID,String])=>Result]) answers { (params, _) => params match {
        case Array(_,_,_,_, p: ((TypeCards,List[Cards],List[Firmware],Int,List[(BSONObjectID,Int)],Map[BSONObjectID,String])=>Result))=>future{p.apply(TypeCards(bson,"mod","type"),List[Cards](),List[Firmware](),0,List((bson,0)),Map())}
      }}

      val r = f.controller.formCards(bson.stringify).apply(FakeRequest(GET, "/inventary/modules/form/cards/"+bson.stringify).withSession("user" -> """{"login":"test"}""","module"->Module.toStrings(module)))
      status(r) must equalTo(OK)
      contentType(r) must beSome.which(_ == "text/html")
      val content = contentAsString(r)
      content must contain("<title>Inventaire des modules</title>")
      content must matchRegex("type\\s*/\\s*mod")
      content must contain("<h3 style=\"text-align:center\">Aucun résultat trouvé</h3>")
      content must matchRegex("<span class=\"bold\">\\s*Stock\\s*</span>\\s*:\\s*0 / 0")

      there was one(f.moduleDaoMock).fold(org.mockito.Matchers.eq(Json.obj("delete"->false,"_id"->Json.obj("$ne"->bson))),any[JsObject],org.mockito.Matchers.eq(module.cartes))(any[(List[BSONObjectID],Module)=>List[BSONObjectID]])(any[ExecutionContext])
      there was one(f.cardsManagerMock).getInventaryCards(any[JsObject],any[JsObject],any[BSONObjectID],any[Result])(any[(TypeCards,List[Cards],List[Firmware],Int,List[(BSONObjectID,Int)],Map[BSONObjectID,String])=>Result])
    }

    "send 200 on OK with 1 result" in new WithApplication {
      val f=fixture
      val typeCards=TypeCards(bson,"mod","type")
      val card=List(Cards(bson2, "Id", bson, bson3, date, None, true, Some("v01"), true, None))
      val firmware=List(Firmware(bson3, "firm", "v02"))
      val module=Module(bson,"id","types",date,List(bson2),List(bson3),Some("un com"))
      val listCards=JsArray(Seq(BSONObjectIDFormat.writes(bson4)))

      f.moduleDaoMock.fold(org.mockito.Matchers.eq(Json.obj("delete"->false,"_id"->Json.obj("$ne"->bson))),any[JsObject],org.mockito.Matchers.eq(module.cartes))(any[(List[BSONObjectID],Module)=>List[BSONObjectID]])(any[ExecutionContext]) returns future{List[BSONObjectID](bson4)}
      f.cardsManagerMock.getInventaryCards(any[JsObject],any[JsObject],any[BSONObjectID],any[Result])(any[(TypeCards,List[Cards],List[Firmware],Int,List[(BSONObjectID,Int)],Map[BSONObjectID,String])=>Result]) answers { (params, _) => params match {
        case Array(_,_,_,_, p: ((TypeCards,List[Cards],List[Firmware],Int,List[(BSONObjectID,Int)],Map[BSONObjectID,String])=>Result))=>future{p.apply(typeCards,card,firmware,1,List((bson,0)),Map(bson2->"Test"))}
      }}

      val r = f.controller.formCards(bson.stringify).apply(FakeRequest(GET, "/inventary/modules/form/cards/"+bson.stringify).withSession("user" -> """{"login":"test"}""","module"->Module.toStrings(module)))
      status(r) must equalTo(OK)
      contentType(r) must beSome.which(_ == "text/html")
      val content = contentAsString(r)
      content must contain("<title>Inventaire des modules</title>")
      content must not contain("<h3 style=\"text-align:center\">Aucun résultat trouvé</h3>")
      content must contain("<td>Id</td>")
      content must contain("<td>22/04/2015</td>")
      content must contain("<td>-</td>")
      content must contain("<td>Oui</td>")
      content must contain("<td>firm (v02)</td>")
      content must contain("<td>v01</td>")
      content must contain("<td>Hors service<br/>Test</td>")

      there was one(f.moduleDaoMock).fold(org.mockito.Matchers.eq(Json.obj("delete"->false,"_id"->Json.obj("$ne"->bson))),any[JsObject],org.mockito.Matchers.eq(module.cartes))(any[(List[BSONObjectID],Module)=>List[BSONObjectID]])(any[ExecutionContext])
      there was one(f.cardsManagerMock).getInventaryCards(any[JsObject],any[JsObject],any[BSONObjectID],any[Result])(any[(TypeCards,List[Cards],List[Firmware],Int,List[(BSONObjectID,Int)],Map[BSONObjectID,String])=>Result])
    }

    "send 200 on OK with 1 result for request with filter" in new WithApplication {
      val f=fixture
      val typeCards=TypeCards(bson,"mod","type")
      val card=List(Cards(bson2, "Id", bson, bson3, date, None, true, Some("v01"), true, None))
      val firmware=List(Firmware(bson3, "firm", "v02"))
      val module=Module(bson,"id","types",date,List(bson2),List(bson3),Some("un com"))
      val listCards=JsArray(Seq(BSONObjectIDFormat.writes(bson4)))

      f.moduleDaoMock.fold(org.mockito.Matchers.eq(Json.obj("delete"->false,"_id"->Json.obj("$ne"->bson))),any[JsObject],org.mockito.Matchers.eq(module.cartes))(any[(List[BSONObjectID],Module)=>List[BSONObjectID]])(any[ExecutionContext]) returns future{List[BSONObjectID](bson4)}
      f.cardsManagerMock.getInventaryCards(org.mockito.Matchers.eq(Json.obj("delete"->false,"types"->bson,"_id"->Json.obj("$nin"->listCards),"id"->Json.obj("$regex"->".*val.*"))),any[JsObject],any[BSONObjectID],any[Result])(any[(TypeCards,List[Cards],List[Firmware],Int,List[(BSONObjectID,Int)],Map[BSONObjectID,String])=>Result]) answers { (params, _) => params match {
        case Array(_,_,_,_, p: ((TypeCards,List[Cards],List[Firmware],Int,List[(BSONObjectID,Int)],Map[BSONObjectID,String])=>Result))=>future{p.apply(typeCards,card,firmware,1,List((bson,0)),Map(bson2->"Test"))}
      }}

      val r = f.controller.formCards(bson.stringify,"id",1,"val").apply(FakeRequest(GET, "/inventary/modules/form/cards/"+bson.stringify).withSession("user" -> """{"login":"test"}""","module"->Module.toStrings(module)))
      status(r) must equalTo(OK)
      contentType(r) must beSome.which(_ == "text/html")
      val content = contentAsString(r)
      content must contain("<title>Inventaire des modules</title>")
      content must not contain("<h3 style=\"text-align:center\">Aucun résultat trouvé</h3>")
      content must contain("<td>Id</td>")
      content must contain("<td>22/04/2015</td>")
      content must contain("<td>-</td>")
      content must contain("<td>Oui</td>")
      content must contain("<td>firm (v02)</td>")
      content must contain("<td>v01</td>")
      content must contain("<td>Hors service<br/>Test</td>")

      there was one(f.moduleDaoMock).fold(org.mockito.Matchers.eq(Json.obj("delete"->false,"_id"->Json.obj("$ne"->bson))),any[JsObject],org.mockito.Matchers.eq(module.cartes))(any[(List[BSONObjectID],Module)=>List[BSONObjectID]])(any[ExecutionContext])
      there was one(f.cardsManagerMock).getInventaryCards(org.mockito.Matchers.eq(Json.obj("delete"->false,"types"->bson,"_id"->Json.obj("$nin"->listCards),"id"->Json.obj("$regex"->".*val.*"))),any[JsObject],any[BSONObjectID],any[Result])(any[(TypeCards,List[Cards],List[Firmware],Int,List[(BSONObjectID,Int)],Map[BSONObjectID,String])=>Result])
    }

    "send 200 on OK with 2 results" in new WithApplication {
      val f=fixture
      val typeCards=TypeCards(bson,"mod","type")
      val card=List(Cards(bson2, "Id", bson, bson3, date, None, true, Some("v01"), true, None),Cards(bson4, "Id2", bson, bson3, date, Some(date2), false, Some("v03"), false, None))
      val firmware=List(Firmware(bson3, "firm", "v02"))
      val module=Module(bson,"id","types",date,List(bson2),List(bson3),Some("un com"))
      val listCards=JsArray(Seq(BSONObjectIDFormat.writes(bson4)))

      f.moduleDaoMock.fold(org.mockito.Matchers.eq(Json.obj("delete"->false,"_id"->Json.obj("$ne"->bson))),any[JsObject],org.mockito.Matchers.eq(module.cartes))(any[(List[BSONObjectID],Module)=>List[BSONObjectID]])(any[ExecutionContext]) returns future{List[BSONObjectID](bson4)}
      f.cardsManagerMock.getInventaryCards(any[JsObject],any[JsObject],any[BSONObjectID],any[Result])(any[(TypeCards,List[Cards],List[Firmware],Int,List[(BSONObjectID,Int)],Map[BSONObjectID,String])=>Result]) answers { (params, _) => params match {
        case Array(_,_,_,_, p: ((TypeCards,List[Cards],List[Firmware],Int,List[(BSONObjectID,Int)],Map[BSONObjectID,String])=>Result))=>future{p.apply(typeCards,card,firmware,2,List((bson,1)),Map(bson4->"Terrain"))}
      }}

      val r = f.controller.formCards(bson.stringify).apply(FakeRequest(GET, "/inventary/modules/form/cards/"+bson.stringify).withSession("user" -> """{"login":"test"}""","module"->Module.toStrings(module)))
      status(r) must equalTo(OK)
      contentType(r) must beSome.which(_ == "text/html")
      val content = contentAsString(r)
      content must contain("<title>Inventaire des modules</title>")
      content must not contain("<h3 style=\"text-align:center\">Aucun résultat trouvé</h3>")
      content must contain("<td>Id</td>")
      content must contain("<td>22/04/2015</td>")
      content must contain("<td>-</td>")
      content must contain("<td>Oui</td>")
      content must contain("<td>firm (v02)</td>")
      content must contain("<td>v01</td>")
      content must contain("<td>Hors service</td>")

      content must contain("<td>Id2</td>")
      content must contain("<td>22/04/2015</td>")
      content must contain("<td>23/04/2015</td>")
      content must contain("<td>Non</td>")
      content must contain("<td>firm (v02)</td>")
      content must contain("<td>v03</td>")
      content must contain("<td>Terrain</td>")

      there was one(f.moduleDaoMock).fold(org.mockito.Matchers.eq(Json.obj("delete"->false,"_id"->Json.obj("$ne"->bson))),any[JsObject],org.mockito.Matchers.eq(module.cartes))(any[(List[BSONObjectID],Module)=>List[BSONObjectID]])(any[ExecutionContext])
      there was one(f.cardsManagerMock).getInventaryCards(any[JsObject],any[JsObject],any[BSONObjectID],any[Result])(any[(TypeCards,List[Cards],List[Firmware],Int,List[(BSONObjectID,Int)],Map[BSONObjectID,String])=>Result])
    }

    "send bad request with list of cards if form submit have error" in new WithApplication{
      val f=fixture
      val typeCards=TypeCards(bson,"mod","type")
      val card=List(Cards(bson2, "Id", bson, bson3, date, None, true, Some("v01"), true, None))
      val firmware=List(Firmware(bson3, "firm", "v02"))
      val controller=new ModuleManagerTest{
        override val typeCardsManager:TypeCardsManagerLike = f.typeCardsManagerMock
        override val cardsManager:CardsManagerLike = f.cardsManagerMock
        override val selectElement=mock[Form[SelectInfo]]
      }

      val req=FakeRequest("POST","/inventary/modules/form/cards/"+bson.stringify).withSession("user" -> """{"login":"test"}""")

      f.typeCardsManagerMock.doIfTypeCardsFound(org.mockito.Matchers.eq(bson))(any[TypeCards => Future[Result]])(any[Unit => Future[Result]]) answers { (params, _) => params match {
        case Array(_,p: (TypeCards => Future[Result]),_) => p.apply(typeCards)
      }}

      controller.selectElement.bindFromRequest()(org.mockito.Matchers.eq(req)) returns controller.selectElement
      controller.selectElement.fold(any[Form[SelectInfo]=>Future[Result]],any[SelectInfo=>Result]) answers {(params, _) => params match {
        case Array(p: (Form[SelectInfo]=>Future[Result]),_)=>p.apply(f.controller.selectElement)
      }}

      f.cardsManagerMock.getInventaryCards(any[JsObject],any[JsObject],any[BSONObjectID],any[Result])(any[(TypeCards,List[Cards],List[Firmware],Int,List[(BSONObjectID,Int)],Map[BSONObjectID,String])=>Result]) answers { (params, _) => params match {
        case Array(_,_,_,_, p: ((TypeCards,List[Cards],List[Firmware],Int,List[(BSONObjectID,Int)],Map[BSONObjectID,String])=>Result))=>future{p.apply(typeCards,card,firmware,1,List((bson,0)),Map(bson2->"Test"))}
      }}

      val r=controller.addCards(bson.stringify).apply(req)

      status(r) must equalTo(BAD_REQUEST)

      there was one(f.typeCardsManagerMock).doIfTypeCardsFound(org.mockito.Matchers.eq(bson))(any[TypeCards => Future[Result]])(any[Unit => Future[Result]])
      there was one(controller.selectElement).bindFromRequest()(org.mockito.Matchers.eq(req))
      there was one(controller.selectElement).fold(any[Form[SelectInfo]=>Future[Result]],any[SelectInfo=>Result])
      there was one(f.cardsManagerMock).getInventaryCards(any[JsObject],any[JsObject],any[BSONObjectID],any[Result])(any[(TypeCards,List[Cards],List[Firmware],Int,List[(BSONObjectID,Int)],Map[BSONObjectID,String])=>Result])
    }

    "send redirect if cards type not found" in new WithApplication{
      val f=fixture

      f.typeCardsManagerMock.doIfTypeCardsFound(org.mockito.Matchers.eq(bson))(any[TypeCards => Future[Result]])(any[Unit => Future[Result]]) answers { (params, _) => params match {
        case Array(_,_, p: (Unit => Future[Result])) => p.apply()
      }}

      val r=f.controller.addCards(bson.stringify).apply(FakeRequest("POST","/inventary/modules/form/cards/"+bson.stringify).withSession("user" -> """{"login":"test"}"""))

      status(r) must equalTo(SEE_OTHER)
      header("Location",r) must beSome("/inventary/modules/form/typeCards")

      there was one(f.typeCardsManagerMock).doIfTypeCardsFound(org.mockito.Matchers.eq(bson))(any[TypeCards => Future[Result]])(any[Unit => Future[Result]])
    }

    "send redirect if no elements select and user press on button 'Envoyer'" in new WithApplication{
      val f=fixture
      val data=Json.parse("""{"send":"Envoyer"}""")
      val typeCards=mock[TypeCards]

      f.typeCardsManagerMock.doIfTypeCardsFound(org.mockito.Matchers.eq(bson))(any[TypeCards => Future[Result]])(any[Unit => Future[Result]]) answers { (params, _) => params match {
        case Array(_,p: (TypeCards => Future[Result]),_) => p.apply(typeCards)
      }}

      val r=f.controller.addCards(bson.stringify).apply(FakeRequest("POST","/inventary/modules/form/cards/"+bson.stringify).withJsonBody(data).withSession("user" -> """{"login":"test"}"""))

      status(r) must equalTo(SEE_OTHER)
      header("Location",r) must beSome("/inventary/modules/form/typeCards")

      there was one(f.typeCardsManagerMock).doIfTypeCardsFound(org.mockito.Matchers.eq(bson))(any[TypeCards => Future[Result]])(any[Unit => Future[Result]])
    }

    "send redirect if no elements select and user press on button 'Passer à l'étape suivante'" in new WithApplication{
      val f=fixture
      val data=Json.parse("""{"send":"Passer à l'étape suivante"}""")
      val typeCards=mock[TypeCards]

      f.typeCardsManagerMock.doIfTypeCardsFound(org.mockito.Matchers.eq(bson))(any[TypeCards => Future[Result]])(any[Unit => Future[Result]]) answers { (params, _) => params match {
        case Array(_,p: (TypeCards => Future[Result]),_) => p.apply(typeCards)
      }}

      val r=f.controller.addCards(bson.stringify).apply(FakeRequest("POST","/inventary/modules/form/cards/"+bson.stringify).withJsonBody(data).withSession("user" -> """{"login":"test"}"""))

      status(r) must equalTo(SEE_OTHER)
      header("Location",r) must beSome("/inventary/modules/form/typeSensors")

      there was one(f.typeCardsManagerMock).doIfTypeCardsFound(org.mockito.Matchers.eq(bson))(any[TypeCards => Future[Result]])(any[Unit => Future[Result]])
    }

    "send redirect if after add cards to module in session" in new WithApplication{
      val f=fixture
      val data=Json.parse("""{"elements":[""""+bson.stringify+""""],"send":"Envoyer"}""")
      val typeCards=mock[TypeCards]

      f.typeCardsManagerMock.doIfTypeCardsFound(org.mockito.Matchers.eq(bson))(any[TypeCards => Future[Result]])(any[Unit => Future[Result]]) answers { (params, _) => params match {
        case Array(_,p: (TypeCards => Future[Result]),_) => p.apply(typeCards)
      }}
      f.cardsDaoMock.fold(any[JsObject],any[JsObject],org.mockito.Matchers.eq(List[BSONObjectID]()))(any[(List[BSONObjectID],Cards)=>List[BSONObjectID]])(any[ExecutionContext]) returns future{List(bson)}


      val r=f.controller.addCards(bson.stringify).apply(FakeRequest("POST","/inventary/modules/form/cards/"+bson.stringify).withJsonBody(data).withSession("user" -> """{"login":"test"}"""))

      status(r) must equalTo(SEE_OTHER)
      header("Location",r) must beSome(("/inventary/modules/form/typeCards"))
      val s=session(r)
      s.get("module") must not equalTo(None)
      val jsonModule=Json.parse(s.get("module").getOrElse(""))
      (jsonModule\"cartes").as[List[BSONObjectID]] must contain(bson)

      there was one(f.cardsDaoMock).fold(any[JsObject],any[JsObject],org.mockito.Matchers.eq(List[BSONObjectID]()))(any[(List[BSONObjectID],Cards)=>List[BSONObjectID]])(any[ExecutionContext])
      there was one(f.typeCardsManagerMock).doIfTypeCardsFound(org.mockito.Matchers.eq(bson))(any[TypeCards => Future[Result]])(any[Unit => Future[Result]])
    }
  }

  "When user is on resource /inventary/modules/form/typeSensors, ModuleManager" should{
    "send 200 on OK with the message 'Aucun résultat trouvé'" in new WithApplication {
      val f=fixture
      val module=Module(bson,"id","types",date,List(bson2),List(bson3),Some("un com"))
      val listSensors=JsArray(Seq(BSONObjectIDFormat.writes(bson4)))

      f.moduleDaoMock.fold(org.mockito.Matchers.eq(Json.obj("delete"->false,"_id"->Json.obj("$ne"->bson))),any[JsObject],org.mockito.Matchers.eq(module.capteurs))(any[(List[BSONObjectID],Module)=>List[BSONObjectID]])(any[ExecutionContext]) returns future{List[BSONObjectID](bson4)}
      f.sensorsDaoMock.fold(org.mockito.Matchers.eq(Json.obj("delete"->false,"_id"->Json.obj("$nin"->listSensors))),org.mockito.Matchers.eq(Json.obj()),org.mockito.Matchers.eq(HashSet[JsValue]()))(any[(HashSet[JsValue],Sensor)=>HashSet[JsValue]])(any[ExecutionContext]) returns future{HashSet[JsValue]()}
      f.typeSensorManagerMock.getInventaryTypeSensor(any[JsObject],anyString,anyString)(any[(List[TypeSensor],List[TypeMesure],List[Espece],List[BSONDocument],List[(BSONObjectID,Int)],List[BSONDocument])=>Result]) answers { (params, _) => params match {
        case Array(_,_,_, p: ((List[TypeSensor],List[TypeMesure],List[Espece],List[BSONDocument],List[(BSONObjectID,Int)],List[BSONDocument])=>Result))=>future{p.apply(List[TypeSensor](),List[TypeMesure](),List(),List[BSONDocument](),List(),List[BSONDocument]())}
      }}

      val r = f.controller.formTypeSensors().apply(FakeRequest(GET, "/inventary/modules/form/typeSensors").withSession("user" -> """{"login":"test"}""","module"->Module.toStrings(module)))
      status(r) must equalTo(OK)
      contentType(r) must beSome.which(_ == "text/html")
      val content = contentAsString(r)
      content must contain("<title>Inventaire des modules</title>")
      content must contain("<h3 style=\"text-align:center\">Aucun résultat trouvé</h3>")

      there was one(f.moduleDaoMock).fold(org.mockito.Matchers.eq(Json.obj("delete"->false,"_id"->Json.obj("$ne"->bson))),any[JsObject],org.mockito.Matchers.eq(module.capteurs))(any[(List[BSONObjectID],Module)=>List[BSONObjectID]])(any[ExecutionContext])
      there was one(f.sensorsDaoMock).fold(org.mockito.Matchers.eq(Json.obj("delete"->false,"_id"->Json.obj("$nin"->listSensors))),org.mockito.Matchers.eq(Json.obj()),org.mockito.Matchers.eq(HashSet[JsValue]()))(any[(HashSet[JsValue],Sensor)=>HashSet[JsValue]])(any[ExecutionContext])
      there was one(f.typeSensorManagerMock).getInventaryTypeSensor(any[JsObject],anyString,anyString)(any[(List[TypeSensor],List[TypeMesure],List[Espece],List[BSONDocument],List[(BSONObjectID,Int)],List[BSONDocument])=>Result])
    }

    "send 200 on OK with 1 result" in new WithApplication {
      val f=fixture
      val module=Module(bson,"id","types",date,List(bson2),List(bson3),Some("un com"))
      val listSensors=JsArray(Seq(BSONObjectIDFormat.writes(bson4)))

      f.moduleDaoMock.fold(org.mockito.Matchers.eq(Json.obj("delete"->false,"_id"->Json.obj("$ne"->bson))),any[JsObject],org.mockito.Matchers.eq(module.capteurs))(any[(List[BSONObjectID],Module)=>List[BSONObjectID]])(any[ExecutionContext]) returns future{List[BSONObjectID](bson4)}
      f.sensorsDaoMock.fold(org.mockito.Matchers.eq(Json.obj("delete"->false,"_id"->Json.obj("$nin"->listSensors))),org.mockito.Matchers.eq(Json.obj()),org.mockito.Matchers.eq(HashSet[JsValue]()))(any[(HashSet[JsValue],Sensor)=>HashSet[JsValue]])(any[ExecutionContext]) returns future{HashSet[JsValue]()}
      f.typeSensorManagerMock.getInventaryTypeSensor(any[JsObject],anyString,anyString)(any[(List[TypeSensor],List[TypeMesure],List[Espece],List[BSONDocument],List[(BSONObjectID,Int)],List[BSONDocument])=>Result]) answers { (params, _) => params match {
        case Array(_,_,_, p: ((List[TypeSensor],List[TypeMesure],List[Espece],List[BSONDocument],List[(BSONObjectID,Int)],List[BSONDocument])=>Result))=>future{p.apply(List[TypeSensor](TypeSensor(bson, "type1", "modele1", "fab1", 1,List(bson3))),List[TypeMesure](TypeMesure(bson2, "mesure1", "unite1")),List(Espece(bson3,"esp1",bson2,2.3f,3.4f)),List[BSONDocument](BSONDocument("_id" -> bson, "count" -> 5)),List((bson,3)),List[BSONDocument]())}
      }}

      val r = f.controller.formTypeSensors().apply(FakeRequest(GET, "/inventary/modules/form/typeSensors").withSession("user" -> """{"login":"test"}""","module"->Module.toStrings(module)))
      status(r) must equalTo(OK)
      contentType(r) must beSome.which(_ == "text/html")
      val content = contentAsString(r)
      content must contain("<title>Inventaire des modules</title>")
      content must not contain("<h3 style=\"text-align:center\">Aucun résultat trouvé</h3>")
      content must contain("type1")
      content must contain("modele1")
      content must matchRegex("<span class=\"bold\">\\s*Fabricant\\s*</span>\\s*:\\s*fab1")
      content must matchRegex("<span class=\"bold\">\\s*Nombre de signaux\\s*</span>\\s*:\\s*1")
      content must contain("<td>esp1</td>")
      content must contain("<td>mesure1</td>")
      content must contain("<td>2.3 unite1</td>")
      content must contain("<td>3.4 unite1</td>")
      content must matchRegex("<span class=\"bold\">\\s*Stock\\s*</span>\\s*:\\s*2 / 5")

      there was one(f.moduleDaoMock).fold(org.mockito.Matchers.eq(Json.obj("delete"->false,"_id"->Json.obj("$ne"->bson))),any[JsObject],org.mockito.Matchers.eq(module.capteurs))(any[(List[BSONObjectID],Module)=>List[BSONObjectID]])(any[ExecutionContext])
      there was one(f.sensorsDaoMock).fold(org.mockito.Matchers.eq(Json.obj("delete"->false,"_id"->Json.obj("$nin"->listSensors))),org.mockito.Matchers.eq(Json.obj()),org.mockito.Matchers.eq(HashSet[JsValue]()))(any[(HashSet[JsValue],Sensor)=>HashSet[JsValue]])(any[ExecutionContext])
      there was one(f.typeSensorManagerMock).getInventaryTypeSensor(any[JsObject],anyString,anyString)(any[(List[TypeSensor],List[TypeMesure],List[Espece],List[BSONDocument],List[(BSONObjectID,Int)],List[BSONDocument])=>Result])
    }

    "send 200 on OK with 2 results" in new WithApplication {
      val f=fixture
      val listTypeSensor=List[TypeSensor](
        TypeSensor(bson, "type1", "modele1", "fab1", 1,List(bson3)),
        TypeSensor(bson3, "type2", "modele2", "fab2", 2,List(bson2))
      )
      val listTypeMesure=List[TypeMesure](TypeMesure(bson2, "mesure1", "unite1"), TypeMesure(bson4, "mesure2", "unite2"))
      val listEspece=List(Espece(bson3,"esp1",bson2,2.3f,3.4f),Espece(bson2,"esp2",bson4,4.5f,5.6f))
      val countSensor=List[BSONDocument](BSONDocument("_id" -> bson, "count" -> 5), BSONDocument("_id" -> bson3, "count" -> 3))
      val module=Module(bson,"id","types",date,List(bson2),List(bson3),Some("un com"))
      val listSensors=JsArray(Seq(BSONObjectIDFormat.writes(bson4)))

      f.moduleDaoMock.fold(org.mockito.Matchers.eq(Json.obj("delete"->false,"_id"->Json.obj("$ne"->bson))),any[JsObject],org.mockito.Matchers.eq(module.capteurs))(any[(List[BSONObjectID],Module)=>List[BSONObjectID]])(any[ExecutionContext]) returns future{List[BSONObjectID](bson4)}
      f.sensorsDaoMock.fold(org.mockito.Matchers.eq(Json.obj("delete"->false,"_id"->Json.obj("$nin"->listSensors))),org.mockito.Matchers.eq(Json.obj()),org.mockito.Matchers.eq(HashSet[JsValue]()))(any[(HashSet[JsValue],Sensor)=>HashSet[JsValue]])(any[ExecutionContext]) returns future{HashSet[JsValue]()}
      f.typeSensorManagerMock.getInventaryTypeSensor(any[JsObject],anyString,anyString)(any[(List[TypeSensor],List[TypeMesure],List[Espece],List[BSONDocument],List[(BSONObjectID,Int)],List[BSONDocument])=>Result]) answers { (params, _) => params match {
        case Array(_,_,_, p: ((List[TypeSensor],List[TypeMesure],List[Espece],List[BSONDocument],List[(BSONObjectID,Int)],List[BSONDocument])=>Result))=>future{p.apply(listTypeSensor,listTypeMesure,listEspece,countSensor,List((bson,3),(bson3,2)),List[BSONDocument]())}
      }}

      val r = f.controller.formTypeSensors().apply(FakeRequest(GET, "/inventary/modules/form/typeSensors").withSession("user" -> """{"login":"test"}""","module"->Module.toStrings(module)))
      status(r) must equalTo(OK)
      contentType(r) must beSome.which(_ == "text/html")
      val content = contentAsString(r)
      content must contain("<title>Inventaire des modules</title>")
      content must not contain("<h3 style=\"text-align:center\">Aucun résultat trouvé</h3>")
      content must contain("type1")
      content must contain("modele1")
      content must matchRegex("<span class=\"bold\">\\s*Fabricant\\s*</span>\\s*:\\s*fab1")
      content must matchRegex("<span class=\"bold\">\\s*Nombre de signaux\\s*</span>\\s*:\\s*1")
      content must contain("<td>esp1</td>")
      content must contain("<td>mesure1</td>")
      content must contain("<td>2.3 unite1</td>")
      content must contain("<td>3.4 unite1</td>")
      content must matchRegex("<span class=\"bold\">\\s*Stock\\s*</span>\\s*:\\s*2 / 5")

      content must contain("type2")
      content must contain("modele2")
      content must matchRegex("<span class=\"bold\">\\s*Fabricant\\s*</span>\\s*:\\s*fab2")
      content must matchRegex("<span class=\"bold\">\\s*Nombre de signaux\\s*</span>\\s*:\\s*2")
      content must contain("<td>esp2</td>")
      content must contain("<td>mesure2</td>")
      content must contain("<td>4.5 unite2</td>")
      content must contain("<td>5.6 unite2</td>")
      content must matchRegex("<span class=\"bold\">\\s*Stock\\s*</span>\\s*:\\s*1 / 3")

      there was one(f.moduleDaoMock).fold(org.mockito.Matchers.eq(Json.obj("delete"->false,"_id"->Json.obj("$ne"->bson))),any[JsObject],org.mockito.Matchers.eq(module.capteurs))(any[(List[BSONObjectID],Module)=>List[BSONObjectID]])(any[ExecutionContext])
      there was one(f.sensorsDaoMock).fold(org.mockito.Matchers.eq(Json.obj("delete"->false,"_id"->Json.obj("$nin"->listSensors))),org.mockito.Matchers.eq(Json.obj()),org.mockito.Matchers.eq(HashSet[JsValue]()))(any[(HashSet[JsValue],Sensor)=>HashSet[JsValue]])(any[ExecutionContext])
      there was one(f.typeSensorManagerMock).getInventaryTypeSensor(any[JsObject],anyString,anyString)(any[(List[TypeSensor],List[TypeMesure],List[Espece],List[BSONDocument],List[(BSONObjectID,Int)],List[BSONDocument])=>Result])
    }
  }

  "When user is on resource /inventary/modules/form/sensors/:id, ModuleManager" should{
    "send 200 on OK with the message 'Aucun résultat trouvé'" in new WithApplication {
      val f=fixture
      val module=Module(bson,"id","types",date,List(bson2),List(bson3),Some("un com"))
      val listSensors=JsArray(Seq(BSONObjectIDFormat.writes(bson4)))

      f.moduleDaoMock.fold(org.mockito.Matchers.eq(Json.obj("delete"->false,"_id"->Json.obj("$ne"->bson))),any[JsObject],org.mockito.Matchers.eq(module.capteurs))(any[(List[BSONObjectID],Module)=>List[BSONObjectID]])(any[ExecutionContext]) returns future{List[BSONObjectID](bson4)}
      f.sensorsManagerMock.getInventarySensor(any[JsObject],any[JsObject],any[BSONObjectID],any[Result])(any[(TypeSensor,List[Espece],List[TypeMesure],List[Sensor],Int,List[(BSONObjectID,Int)],Map[BSONObjectID,String])=>Result]) answers { (params, _) => params match {
        case Array(_,_,_,_, p: ((TypeSensor,List[Espece],List[TypeMesure],List[Sensor],Int,List[(BSONObjectID,Int)],Map[BSONObjectID,String])=>Result))=>future{p.apply(TypeSensor(bson,"type1","modele1","fab1",1,List(bson3)),List(Espece(bson3,"esp1",bson2,2.3f,3.4f)),List(TypeMesure(bson2, "mesure1", "unite1")),List[Sensor](),0,List((bson,0)),Map())}
      }}

      val r = f.controller.formSensors(bson.stringify).apply(FakeRequest(GET, "/inventary/modules/form/sensors/"+bson.stringify).withSession("user" -> """{"login":"test"}""","module"->Module.toStrings(module)))
      status(r) must equalTo(OK)
      contentType(r) must beSome.which(_ == "text/html")
      val content = contentAsString(r)
      content must contain("<title>Inventaire des modules</title>")
      content must contain("<h3 style=\"text-align:center\">Aucun résultat trouvé</h3>")
      content must matchRegex("type1\\s*/\\s*modele1")
      content must matchRegex("<span class=\"bold\">\\s*Fabricant\\s*</span>\\s*:\\s*fab1")
      content must matchRegex("<span class=\"bold\">\\s*Nombre de signaux\\s*</span>\\s*:\\s*1")
      content must matchRegex("<td>mesure1</td>")
      content must contain("<td>esp1</td>")
      content must contain("<td>2.3 unite1</td>")
      content must contain("<td>3.4 unite1</td>")
      content must matchRegex("<span class=\"bold\">\\s*Stock\\s*</span>\\s*:\\s*0 / 0")

      there was one(f.moduleDaoMock).fold(org.mockito.Matchers.eq(Json.obj("delete"->false,"_id"->Json.obj("$ne"->bson))),any[JsObject],org.mockito.Matchers.eq(module.capteurs))(any[(List[BSONObjectID],Module)=>List[BSONObjectID]])(any[ExecutionContext])
      there was one(f.sensorsManagerMock).getInventarySensor(any[JsObject],any[JsObject],any[BSONObjectID],any[Result])(any[(TypeSensor,List[Espece],List[TypeMesure],List[Sensor],Int,List[(BSONObjectID,Int)],Map[BSONObjectID,String])=>Result])
    }

    "send 200 on OK with 1 result" in new WithApplication {
      val f=fixture
      val typeSensor=TypeSensor(bson,"type1","modele1","fab1",1,List())
      val typeMesure=TypeMesure(bson2, "mesure1", "unite1")
      val sensors=List[Sensor](Sensor(bson3, "Id", bson, None, date, None, false, None))
      val module=Module(bson,"id","types",date,List(bson2),List(bson3),Some("un com"))
      val listSensors=JsArray(Seq(BSONObjectIDFormat.writes(bson4)))

      f.moduleDaoMock.fold(org.mockito.Matchers.eq(Json.obj("delete"->false,"_id"->Json.obj("$ne"->bson))),any[JsObject],org.mockito.Matchers.eq(module.capteurs))(any[(List[BSONObjectID],Module)=>List[BSONObjectID]])(any[ExecutionContext]) returns future{List[BSONObjectID](bson4)}
      f.sensorsManagerMock.getInventarySensor(any[JsObject],any[JsObject],any[BSONObjectID],any[Result])(any[(TypeSensor,List[Espece],List[TypeMesure],List[Sensor],Int,List[(BSONObjectID,Int)],Map[BSONObjectID,String])=>Result]) answers { (params, _) => params match {
        case Array(_,_,_,_, p: ((TypeSensor,List[Espece],List[TypeMesure],List[Sensor],Int,List[(BSONObjectID,Int)],Map[BSONObjectID,String])=>Result))=>future{p.apply(typeSensor,List(),List(typeMesure),sensors,1,List((bson,0)),Map(bson3->"Test"))}
      }}

      val r = f.controller.formSensors(bson.stringify).apply(FakeRequest(GET, "/inventary/modules/form/sensors/"+bson.stringify).withSession("user" -> """{"login":"test"}""","module"->Module.toStrings(module)))
      status(r) must equalTo(OK)
      contentType(r) must beSome.which(_ == "text/html")
      val content = contentAsString(r)
      content must contain("<title>Inventaire des modules</title>")
      content must not contain("<h3 style=\"text-align:center\">Aucun résultat trouvé</h3>")
      content must contain("<td>Id</td>")
      content must contain("<td>22/04/2015</td>")
      content must contain("<td>Test</td>")

      there was one(f.moduleDaoMock).fold(org.mockito.Matchers.eq(Json.obj("delete"->false,"_id"->Json.obj("$ne"->bson))),any[JsObject],org.mockito.Matchers.eq(module.capteurs))(any[(List[BSONObjectID],Module)=>List[BSONObjectID]])(any[ExecutionContext])
      there was one(f.sensorsManagerMock).getInventarySensor(any[JsObject],any[JsObject],any[BSONObjectID],any[Result])(any[(TypeSensor,List[Espece],List[TypeMesure],List[Sensor],Int,List[(BSONObjectID,Int)],Map[BSONObjectID,String])=>Result])
    }

    "send 200 on OK with 1 result for request with filter" in new WithApplication {
      val f=fixture
      val typeSensor=TypeSensor(bson,"type1","modele1","fab1",1,List())
      val typeMesure=TypeMesure(bson2, "mesure1", "unite1")
      val sensors=List[Sensor](Sensor(bson3, "Id", bson, None, date, None, false, None))
      val module=Module(bson,"id","types",date,List(bson2),List(bson3),Some("un com"))
      val listSensors=JsArray(Seq(BSONObjectIDFormat.writes(bson4)))

      f.moduleDaoMock.fold(org.mockito.Matchers.eq(Json.obj("delete"->false,"_id"->Json.obj("$ne"->bson))),any[JsObject],org.mockito.Matchers.eq(module.capteurs))(any[(List[BSONObjectID],Module)=>List[BSONObjectID]])(any[ExecutionContext]) returns future{List[BSONObjectID](bson4)}
      f.sensorsManagerMock.getInventarySensor(org.mockito.Matchers.eq(Json.obj("delete"->false,"types"->bson,"_id"->Json.obj("$nin"->listSensors),"id"->Json.obj("$regex"->".*val.*"))),any[JsObject],any[BSONObjectID],any[Result])(any[(TypeSensor,List[Espece],List[TypeMesure],List[Sensor],Int,List[(BSONObjectID,Int)],Map[BSONObjectID,String])=>Result]) answers { (params, _) => params match {
        case Array(_,_,_,_, p: ((TypeSensor,List[Espece],List[TypeMesure],List[Sensor],Int,List[(BSONObjectID,Int)],Map[BSONObjectID,String])=>Result))=>future{p.apply(typeSensor,List(),List(typeMesure),sensors,1,List((bson,0)),Map(bson3->"Test"))}
      }}

      val r = f.controller.formSensors(bson.stringify,"id",1,"val").apply(FakeRequest(GET, "/inventary/modules/form/sensors/"+bson.stringify).withSession("user" -> """{"login":"test"}""","module"->Module.toStrings(module)))
      status(r) must equalTo(OK)
      contentType(r) must beSome.which(_ == "text/html")
      val content = contentAsString(r)
      content must contain("<title>Inventaire des modules</title>")
      content must not contain("<h3 style=\"text-align:center\">Aucun résultat trouvé</h3>")
      content must contain("<td>Id</td>")
      content must contain("<td>22/04/2015</td>")
      content must contain("<td>Test</td>")

      there was one(f.moduleDaoMock).fold(org.mockito.Matchers.eq(Json.obj("delete"->false,"_id"->Json.obj("$ne"->bson))),any[JsObject],org.mockito.Matchers.eq(module.capteurs))(any[(List[BSONObjectID],Module)=>List[BSONObjectID]])(any[ExecutionContext])
      there was one(f.sensorsManagerMock).getInventarySensor(org.mockito.Matchers.eq(Json.obj("delete"->false,"types"->bson,"_id"->Json.obj("$nin"->listSensors),"id"->Json.obj("$regex"->".*val.*"))),any[JsObject],any[BSONObjectID],any[Result])(any[(TypeSensor,List[Espece],List[TypeMesure],List[Sensor],Int,List[(BSONObjectID,Int)],Map[BSONObjectID,String])=>Result])
    }

    "send 200 on OK with 2 results" in new WithApplication {
      val f=fixture
      val typeSensor=TypeSensor(bson,"type1","modele1","fab1",1,List())
      val typeMesure=TypeMesure(bson2, "mesure1", "unite1")
      val sensors=List[Sensor](Sensor(bson3, "Id", bson, None, date, None, false, None),Sensor(bson4, "Id2", bson, Some(date), date, Some(date), true, Some("un com")))
      val module=Module(bson,"id","types",date,List(bson2),List(bson3),Some("un com"))
      val listSensors=JsArray(Seq(BSONObjectIDFormat.writes(bson4)))

      f.moduleDaoMock.fold(org.mockito.Matchers.eq(Json.obj("delete"->false,"_id"->Json.obj("$ne"->bson))),any[JsObject],org.mockito.Matchers.eq(module.capteurs))(any[(List[BSONObjectID],Module)=>List[BSONObjectID]])(any[ExecutionContext]) returns future{List[BSONObjectID](bson4)}
      f.sensorsManagerMock.getInventarySensor(any[JsObject],any[JsObject],any[BSONObjectID],any[Result])(any[(TypeSensor,List[Espece],List[TypeMesure],List[Sensor],Int,List[(BSONObjectID,Int)],Map[BSONObjectID,String])=>Result]) answers { (params, _) => params match {
        case Array(_,_,_,_, p: ((TypeSensor,List[Espece],List[TypeMesure],List[Sensor],Int,List[(BSONObjectID,Int)],Map[BSONObjectID,String])=>Result))=>future{p.apply(typeSensor,List(),List(typeMesure),sensors,2,List((bson,1)),Map(bson3->"Test",bson4->"Terrain"))}
      }}

      val r = f.controller.formSensors(bson.stringify).apply(FakeRequest(GET, "/inventary/modules/form/sensors/"+bson.stringify).withSession("user" -> """{"login":"test"}""","module"->Module.toStrings(module)))
      status(r) must equalTo(OK)
      contentType(r) must beSome.which(_ == "text/html")
      val content = contentAsString(r)
      content must contain("<title>Inventaire des modules</title>")
      content must not contain("<h3 style=\"text-align:center\">Aucun résultat trouvé</h3>")
      content must contain("<td>Id</td>")
      content must contains("<td>22/04/2015</td>", 4)
      content must contain("<td>Test</td>")
      content must contain("<td>Id2</td>")
      content must contain("<td>Hors service<br/>Terrain</td>")

      there was one(f.moduleDaoMock).fold(org.mockito.Matchers.eq(Json.obj("delete"->false,"_id"->Json.obj("$ne"->bson))),any[JsObject],org.mockito.Matchers.eq(module.capteurs))(any[(List[BSONObjectID],Module)=>List[BSONObjectID]])(any[ExecutionContext])
      there was one(f.sensorsManagerMock).getInventarySensor(any[JsObject],any[JsObject],any[BSONObjectID],any[Result])(any[(TypeSensor,List[Espece],List[TypeMesure],List[Sensor],Int,List[(BSONObjectID,Int)],Map[BSONObjectID,String])=>Result])
    }

    "send bad request with list of sensors if form submit without data" in new WithApplication{
      val f=fixture
      val typeSensor=TypeSensor(bson,"type1","modele1","fab1",1,List())
      val typeMesure=TypeMesure(bson2, "mesure1", "unite1")
      val sensors=List[Sensor](Sensor(bson3, "Id", bson, None, date, None, false, None))
      val controller=new ModuleManagerTest{
        override val typeSensorManager:TypeSensorManagerLike = f.typeSensorManagerMock
        override val sensorsManager:SensorManagerLike = f.sensorsManagerMock
        override val selectElement=mock[Form[SelectInfo]]
      }

      val req=FakeRequest("POST","/inventary/modules/form/sensors/"+bson.stringify).withSession("user" -> """{"login":"test"}""")

      f.typeSensorManagerMock.doIfTypeSensorFound(org.mockito.Matchers.eq(bson))(any[TypeSensor => Future[Result]])(any[Unit => Future[Result]]) answers { (params, _) => params match {
        case Array(_,p: (TypeSensor => Future[Result]),_) => p.apply(typeSensor)
      }}

      controller.selectElement.bindFromRequest()(org.mockito.Matchers.eq(req)) returns controller.selectElement
      controller.selectElement.fold(any[Form[SelectInfo]=>Future[Result]],any[SelectInfo=>Result]) answers {(params, _) => params match {
        case Array(p: (Form[SelectInfo]=>Future[Result]),_)=>p.apply(f.controller.selectElement)
      }}

      f.sensorsManagerMock.getInventarySensor(any[JsObject],any[JsObject],any[BSONObjectID],any[Result])(any[(TypeSensor,List[Espece],List[TypeMesure],List[Sensor],Int,List[(BSONObjectID,Int)],Map[BSONObjectID,String])=>Result]) answers { (params, _) => params match {
        case Array(_,_,_,_, p: ((TypeSensor,List[Espece],List[TypeMesure],List[Sensor],Int,List[(BSONObjectID,Int)],Map[BSONObjectID,String])=>Result))=>future{p.apply(typeSensor,List(),List(typeMesure),sensors,0,List((bson,0)),Map(bson3->"Test"))}
      }}

      val r=controller.addSensors(bson.stringify).apply(req)

      status(r) must equalTo(BAD_REQUEST)

      there was one(f.typeSensorManagerMock).doIfTypeSensorFound(org.mockito.Matchers.eq(bson))(any[TypeSensor => Future[Result]])(any[Unit => Future[Result]])
      there was one(controller.selectElement).bindFromRequest()(org.mockito.Matchers.eq(req))
      there was one(controller.selectElement).fold(any[Form[SelectInfo]=>Future[Result]],any[SelectInfo=>Result])
      there was one(f.sensorsManagerMock).getInventarySensor(any[JsObject],any[JsObject],any[BSONObjectID],any[Result])(any[(TypeSensor,List[Espece],List[TypeMesure],List[Sensor],Int,List[(BSONObjectID,Int)],Map[BSONObjectID,String])=>Result])
    }

    "send redirect if sensors type not found" in new WithApplication{
      val f=fixture

      f.typeSensorManagerMock.doIfTypeSensorFound(org.mockito.Matchers.eq(bson))(any[TypeSensor => Future[Result]])(any[Unit => Future[Result]]) answers { (params, _) => params match {
        case Array(_,_, p: (Unit => Future[Result])) => p.apply()
      }}

      val r=f.controller.addSensors(bson.stringify).apply(FakeRequest("POST","/inventary/modules/form/sensors/"+bson.stringify).withSession("user" -> """{"login":"test"}"""))

      status(r) must equalTo(SEE_OTHER)
      header("Location",r) must beSome("/inventary/modules/form/typeSensors")

      there was one(f.typeSensorManagerMock).doIfTypeSensorFound(org.mockito.Matchers.eq(bson))(any[TypeSensor => Future[Result]])(any[Unit => Future[Result]])
    }

    "send redirect if no elements select and user press on button 'Envoyer'" in new WithApplication{
      val f=fixture
      val data=Json.parse("""{"send":"Envoyer"}""")
      val typeSensor=mock[TypeSensor]

      f.typeSensorManagerMock.doIfTypeSensorFound(org.mockito.Matchers.eq(bson))(any[TypeSensor => Future[Result]])(any[Unit => Future[Result]]) answers { (params, _) => params match {
        case Array(_,p: (TypeSensor => Future[Result]),_) => p.apply(typeSensor)
      }}

      val r=f.controller.addSensors(bson.stringify).apply(FakeRequest("POST","/inventary/modules/form/sensors/"+bson.stringify).withJsonBody(data).withSession("user" -> """{"login":"test"}"""))

      status(r) must equalTo(SEE_OTHER)
      header("Location",r) must beSome("/inventary/modules/form/typeSensors")

      there was one(f.typeSensorManagerMock).doIfTypeSensorFound(org.mockito.Matchers.eq(bson))(any[TypeSensor => Future[Result]])(any[Unit => Future[Result]])
    }

    "send redirect if no elements select and user press on button 'Passer à l'étape suivante'" in new WithApplication{
      val f=fixture
      val data=Json.parse("""{"send":"Passer à l'étape suivante"}""")
      val typeSensor=mock[TypeSensor]

      f.typeSensorManagerMock.doIfTypeSensorFound(org.mockito.Matchers.eq(bson))(any[TypeSensor => Future[Result]])(any[Unit => Future[Result]]) answers { (params, _) => params match {
        case Array(_,p: (TypeSensor => Future[Result]),_) => p.apply(typeSensor)
      }}

      val r=f.controller.addSensors(bson.stringify).apply(FakeRequest("POST","/inventary/modules/form/sensors/"+bson.stringify).withJsonBody(data).withSession("user" -> """{"login":"test"}"""))

      status(r) must equalTo(SEE_OTHER)
      header("Location",r) must beSome("/inventary/modules/form/validate")

      there was one(f.typeSensorManagerMock).doIfTypeSensorFound(org.mockito.Matchers.eq(bson))(any[TypeSensor => Future[Result]])(any[Unit => Future[Result]])
    }

    "send redirect after add sensors to module in session if user press on button 'Envoyer'" in new WithApplication{
      val f=fixture
      val data=Json.parse("""{"elements":[""""+bson.stringify+""""],"send":"Envoyer"}""")
      val typeSensor=mock[TypeSensor]

      f.typeSensorManagerMock.doIfTypeSensorFound(org.mockito.Matchers.eq(bson))(any[TypeSensor => Future[Result]])(any[Unit => Future[Result]]) answers { (params, _) => params match {
        case Array(_,p: (TypeSensor => Future[Result]),_) => p.apply(typeSensor)
      }}
      f.sensorsDaoMock.fold(any[JsObject],any[JsObject],org.mockito.Matchers.eq(List[BSONObjectID]()))(any[(List[BSONObjectID],Sensor)=>List[BSONObjectID]])(any[ExecutionContext]) returns future{List(bson)}


      val r=f.controller.addSensors(bson.stringify).apply(FakeRequest("POST","/inventary/modules/form/sensors/"+bson.stringify).withJsonBody(data).withSession("user" -> """{"login":"test"}"""))

      status(r) must equalTo(SEE_OTHER)
      header("Location",r) must beSome(("/inventary/modules/form/typeSensors"))
      val s=session(r)
      s.get("module") must not equalTo(None)
      val jsonModule=Json.parse(s.get("module").getOrElse(""))
      (jsonModule\"capteurs").as[List[BSONObjectID]] must contain(bson)

      there was one(f.sensorsDaoMock).fold(any[JsObject],any[JsObject],org.mockito.Matchers.eq(List[BSONObjectID]()))(any[(List[BSONObjectID],Sensor)=>List[BSONObjectID]])(any[ExecutionContext])
      there was one(f.typeSensorManagerMock).doIfTypeSensorFound(org.mockito.Matchers.eq(bson))(any[TypeSensor => Future[Result]])(any[Unit => Future[Result]])
    }
  }

  "When user is on resource /inventary/modules/form, ModuleManager" should {
    "send 200 OK page with an empty form" in new WithApplication {
      val f=fixture

      f.moduleDaoMock.findListType() returns future{Stream[BSONDocument]()}

      val r=f.controller.formGeneralAdd().apply(FakeRequest("GET","/inventary/modules/form").withSession("user" -> """{"login":"test"}"""))

      status(r) must equalTo(OK)
      val content=contentAsString(r)
      content must contain("<input id=\"types\" name=\"types\" class=\"form-control\" list=\"list_type\" autofocus=\"autofocus\" type=\"text\" autocomplete=\"off\" value=\"\"/>")
      content must contain("<input type=\"text\" id=\"id\" name=\"id\" value=\"\" class=\"form-control\"/>")
      content must matchRegex("<input type=\"date\" id=\"dateAssemblage\" name=\"dateAssemblage\" value=\"\\d{4}-\\d{2}-\\d{2}\" class=\"form-control\"/>")
      content must contain("<textarea id=\"commentaire\" name=\"commentaire\" class=\"form-control\"></textarea>")
      val s=session(r)
      s.get("module") must not equalTo(None)
      s.get("moduleForm") must equalTo(Some("insert"))
      val mod=Module.toModule(s.get("module"))
      mod.id must equalTo("")
      mod.types must equalTo("")
      mod.commentaire must equalTo(None)
      mod.capteurs must equalTo(List[BSONObjectID]())
      mod.cartes must equalTo(List[BSONObjectID]())

      there was one(f.moduleDaoMock).findListType()
    }

    "send bad request with form if field required" in new WithApplication{
      val f=fixture

      f.moduleDaoMock.findListType() returns future{Stream[BSONDocument]()}

      val r=f.controller.addInformation().apply(FakeRequest("POST","/inventary/modules/form").withSession("user" -> """{"login":"test"}""","moduleForm"->"insert"))

      status(r) must equalTo(BAD_REQUEST)
      val content=contentAsString(r)
      content must contains("<span class=\"control-label errors\">This field is required</span>",3)

      there was one(f.moduleDaoMock).findListType()
    }

    "send bad request with form if date is not valid" in new WithApplication{
      val f=fixture
      val data=Json.parse("""{"id":"id","types":"type","dateAssemblage":"a"}""")

      f.moduleDaoMock.findListType() returns future{Stream[BSONDocument]()}

      val r=f.controller.addInformation().apply(FakeRequest("POST","/inventary/modules/form").withJsonBody(data).withSession("user" -> """{"login":"test"}""","moduleForm"->"insert"))

      status(r) must equalTo(BAD_REQUEST)
      val content=contentAsString(r)
      content must contain("<span class=\"control-label errors\">Valid date required</span>")

      there was one(f.moduleDaoMock).findListType()
    }

    "send redirect after insert information if module not exist" in new WithApplication{
      val f=fixture
      val data=Json.parse("""{"id":"id","types":"type","dateAssemblage":"2015-04-22","commentaire":"un com"}""")

      f.moduleDaoMock.findOne(any[JsObject])(any[ExecutionContext]) returns future{None}

      val r=f.controller.addInformation().apply(FakeRequest("POST","/inventary/modules/form").withJsonBody(data).withSession("user" -> """{"login":"test"}"""))

      status(r) must equalTo(SEE_OTHER)
      header("Location",r) must beSome("/inventary/modules/form/typeCards")
      val s=session(r)
      s.get("module") must not equalTo(None)
      val mod=Module.toModule(s.get("module"))
      mod.id must equalTo("id")
      mod.types must equalTo("type")
      mod.commentaire must equalTo(Some("un com"))
      mod.dateAssemblage must equalTo(new Date(115,3,22))
      mod.capteurs must equalTo(List[BSONObjectID]())
      mod.cartes must equalTo(List[BSONObjectID]())

      there was one(f.moduleDaoMock).findOne(any[JsObject])(any[ExecutionContext])
    }

    "send bad request with form if module exist" in new WithApplication{
      val f=fixture
      val data=Json.parse("""{"id":"id","types":"type","dateAssemblage":"2015-04-22","commentaire":"un com"}""")
      val mod=mock[Module]

      f.moduleDaoMock.findOne(any[JsObject])(any[ExecutionContext]) returns future{Some(mod)}
      f.moduleDaoMock.findListType() returns future{Stream[BSONDocument]()}

      val r=f.controller.addInformation().apply(FakeRequest("POST","/inventary/modules/form").withJsonBody(data).withSession("user" -> """{"login":"test"}""","moduleForm"->"insert"))

      status(r) must equalTo(BAD_REQUEST)
      val content=contentAsString(r)
      content must contain("<div class=\"alert alert-danger\" role=\"alert\">Ce module existe déjà</div>")
      content must contain("<input id=\"types\" name=\"types\" class=\"form-control\" list=\"list_type\" autofocus=\"autofocus\" type=\"text\" autocomplete=\"off\" value=\"type\"/>")
      content must contain("<input type=\"text\" id=\"id\" name=\"id\" value=\"id\" class=\"form-control\"/>")
      content must contain("<input type=\"date\" id=\"dateAssemblage\" name=\"dateAssemblage\" value=\"2015-04-22\" class=\"form-control\"/>")
      content must contain("<textarea id=\"commentaire\" name=\"commentaire\" class=\"form-control\">un com</textarea>")

      there was one(f.moduleDaoMock).findOne(any[JsObject])(any[ExecutionContext])
      there was one(f.moduleDaoMock).findListType()
    }

    "send 500 internal error if mongoDB error when find module" in new WithApplication{
      val f=fixture
      val data=Json.parse("""{"id":"id","types":"type","dateAssemblage":"2015-04-22","commentaire":"un com"}""")
      val futureMock=mock[Future[Option[Module]]]
      val throwable=mock[Throwable]

      f.moduleDaoMock.findOne(any[JsObject])(any[ExecutionContext]) returns futureMock
      futureMock.flatMap(any[Option[Module]=>Future[Option[Module]]])(any[ExecutionContext]) returns futureMock
      futureMock.recover(any[PartialFunction[Throwable,Result]])(any[ExecutionContext]) answers {value=>future{value.asInstanceOf[PartialFunction[Throwable,Result]](throwable)}}

      val r=f.controller.addInformation().apply(FakeRequest("POST","/inventary/modules/form").withJsonBody(data).withSession("user" -> """{"login":"test"}"""))

      status(r) must equalTo(INTERNAL_SERVER_ERROR)

      there was one(f.moduleDaoMock).findOne(any[JsObject])(any[ExecutionContext])
      there was one(futureMock).flatMap(any[Option[Module]=>Future[Option[Module]]])(any[ExecutionContext])
      there was one(futureMock).recover(any[PartialFunction[Throwable,Result]])(any[ExecutionContext])
    }
  }

  "When user is on resource /inventary/modules/form/:id, ModuleManager" should {
    "send 200 OK page with prefilled form" in new WithApplication {
      val f=fixture
      val module=Module(bson,"id","types",date,List(bson2),List(bson3),Some("un com"))

      f.moduleDaoMock.findOne(org.mockito.Matchers.eq(Json.obj("delete"->false,"_id"->bson)))(any[ExecutionContext]) returns future{Some(module)}
      f.moduleDaoMock.findListType() returns future{Stream[BSONDocument]()}

      val r=f.controller.formGeneralUpdate(bson.stringify).apply(FakeRequest("GET","/inventary/modules/form/"+bson.stringify).withSession("user" -> """{"login":"test"}"""))

      status(r) must equalTo(OK)
      val content=contentAsString(r)
      content must contain("<input id=\"types\" name=\"types\" class=\"form-control\" list=\"list_type\" readonly=\"readonly\" type=\"text\" autocomplete=\"off\" value=\"types\"/>")
      content must contain("<input type=\"text\" id=\"id\" name=\"id\" value=\"id\" class=\"form-control\" readonly=\"readonly\"/>")
      content must contain("<input type=\"date\" id=\"dateAssemblage\" name=\"dateAssemblage\" value=\"2015-04-22\" class=\"form-control\" readonly=\"readonly\"/>")
      content must contain("<textarea id=\"commentaire\" name=\"commentaire\" class=\"form-control\">un com</textarea>")
      val s=session(r)
      s.get("module") must not equalTo(None)
      s.get("moduleForm") must equalTo(Some("update"))
      val mod=Module.toModule(s.get("module"))
      mod.id must equalTo("id")
      mod.types must equalTo("types")
      mod.commentaire must equalTo(Some("un com"))
      mod.capteurs must equalTo(List[BSONObjectID](bson3))
      mod.cartes must equalTo(List[BSONObjectID](bson2))

      there was one(f.moduleDaoMock).findOne(org.mockito.Matchers.eq(Json.obj("delete"->false,"_id"->bson)))(any[ExecutionContext])
      there was one(f.moduleDaoMock).findListType()
    }

    "send redirect if module not exist" in new WithApplication {
      val f=fixture
      val module=Module(bson,"id","types",date,List(bson2),List(bson3),Some("un com"))

      f.moduleDaoMock.findOne(org.mockito.Matchers.eq(Json.obj("delete"->false,"_id"->bson)))(any[ExecutionContext]) returns future{None}

      val r=f.controller.formGeneralUpdate(bson.stringify).apply(FakeRequest("GET","/inventary/modules/form/"+bson.stringify).withSession("user" -> """{"login":"test"}"""))

      status(r) must equalTo(SEE_OTHER)
      header("Location",r) must beSome("/inventary/modules")
      val s=session(r)
      s.get("module") must equalTo(None)
      s.get("moduleForm") must equalTo(None)

      there was one(f.moduleDaoMock).findOne(org.mockito.Matchers.eq(Json.obj("delete"->false,"_id"->bson)))(any[ExecutionContext])
    }
  }

  "When user is on resource /inventary/modules/form/update, ModuleManager" should {
    "send 200 OK page with prefilled form" in new WithApplication {
      val f=fixture
      val module=Module(bson,"id","types",date,List(bson2),List(bson3),Some("un com"))

      f.moduleDaoMock.findListType() returns future{Stream[BSONDocument]()}

      val r=f.controller.formUpdate().apply(FakeRequest("GET","/inventary/modules/form/update").withSession("user" -> """{"login":"test"}""","module"->Module.toStrings(module),"moduleForm"->"insert"))

      status(r) must equalTo(OK)
      val content=contentAsString(r)
      content must contain("<input id=\"types\" name=\"types\" class=\"form-control\" list=\"list_type\" autofocus=\"autofocus\" type=\"text\" autocomplete=\"off\" value=\"types\"/>")
      content must contain("<input type=\"text\" id=\"id\" name=\"id\" value=\"id\" class=\"form-control\"/>")
      content must contain("<input type=\"date\" id=\"dateAssemblage\" name=\"dateAssemblage\" value=\"2015-04-22\" class=\"form-control\"/>")
      content must contain("<textarea id=\"commentaire\" name=\"commentaire\" class=\"form-control\">un com</textarea>")

      there was one(f.moduleDaoMock).findListType()
    }
  }

  "When user is on resource /inventary/modules/form/validate, ModuleManager" should{
    "send 200 OK page with the summary of module information" in new WithApplication{
      val f=fixture
      val matcher=Json.obj("delete"->false,"_id"->Json.obj("$in"->JsArray(Seq(BSONObjectIDFormat.writes(bson)))))
      val matcher2=Json.obj("delete"->false,"_id"->Json.obj("$in"->JsArray(Seq(BSONObjectIDFormat.writes(bson2)))))
      val typeSensors=TypeSensor(bson2,"typeSensor","modeleSensor","fab",1,List(bson4),false)
      val typeMesure=TypeMesure(bson3,"tension","volt")
      val sensor=Sensor(bson, "Id", bson2, Some(date), date2, Some(date2), true, Some("un com"))
      val typeCards=TypeCards(bson2,"modeleCards","typeCards",false)
      val firmware=Firmware(bson3,"firm","version")
      val cards=Cards(bson, "Id2", bson2, bson3, date, Some(date2), false, Some("v03"), false, Some("un com"))
      val module=Module(bson4,"id","types",date,List(bson),List(bson),Some("un com"))

      f.moduleDaoMock.fold(org.mockito.Matchers.eq(Json.obj("_id"->Json.obj("$ne"->bson4),"delete"->false)),any[JsObject],org.mockito.Matchers.eq(List[BSONObjectID]()))(any[(List[BSONObjectID],Module)=>List[BSONObjectID]])(any[ExecutionContext]) returns future{List[BSONObjectID]()}
      f.sensorsDaoMock.fold(org.mockito.Matchers.eq(matcher),any[JsObject],org.mockito.Matchers.eq(HashSet[BSONObjectID]()))(any[(HashSet[BSONObjectID],Sensor)=>HashSet[BSONObjectID]])(any[ExecutionContext]) returns future{HashSet(bson2)}
      f.typeSensorsDaoMock.findAll(org.mockito.Matchers.eq(matcher2),any[JsObject])(any[ExecutionContext]) returns future{List(typeSensors)}
      f.especeDaoMock.findAll(org.mockito.Matchers.eq(Json.obj("_id"->Json.obj("$in"->List(bson4)))),any[JsObject])(any[ExecutionContext]) returns future{List(Espece(bson4,"esp1",bson3,2.3f,3.4f))}
      f.typeMesureDaoMock.findAll(any[JsObject],any[JsObject])(any[ExecutionContext]) returns future{List(typeMesure)}
      f.sensorsDaoMock.findAll(org.mockito.Matchers.eq(matcher),any[JsObject])(any[ExecutionContext]) returns future{List(sensor)}
      f.cardsDaoMock.fold(org.mockito.Matchers.eq(matcher),any[JsObject],org.mockito.Matchers.eq(HashSet[BSONObjectID]()))(any[(HashSet[BSONObjectID],Cards)=>HashSet[BSONObjectID]])(any[ExecutionContext]) returns future{HashSet(bson2)}
      f.typeCardsDaoMock.findAll(org.mockito.Matchers.eq(matcher2),any[JsObject])(any[ExecutionContext]) returns future{List(typeCards)}
      f.firmwareDaoMock.findAll(any[JsObject],any[JsObject])(any[ExecutionContext]) returns future{List(firmware)}
      f.cardsDaoMock.findAll(org.mockito.Matchers.eq(matcher),any[JsObject])(any[ExecutionContext]) returns future{List(cards)}
      f.moduleDaoMock.findOne(any[JsObject])(any[ExecutionContext]) returns future{None}

      val req=FakeRequest("GET","/inventary/modules/form/validate").withSession("user" -> """{"login":"test"}""","module"->Module.toStrings(module))
      val r=f.controller.validate().apply(req)

      status(r) must equalTo(OK)
      val content=contentAsString(r)
      content must not contain("<div class=\"alert alert-danger\" role=\"alert\">Attention certaines cartes ou capteurs ne sont plus disponibles</div>")
      content must contain("<h4>types / id</h4>")
      content must contain("<div class=\"row\"><span class=\"bold\">Commentaires</span> : un com</div>")
      content must contain("<div class=\"row\"><span class=\"bold\">Date d&#x27;assemblage</span> : 22/04/2015</div>")
      content must contain("<h5 class=\"bold\">typeCards / modeleCards</h5>")
      content must contain("<td>Id2</td>")
      content must contains("<td>22/04/2015</td>",2)
      content must contains("<td>23/04/2015</td>",3)
      content must contain("<td>firm (version)</td>")
      content must contain("<td> Non </td>")
      content must contain("<td>v03</td>")
      content must contain("<h5 class=\"bold\">typeSensor / modeleSensor</h5>")
      content must contain("<div class=\"row\"> <span class=\"bold\">Fabricant</span> : fab</div>")
      content must contain("<div class=\"row\"> <span class=\"bold\">Nombre de signaux</span> : 1</div>")
      content must contain("<td>tension</td>")
      content must contain("<td>esp1</td>")
      content must contain("<td>2.3 volt</td>")
      content must contain("<td>3.4 volt</td>")
      content must contain("<td>Id</td>")
      content must contain("<td>Hors service</td>")

      there was 2.times(f.moduleDaoMock).fold(org.mockito.Matchers.eq(Json.obj("_id"->Json.obj("$ne"->bson4),"delete"->false)),any[JsObject],org.mockito.Matchers.eq(List[BSONObjectID]()))(any[(List[BSONObjectID],Module)=>List[BSONObjectID]])(any[ExecutionContext])
      there was one(f.sensorsDaoMock).fold(org.mockito.Matchers.eq(matcher),any[JsObject],org.mockito.Matchers.eq(HashSet[BSONObjectID]()))(any[(HashSet[BSONObjectID],Sensor)=>HashSet[BSONObjectID]])(any[ExecutionContext])
      there was one(f.typeSensorsDaoMock).findAll(org.mockito.Matchers.eq(matcher2),any[JsObject])(any[ExecutionContext])
      there was one(f.typeMesureDaoMock).findAll(any[JsObject],any[JsObject])(any[ExecutionContext])
      there was one(f.sensorsDaoMock).findAll(org.mockito.Matchers.eq(matcher),any[JsObject])(any[ExecutionContext])
      there was one(f.cardsDaoMock).fold(org.mockito.Matchers.eq(matcher),any[JsObject],org.mockito.Matchers.eq(HashSet[BSONObjectID]()))(any[(HashSet[BSONObjectID],Cards)=>HashSet[BSONObjectID]])(any[ExecutionContext])
      there was one(f.typeCardsDaoMock).findAll(org.mockito.Matchers.eq(matcher2),any[JsObject])(any[ExecutionContext])
      there was one(f.firmwareDaoMock).findAll(any[JsObject],any[JsObject])(any[ExecutionContext])
      there was one(f.cardsDaoMock).findAll(org.mockito.Matchers.eq(matcher),any[JsObject])(any[ExecutionContext])
      there was one(f.moduleDaoMock).findOne(any[JsObject])(any[ExecutionContext])
      there was one(f.especeDaoMock).findAll(org.mockito.Matchers.eq(Json.obj("_id"->Json.obj("$in"->List(bson4)))),any[JsObject])(any[ExecutionContext])
    }

    "send Bad request if not have the good number of cards or sensors" in new WithApplication{
      val f=fixture
      val bson5=BSONObjectID.generate
      val matcher=Json.obj("delete"->false,"_id"->Json.obj("$in"->JsArray(Seq(BSONObjectIDFormat.writes(bson)))))
      val matcher2=Json.obj("delete"->false,"_id"->Json.obj("$in"->JsArray(Seq(BSONObjectIDFormat.writes(bson2)))))
      val typeSensors=TypeSensor(bson2,"typeSensor","modeleSensor","fab",1,List(bson4),false)
      val typeMesure=TypeMesure(bson3,"tension","volt")
      val sensor=Sensor(bson, "Id", bson2, Some(date), date2, Some(date2), true, Some("un com"))
      val typeCards=TypeCards(bson2,"modeleCards","typeCards",false)
      val firmware=Firmware(bson3,"firm","version")
      val cards=Cards(bson, "Id2", bson2, bson3, date, Some(date2), false, Some("v03"), false, Some("un com"))
      val module=Module(bson4,"id","types",date,List(bson,bson5),List(bson,bson5),Some("un com"))

      f.moduleDaoMock.fold(org.mockito.Matchers.eq(Json.obj("_id"->Json.obj("$ne"->bson4),"delete"->false)),any[JsObject],org.mockito.Matchers.eq(List[BSONObjectID]()))(any[(List[BSONObjectID],Module)=>List[BSONObjectID]])(any[ExecutionContext]) returns future{List[BSONObjectID](bson5)}
      f.sensorsDaoMock.fold(org.mockito.Matchers.eq(matcher),any[JsObject],org.mockito.Matchers.eq(HashSet[BSONObjectID]()))(any[(HashSet[BSONObjectID],Sensor)=>HashSet[BSONObjectID]])(any[ExecutionContext]) returns future{HashSet(bson2)}
      f.typeSensorsDaoMock.findAll(org.mockito.Matchers.eq(matcher2),any[JsObject])(any[ExecutionContext]) returns future{List(typeSensors)}
      f.especeDaoMock.findAll(org.mockito.Matchers.eq(Json.obj("_id"->Json.obj("$in"->List(bson4)))),any[JsObject])(any[ExecutionContext]) returns future{List(Espece(bson4,"esp1",bson3,2.3f,3.4f))}
      f.typeMesureDaoMock.findAll(any[JsObject],any[JsObject])(any[ExecutionContext]) returns future{List(typeMesure)}
      f.sensorsDaoMock.findAll(org.mockito.Matchers.eq(matcher),any[JsObject])(any[ExecutionContext]) returns future{List(sensor)}
      f.cardsDaoMock.fold(org.mockito.Matchers.eq(matcher),any[JsObject],org.mockito.Matchers.eq(HashSet[BSONObjectID]()))(any[(HashSet[BSONObjectID],Cards)=>HashSet[BSONObjectID]])(any[ExecutionContext]) returns future{HashSet(bson2)}
      f.typeCardsDaoMock.findAll(org.mockito.Matchers.eq(matcher2),any[JsObject])(any[ExecutionContext]) returns future{List(typeCards)}
      f.firmwareDaoMock.findAll(any[JsObject],any[JsObject])(any[ExecutionContext]) returns future{List(firmware)}
      f.cardsDaoMock.findAll(org.mockito.Matchers.eq(matcher),any[JsObject])(any[ExecutionContext]) returns future{List(cards)}

      val req=FakeRequest("GET","/inventary/modules/form/validate").withSession("user" -> """{"login":"test"}""","module"->Module.toStrings(module))
      val r=f.controller.validate().apply(req)

      status(r) must equalTo(BAD_REQUEST)
      val content=contentAsString(r)
      content must contain("<div class=\"alert alert-danger\" role=\"alert\">Attention certaines cartes ou capteurs ne sont plus disponibles</div>")

      there was 2.times(f.moduleDaoMock).fold(org.mockito.Matchers.eq(Json.obj("_id"->Json.obj("$ne"->bson4),"delete"->false)),any[JsObject],org.mockito.Matchers.eq(List[BSONObjectID]()))(any[(List[BSONObjectID],Module)=>List[BSONObjectID]])(any[ExecutionContext])
      there was one(f.sensorsDaoMock).fold(org.mockito.Matchers.eq(matcher),any[JsObject],org.mockito.Matchers.eq(HashSet[BSONObjectID]()))(any[(HashSet[BSONObjectID],Sensor)=>HashSet[BSONObjectID]])(any[ExecutionContext])
      there was one(f.typeSensorsDaoMock).findAll(org.mockito.Matchers.eq(matcher2),any[JsObject])(any[ExecutionContext])
      there was one(f.typeMesureDaoMock).findAll(any[JsObject],any[JsObject])(any[ExecutionContext])
      there was one(f.sensorsDaoMock).findAll(org.mockito.Matchers.eq(matcher),any[JsObject])(any[ExecutionContext])
      there was one(f.cardsDaoMock).fold(org.mockito.Matchers.eq(matcher),any[JsObject],org.mockito.Matchers.eq(HashSet[BSONObjectID]()))(any[(HashSet[BSONObjectID],Cards)=>HashSet[BSONObjectID]])(any[ExecutionContext])
      there was one(f.typeCardsDaoMock).findAll(org.mockito.Matchers.eq(matcher2),any[JsObject])(any[ExecutionContext])
      there was one(f.firmwareDaoMock).findAll(any[JsObject],any[JsObject])(any[ExecutionContext])
      there was one(f.cardsDaoMock).findAll(org.mockito.Matchers.eq(matcher),any[JsObject])(any[ExecutionContext])
      there was one(f.especeDaoMock).findAll(org.mockito.Matchers.eq(Json.obj("_id"->Json.obj("$in"->List(bson4)))),any[JsObject])(any[ExecutionContext])
    }

    "send redirect if session moduleForm not equals to Some(\"insert\") or Some(\"update\")" in new WithApplication{
      val f=fixture

      val r=f.controller.insertModule().apply(FakeRequest("POST","/inventary/modules/form/validate").withSession("user" -> """{"login":"test"}"""))

      status(r) must equalTo(SEE_OTHER)
      header("Location",r) must beSome("/inventary/modules/form/validate")
    }

    "send redirect after insert module" in new WithApplication{
      val f=fixture
      val module=Module(bson,"id","types",date,List(bson2),List(bson2),Some("un com"))
      val cardsMock=mock[Cards]
      val sensorsMock=mock[Sensor]
      val lastError=mock[LastError]

      f.moduleDaoMock.fold(org.mockito.Matchers.eq(Json.obj("_id"->Json.obj("$ne"->bson),"delete"->false)),any[JsObject],org.mockito.Matchers.eq(List[BSONObjectID]()))(any[(List[BSONObjectID],Module)=>List[BSONObjectID]])(any[ExecutionContext]) returns future{List[BSONObjectID](bson3)}
      f.cardsDaoMock.findAll(any[JsObject],any[JsObject])(any[ExecutionContext]) returns future{List(cardsMock)}
      f.sensorsDaoMock.findAll(any[JsObject],any[JsObject])(any[ExecutionContext]) returns future{List(sensorsMock)}
      f.moduleDaoMock.findOne(any[JsObject])(any[ExecutionContext]) returns future{None}
      f.moduleDaoMock.insert(org.mockito.Matchers.eq(module),any[GetLastError])(any[ExecutionContext]) returns future{lastError}

      val r=f.controller.insertModule().apply(FakeRequest("POST","/inventary/modules/form/validate").withSession("user" -> """{"login":"test"}""","moduleForm"->"insert","module"->Module.toStrings(module)))

      status(r) must equalTo(SEE_OTHER)
      header("Location",r) must beSome("/inventary/modules")

      there was 2.times(f.moduleDaoMock).fold(org.mockito.Matchers.eq(Json.obj("_id"->Json.obj("$ne"->bson),"delete"->false)),any[JsObject],org.mockito.Matchers.eq(List[BSONObjectID]()))(any[(List[BSONObjectID],Module)=>List[BSONObjectID]])(any[ExecutionContext])
      there was one(f.cardsDaoMock).findAll(any[JsObject],any[JsObject])(any[ExecutionContext])
      there was one(f.sensorsDaoMock).findAll(any[JsObject],any[JsObject])(any[ExecutionContext])
      there was one(f.moduleDaoMock).findOne(any[JsObject])(any[ExecutionContext])
      there was one(f.moduleDaoMock).insert(org.mockito.Matchers.eq(module),any[GetLastError])(any[ExecutionContext])
    }

    "send redirect after update module" in new WithApplication{
      val f=fixture
      val module=Module(bson,"id","types",date,List(bson2),List(bson2),Some("un com"))
      val cardsMock=mock[Cards]
      val sensorsMock=mock[Sensor]
      val lastError=mock[LastError]

      f.moduleDaoMock.fold(org.mockito.Matchers.eq(Json.obj("_id"->Json.obj("$ne"->bson),"delete"->false)),any[JsObject],org.mockito.Matchers.eq(List[BSONObjectID]()))(any[(List[BSONObjectID],Module)=>List[BSONObjectID]])(any[ExecutionContext]) returns future{List[BSONObjectID](bson3)}
      f.cardsDaoMock.findAll(any[JsObject],any[JsObject])(any[ExecutionContext]) returns future{List(cardsMock)}
      f.sensorsDaoMock.findAll(any[JsObject],any[JsObject])(any[ExecutionContext]) returns future{List(sensorsMock)}
      f.moduleDaoMock.findOne(any[JsObject])(any[ExecutionContext]) returns future{None}
      f.moduleDaoMock.updateById(org.mockito.Matchers.eq(bson),org.mockito.Matchers.eq(module),any[GetLastError])(any[Writes[Module]],any[ExecutionContext]) returns future{lastError}

      val r=f.controller.insertModule().apply(FakeRequest("POST","/inventary/modules/form/validate").withSession("user" -> """{"login":"test"}""","moduleForm"->"update","module"->Module.toStrings(module)))

      status(r) must equalTo(SEE_OTHER)
      header("Location",r) must beSome("/inventary/modules")

      there was 2.times(f.moduleDaoMock).fold(org.mockito.Matchers.eq(Json.obj("_id"->Json.obj("$ne"->bson),"delete"->false)),any[JsObject],org.mockito.Matchers.eq(List[BSONObjectID]()))(any[(List[BSONObjectID],Module)=>List[BSONObjectID]])(any[ExecutionContext])
      there was one(f.cardsDaoMock).findAll(any[JsObject],any[JsObject])(any[ExecutionContext])
      there was one(f.sensorsDaoMock).findAll(any[JsObject],any[JsObject])(any[ExecutionContext])
      there was one(f.moduleDaoMock).findOne(any[JsObject])(any[ExecutionContext])
      there was one(f.moduleDaoMock).updateById(org.mockito.Matchers.eq(bson),org.mockito.Matchers.eq(module),any[GetLastError])(any[Writes[Module]],any[ExecutionContext])
    }
  }

  "When user is on resource /inventary/modules/:id/delete, ModuleManager" should{
    "send redirect if module not found" in new WithApplication{
      val f=fixture

      f.moduleDaoMock.findOne(org.mockito.Matchers.eq(Json.obj("delete"->false,"_id"->bson)))(any[ExecutionContext]) returns future{None}

      val r=f.controller.deleteModule(bson.stringify).apply(FakeRequest("POST","/inventary/modules/"+bson.stringify+"/delete").withSession("user" -> """{"login":"test"}"""))

      status(r) must equalTo(SEE_OTHER)
      header("Location",r) must beSome("/inventary/modules")

      there was one(f.moduleDaoMock).findOne(org.mockito.Matchers.eq(Json.obj("delete"->false,"_id"->bson)))(any[ExecutionContext])
      there was no(f.moduleDaoMock).updateById(any[BSONObjectID],any[Module],any[GetLastError])(any[Writes[Module]],any[ExecutionContext])
    }

    "send redirect after delete module" in new WithApplication{
      val f=fixture
      val module=Module(bson,"id","types",date,List(bson2),List(bson3),Some("un com"))
      val lastError=mock[LastError]

      f.moduleDaoMock.findOne(org.mockito.Matchers.eq(Json.obj("delete"->false,"_id"->bson)))(any[ExecutionContext]) returns future{Some(module)}
      f.moduleDaoMock.updateById(org.mockito.Matchers.eq(bson),org.mockito.Matchers.eq(module.copy(delete=true)),any[GetLastError])(any[Writes[Module]],any[ExecutionContext]) returns future{lastError}

      val r=f.controller.deleteModule(bson.stringify).apply(FakeRequest("POST","/inventary/modules/"+bson.stringify+"/delete").withSession("user" -> """{"login":"test"}"""))

      status(r) must equalTo(SEE_OTHER)
      header("Location",r) must beSome("/inventary/modules")

      there was one(f.moduleDaoMock).findOne(org.mockito.Matchers.eq(Json.obj("delete"->false,"_id"->bson)))(any[ExecutionContext])
      there was one(f.moduleDaoMock).updateById(org.mockito.Matchers.eq(bson),org.mockito.Matchers.eq(module.copy(delete=true)),any[GetLastError])(any[Writes[Module]],any[ExecutionContext])
    }
  }

  "When user is on resource /inventary/modules/form/cards/:id/delete, ModuleManager" should {
    "send redirect after delete cards in the module" in new WithApplication{
      val f=fixture
      val module=Module(bson,"id","types",date,List(bson2,bson3),List(bson2,bson3),Some("un com"))

      val req=FakeRequest("POST","/inventary/modules/form/cards/"+bson2.stringify+"/delete").withSession("user" -> """{"login":"test"}""","module"->Module.toStrings(module))
      val r=f.controller.deleteCards(bson2.stringify).apply(req)
      val s=session(r)
      s.get("module") must beSome(Module.toStrings(module.copy(cartes=List(bson3))))

      status(r) must equalTo(SEE_OTHER)
      header("Location",r) must beSome("/inventary/modules/form/validate")
    }
  }

  "When user is on resource /inventary/modules/form/sensors/:id/delete, ModuleManager" should {
    "send redirect after delete sensors in the module" in new WithApplication{
      val f=fixture
      val module=Module(bson,"id","types",date,List(bson2,bson3),List(bson2,bson3),Some("un com"))

      val req=FakeRequest("POST","/inventary/modules/form/sensors/"+bson2.stringify+"/delete").withSession("user" -> """{"login":"test"}""","module"->Module.toStrings(module))
      val r=f.controller.deleteSensors(bson2.stringify).apply(req)
      val s=session(r)
      s.get("module") must beSome(Module.toStrings(module.copy(capteurs=List(bson3))))

      status(r) must equalTo(SEE_OTHER)
      header("Location",r) must beSome("/inventary/modules/form/validate")
    }
  }

  "When user is on resource /inventary/modules/:id/information, ModuleManager" should {
    "send a 404 not found if condition not found" in new WithApplication{
      val f=fixture

      f.conditionDaoMock.findOne(any[JsObject])(any[ExecutionContext]) returns future{None}

      val req=FakeRequest("GET","/inventary/modules/"+bson.stringify+"/information").withSession("user" -> """{"login":"test"}""")
      val r=f.controller.getModuleInformation(bson.stringify).apply(req)

      status(r) must equalTo(NOT_FOUND)

      there was one(f.conditionDaoMock).findOne(any[JsObject])(any[ExecutionContext])
    }

    "send a 404 not found if campaign not found" in new WithApplication{
      val f=fixture
      val cond=mock[Condition]

      cond._id returns bson2
      f.conditionDaoMock.findOne(any[JsObject])(any[ExecutionContext]) returns future{Some(cond)}
      f.campaignDaoMock.findOne(org.mockito.Matchers.eq(Json.obj("conditions"->cond._id)))(any[ExecutionContext]) returns future{None}

      val req=FakeRequest("GET","/inventary/modules/"+bson.stringify+"/information").withSession("user" -> """{"login":"test"}""")
      val r=f.controller.getModuleInformation(bson.stringify).apply(req)

      status(r) must equalTo(NOT_FOUND)

      there was one(f.conditionDaoMock).findOne(any[JsObject])(any[ExecutionContext])
      there was one(f.campaignDaoMock).findOne(org.mockito.Matchers.eq(Json.obj("conditions"->cond._id)))(any[ExecutionContext])
    }

    "send a JSON without the localisation" in new WithApplication{
      val f=fixture
      val cond=mock[Condition]
      val camp=mock[Campagne]

      cond._id returns bson2
      camp.dataToken returns "token"
      camp.collectUrl returns "collectUrl"
      camp.version returns 1
      f.conditionDaoMock.findOne(any[JsObject])(any[ExecutionContext]) returns future{Some(cond)}
      f.campaignDaoMock.findOne(org.mockito.Matchers.eq(Json.obj("conditions"->cond._id)))(any[ExecutionContext]) returns future{Some(camp)}
      f.localisationDaoMock.findOne(org.mockito.Matchers.eq(Json.obj("condition"->cond._id)))(any[ExecutionContext]) returns future{None}

      val req=FakeRequest("GET","/inventary/modules/"+bson.stringify+"/information").withSession("user" -> """{"login":"test"}""")
      val r=f.controller.getModuleInformation(bson.stringify).apply(req)

      status(r) must equalTo(OK)
      contentAsJson(r) must equalTo(Json.obj("dataToken"->"token","collectUrl"->"collectUrl/1","condition"->bson2.stringify))

      there was one(f.conditionDaoMock).findOne(any[JsObject])(any[ExecutionContext])
      there was one(f.campaignDaoMock).findOne(org.mockito.Matchers.eq(Json.obj("conditions"->cond._id)))(any[ExecutionContext])
      there was one(f.localisationDaoMock).findOne(org.mockito.Matchers.eq(Json.obj("condition"->cond._id)))(any[ExecutionContext])
    }

    "send a JSON" in new WithApplication{
      val f=fixture
      val cond=mock[Condition]
      val camp=mock[Campagne]
      val loc=mock[Localisation]

      cond._id returns bson2
      camp.dataToken returns "token"
      camp.collectUrl returns "collectUrl"
      camp.version returns 1
      loc.lat returns Some(2.3f)
      loc.lon returns Some(4.5f)
      f.conditionDaoMock.findOne(any[JsObject])(any[ExecutionContext]) returns future{Some(cond)}
      f.campaignDaoMock.findOne(org.mockito.Matchers.eq(Json.obj("conditions"->cond._id)))(any[ExecutionContext]) returns future{Some(camp)}
      f.localisationDaoMock.findOne(org.mockito.Matchers.eq(Json.obj("condition"->cond._id)))(any[ExecutionContext]) returns future{Some(loc)}

      val req=FakeRequest("GET","/inventary/modules/"+bson.stringify+"/information").withSession("user" -> """{"login":"test"}""")
      val r=f.controller.getModuleInformation(bson.stringify).apply(req)

      status(r) must equalTo(OK)
      contentAsJson(r) must equalTo(Json.obj("dataToken"->"token","collectUrl"->"collectUrl/1","condition"->bson2.stringify,"latitude"->2.3f,"longitude"->4.5f))

      there was one(f.conditionDaoMock).findOne(any[JsObject])(any[ExecutionContext])
      there was one(f.campaignDaoMock).findOne(org.mockito.Matchers.eq(Json.obj("conditions"->cond._id)))(any[ExecutionContext])
      there was one(f.localisationDaoMock).findOne(org.mockito.Matchers.eq(Json.obj("condition"->cond._id)))(any[ExecutionContext])
    }
  }

  "When method getInventaryModule is called, ModuleManager" should{
    "apply view function and not consider the filtre" in new WithApplication{
      val f=fixture
      val view=mock[(List[Module],List[BSONDocument])=>Result]

      f.moduleDaoMock.findAll(org.mockito.Matchers.eq(Json.obj("delete"->false)),any[JsObject])(any[ExecutionContext]) returns future{List[Module]()}
      f.moduleDaoMock.findListType() returns future{Stream[BSONDocument]()}
      view.apply(org.mockito.Matchers.eq(List[Module]()),org.mockito.Matchers.eq(List[BSONDocument]())) returns Results.Ok("apply func")

      val r=f.controller.getInventaryModule("","")(view)

      status(r) must equalTo(OK)
      contentAsString(r) must equalTo("apply func")

      there was one(f.moduleDaoMock).findAll(org.mockito.Matchers.eq(Json.obj("delete"->false)),any[JsObject])(any[ExecutionContext])
      there was one(f.moduleDaoMock).findListType()
      view.apply(org.mockito.Matchers.eq(List[Module]()),org.mockito.Matchers.eq(List[BSONDocument]())) returns Results.Ok("apply func")
    }

    "apply view function and consider the type filtre" in new WithApplication{
      val f=fixture
      val view=mock[(List[Module],List[BSONDocument])=>Result]

      f.moduleDaoMock.findAll(org.mockito.Matchers.eq(Json.obj("delete"->false,"types"->"filtre")),any[JsObject])(any[ExecutionContext]) returns future{List[Module]()}
      f.moduleDaoMock.findListType() returns future{Stream[BSONDocument]()}
      view.apply(org.mockito.Matchers.eq(List[Module]()),org.mockito.Matchers.eq(List[BSONDocument]())) returns Results.Ok("apply func")

      val r=f.controller.getInventaryModule("filtre","")(view)

      status(r) must equalTo(OK)
      contentAsString(r) must equalTo("apply func")

      there was one(f.moduleDaoMock).findAll(org.mockito.Matchers.eq(Json.obj("delete"->false,"types"->"filtre")),any[JsObject])(any[ExecutionContext])
      there was one(f.moduleDaoMock).findListType()
      view.apply(org.mockito.Matchers.eq(List[Module]()),org.mockito.Matchers.eq(List[BSONDocument]())) returns Results.Ok("apply func")
    }

    "apply view function and consider the id filtre" in new WithApplication{
      val f=fixture
      val view=mock[(List[Module],List[BSONDocument])=>Result]

      f.moduleDaoMock.findAll(org.mockito.Matchers.eq(Json.obj("delete"->false,"id"->Json.obj("$regex"->".*id.*"))),any[JsObject])(any[ExecutionContext]) returns future{List[Module]()}
      f.moduleDaoMock.findListType() returns future{Stream[BSONDocument]()}
      view.apply(org.mockito.Matchers.eq(List[Module]()),org.mockito.Matchers.eq(List[BSONDocument]())) returns Results.Ok("apply func")

      val r=f.controller.getInventaryModule("","id")(view)

      status(r) must equalTo(OK)
      contentAsString(r) must equalTo("apply func")

      there was one(f.moduleDaoMock).findAll(org.mockito.Matchers.eq(Json.obj("delete"->false,"id"->Json.obj("$regex"->".*id.*"))),any[JsObject])(any[ExecutionContext])
      there was one(f.moduleDaoMock).findListType()
      view.apply(org.mockito.Matchers.eq(List[Module]()),org.mockito.Matchers.eq(List[BSONDocument]())) returns Results.Ok("apply func")
    }

    "apply view function and consider filtres" in new WithApplication{
      val f=fixture
      val view=mock[(List[Module],List[BSONDocument])=>Result]

      f.moduleDaoMock.findAll(org.mockito.Matchers.eq(Json.obj("delete"->false,"types"->"filtre","id"->Json.obj("$regex"->".*id.*"))),any[JsObject])(any[ExecutionContext]) returns future{List[Module]()}
      f.moduleDaoMock.findListType() returns future{Stream[BSONDocument]()}
      view.apply(org.mockito.Matchers.eq(List[Module]()),org.mockito.Matchers.eq(List[BSONDocument]())) returns Results.Ok("apply func")

      val r=f.controller.getInventaryModule("filtre","id")(view)

      status(r) must equalTo(OK)
      contentAsString(r) must equalTo("apply func")

      there was one(f.moduleDaoMock).findAll(org.mockito.Matchers.eq(Json.obj("delete"->false,"types"->"filtre","id"->Json.obj("$regex"->".*id.*"))),any[JsObject])(any[ExecutionContext])
      there was one(f.moduleDaoMock).findListType()
      view.apply(org.mockito.Matchers.eq(List[Module]()),org.mockito.Matchers.eq(List[BSONDocument]())) returns Results.Ok("apply func")
    }
  }

  "When method submitForm is called, ModuleManager" should{
    "send redirect if module information error" in new WithApplication{
      val f=fixture
      val module=Module(bson,"id","types",date,List(bson2,bson3),List(bson2,bson3),Some("un com"))
      val cardsMock=mock[Cards]
      val sensorsMock=mock[Sensor]
      val func=mock[Module=>Future[Result]]

      f.moduleDaoMock.fold(org.mockito.Matchers.eq(Json.obj("_id"->Json.obj("$ne"->bson),"delete"->false)),any[JsObject],org.mockito.Matchers.eq(List[BSONObjectID]()))(any[(List[BSONObjectID],Module)=>List[BSONObjectID]])(any[ExecutionContext]) returns future{List[BSONObjectID](bson3)}
      f.cardsDaoMock.findAll(any[JsObject],any[JsObject])(any[ExecutionContext]) returns future{List(cardsMock)}
      f.sensorsDaoMock.findAll(any[JsObject],any[JsObject])(any[ExecutionContext]) returns future{List()}

      val r=f.controller.submitForm(func)(FakeRequest("POST","/inventary/modules/form/validate").withSession("user" -> """{"login":"test"}""","moduleForm"->"insert","module"->Module.toStrings(module)))

      status(r) must equalTo(SEE_OTHER)
      header("Location",r) must beSome("/inventary/modules/form/validate")

      there was 2.times(f.moduleDaoMock).fold(org.mockito.Matchers.eq(Json.obj("_id"->Json.obj("$ne"->bson),"delete"->false)),any[JsObject],org.mockito.Matchers.eq(List[BSONObjectID]()))(any[(List[BSONObjectID],Module)=>List[BSONObjectID]])(any[ExecutionContext])
      there was one(f.cardsDaoMock).findAll(any[JsObject],any[JsObject])(any[ExecutionContext])
      there was one(f.sensorsDaoMock).findAll(any[JsObject],any[JsObject])(any[ExecutionContext])
    }

    "send redirect after insert module" in new WithApplication{
      val f=fixture
      val module=Module(bson,"id","types",date,List(bson2),List(bson2),Some("un com"))
      val cardsMock=mock[Cards]
      val sensorsMock=mock[Sensor]
      val func=mock[Module=>Future[Result]]

      f.moduleDaoMock.fold(org.mockito.Matchers.eq(Json.obj("_id"->Json.obj("$ne"->bson),"delete"->false)),any[JsObject],org.mockito.Matchers.eq(List[BSONObjectID]()))(any[(List[BSONObjectID],Module)=>List[BSONObjectID]])(any[ExecutionContext]) returns future{List[BSONObjectID](bson3)}
      f.cardsDaoMock.findAll(any[JsObject],any[JsObject])(any[ExecutionContext]) returns future{List(cardsMock)}
      f.sensorsDaoMock.findAll(any[JsObject],any[JsObject])(any[ExecutionContext]) returns future{List(sensorsMock)}
      f.moduleDaoMock.findOne(any[JsObject])(any[ExecutionContext]) returns future{None}
      func.apply(org.mockito.Matchers.eq(module)) returns future{Results.Ok("func")}

      val r=f.controller.submitForm(func)(FakeRequest("POST","/inventary/modules/form/validate").withSession("user" -> """{"login":"test"}""","moduleForm"->"insert","module"->Module.toStrings(module)))

      status(r) must equalTo(OK)
      contentAsString(r) must equalTo("func")

      there was 2.times(f.moduleDaoMock).fold(org.mockito.Matchers.eq(Json.obj("_id"->Json.obj("$ne"->bson),"delete"->false)),any[JsObject],org.mockito.Matchers.eq(List[BSONObjectID]()))(any[(List[BSONObjectID],Module)=>List[BSONObjectID]])(any[ExecutionContext])
      there was one(f.cardsDaoMock).findAll(any[JsObject],any[JsObject])(any[ExecutionContext])
      there was one(f.sensorsDaoMock).findAll(any[JsObject],any[JsObject])(any[ExecutionContext])
      there was one(f.moduleDaoMock).findOne(any[JsObject])(any[ExecutionContext])
      there was one(func).apply(org.mockito.Matchers.eq(module))
    }
  }

  "When method verifyData is called, ModuleManager" should{
    "send redirect if module id is empty" in new WithApplication{
      val f=fixture
      val mod=Module(bson,"","types",date,List(),List(),Some("un com"))
      val funcError=mock[(Module,String)=>Future[Result]]
      val funcSuccess=mock[Module=>Future[Result]]

      val req=FakeRequest("GET","url")
      val action=Action.async{f.controller.verifyData(mod,List(),List())(funcError)(funcSuccess)}
      val r=call(action,req)

      status(r) must equalTo(SEE_OTHER)
      header("Location",r) must beSome("/inventary/modules/form/update")
    }

    "send redirect if module type is empty" in new WithApplication{
      val f=fixture
      val mod=Module(bson,"id","",date,List(),List(),Some("un com"))
      val funcError=mock[(Module,String)=>Future[Result]]
      val funcSuccess=mock[Module=>Future[Result]]

      val req=FakeRequest("GET","url")
      val action=Action.async{f.controller.verifyData(mod,List(),List())(funcError)(funcSuccess)}
      val r=call(action,req)

      status(r) must equalTo(SEE_OTHER)
      header("Location",r) must beSome("/inventary/modules/form/update")
    }

    "send redirect if not have cards selected" in new WithApplication{
      val f=fixture
      val mod=Module(bson,"id","type",date,List(),List(),Some("un com"))
      val funcError=mock[(Module,String)=>Future[Result]]
      val funcSuccess=mock[Module=>Future[Result]]

      val req=FakeRequest("GET","url")
      val action=Action.async{f.controller.verifyData(mod,List(),List())(funcError)(funcSuccess)}
      val r=call(action,req)

      status(r) must equalTo(SEE_OTHER)
      header("Location",r) must beSome("/inventary/modules/form/typeCards")
    }

    "apply error function if not have the good number of cards" in new WithApplication{
      val f=fixture
      val mod=Module(bson,"id","type",date,List(bson,bson2),List(),Some("un com"))
      val funcError=mock[(Module,String)=>Future[Result]]
      val funcSuccess=mock[Module=>Future[Result]]
      val cards=Cards(bson, "Id2", bson2, bson3, date, Some(date2), false, Some("v03"), false, Some("un com"))

      funcError.apply(org.mockito.Matchers.eq(mod.copy(cartes=List(bson))),anyString) returns future{Results.BadRequest("func error")}

      val req=FakeRequest("GET","url")
      val action=Action.async{f.controller.verifyData(mod,List(cards),List())(funcError)(funcSuccess)}
      val r=call(action,req)

      status(r) must equalTo(BAD_REQUEST)
      contentAsString(r) must equalTo("func error")

      there was one(funcError).apply(org.mockito.Matchers.eq(mod.copy(cartes=List(bson))),anyString)
    }

    "apply error function if not have the good number of sensors" in new WithApplication{
      val f=fixture
      val mod=Module(bson,"id","type",date,List(bson3),List(bson,bson2),Some("un com"))
      val funcError=mock[(Module,String)=>Future[Result]]
      val funcSuccess=mock[Module=>Future[Result]]
      val cards=Cards(bson3, "Id2", bson2, bson3, date, Some(date2), false, Some("v03"), false, Some("un com"))
      val sensors=Sensor(bson, "Id", bson2, Some(date), date2, Some(date2), true, Some("un com"))

      funcError.apply(org.mockito.Matchers.eq(mod.copy(capteurs=List(bson))),anyString) returns future{Results.BadRequest("func error")}

      val req=FakeRequest("GET","url")
      val action=Action.async{f.controller.verifyData(mod,List(cards),List(sensors))(funcError)(funcSuccess)}
      val r=call(action,req)

      status(r) must equalTo(BAD_REQUEST)
      contentAsString(r) must equalTo("func error")

      there was one(funcError).apply(org.mockito.Matchers.eq(mod.copy(capteurs=List(bson))),anyString)
    }

    "apply success function if not have error" in new WithApplication{
      val f=fixture
      val mod=Module(bson,"id","type",date,List(bson3),List(bson),Some("un com"))
      val funcError=mock[(Module,String)=>Future[Result]]
      val funcSuccess=mock[Module=>Future[Result]]
      val cardsMock=mock[Cards]
      val sensorsMock=mock[Sensor]

      f.moduleDaoMock.findOne(any[JsObject])(any[ExecutionContext]) returns future{None}
      funcSuccess.apply(org.mockito.Matchers.eq(mod)) returns future{Results.Ok("func success")}

      val req=FakeRequest("GET","url")
      val action=Action.async{f.controller.verifyData(mod,List(cardsMock),List(sensorsMock))(funcError)(funcSuccess)}
      val r=call(action,req)

      status(r) must equalTo(OK)
      contentAsString(r) must equalTo("func success")

      there was one(funcSuccess).apply(org.mockito.Matchers.eq(mod))
      there was one(f.moduleDaoMock).findOne(any[JsObject])(any[ExecutionContext])
    }

    "apply error function if module exist" in new WithApplication{
      val f=fixture
      val mod=Module(bson,"id","type",date,List(bson3),List(bson),Some("un com"))
      val funcError=mock[(Module,String)=>Future[Result]]
      val funcSuccess=mock[Module=>Future[Result]]
      val cardsMock=mock[Cards]
      val sensorsMock=mock[Sensor]

      f.moduleDaoMock.findOne(any[JsObject])(any[ExecutionContext]) returns future{Some(mod)}
      funcError.apply(org.mockito.Matchers.eq(mod),anyString) returns future{Results.BadRequest("func error")}

      val req=FakeRequest("GET","url")
      val action=Action.async{f.controller.verifyData(mod,List(cardsMock),List(sensorsMock))(funcError)(funcSuccess)}
      val r=call(action,req)

      status(r) must equalTo(BAD_REQUEST)
      contentAsString(r) must equalTo("func error")

      there was one(funcError).apply(org.mockito.Matchers.eq(mod),anyString)
      there was one(f.moduleDaoMock).findOne(any[JsObject])(any[ExecutionContext])
    }
  }

  "When method addCardsToModule is called, ModuleManager" should{
    "send redirect to /inventary/modules/form/typeCards and add cards to module in session" in new WithApplication{
      val f=fixture
      val info=SelectInfo(List[String](bson.stringify),"Envoyer")

      f.cardsDaoMock.fold(any[JsObject],any[JsObject],org.mockito.Matchers.eq(List[BSONObjectID]()))(any[(List[BSONObjectID],Cards)=>List[BSONObjectID]])(any[ExecutionContext]) returns future{List(bson)}

      val req=FakeRequest("POST","url")
      val action=Action.async{implicit request => f.controller.addCardsToModule(info)}
      val r=call(action,req)

      status(r) must equalTo(SEE_OTHER)
      header("Location",r) must beSome(("/inventary/modules/form/typeCards"))
      val s=session(r)
      s.get("module") must not equalTo(None)
      val jsonModule=Json.parse(s.get("module").getOrElse(""))
      (jsonModule\"cartes").as[List[BSONObjectID]] must contain(bson)

      there was one(f.cardsDaoMock).fold(any[JsObject],any[JsObject],org.mockito.Matchers.eq(List[BSONObjectID]()))(any[(List[BSONObjectID],Cards)=>List[BSONObjectID]])(any[ExecutionContext])
    }

    "send redirect to /inventary/modules/form/typeSensors and add cards to module in session" in new WithApplication{
      val f=fixture
      val info=SelectInfo(List[String](bson.stringify),"Passer à l'étape suivante")

      f.cardsDaoMock.fold(any[JsObject],any[JsObject],org.mockito.Matchers.eq(List[BSONObjectID]()))(any[(List[BSONObjectID],Cards)=>List[BSONObjectID]])(any[ExecutionContext]) returns future{List(bson)}

      val req=FakeRequest("POST","url")
      val action=Action.async{implicit request => f.controller.addCardsToModule(info)}
      val r=call(action,req)

      status(r) must equalTo(SEE_OTHER)
      header("Location",r) must beSome(("/inventary/modules/form/typeSensors"))
      val s=session(r)
      s.get("module") must not equalTo(None)
      val jsonModule=Json.parse(s.get("module").getOrElse(""))
      (jsonModule\"cartes").as[List[BSONObjectID]] must contain(bson)

      there was one(f.cardsDaoMock).fold(any[JsObject],any[JsObject],org.mockito.Matchers.eq(List[BSONObjectID]()))(any[(List[BSONObjectID],Cards)=>List[BSONObjectID]])(any[ExecutionContext])
    }

    "send redirect to /inventary/modules/form/typeCards and add cards to module in session without delete cards add before" in new WithApplication{
      val f=fixture
      val info=SelectInfo(List[String](bson.stringify),"Envoyer")

      f.cardsDaoMock.fold(any[JsObject],any[JsObject],org.mockito.Matchers.eq(List[BSONObjectID]()))(any[(List[BSONObjectID],Cards)=>List[BSONObjectID]])(any[ExecutionContext]) returns future{List(bson)}

      val req=FakeRequest("POST","url").withSession("module"->Module.toStrings(Module(id="",types="",dateAssemblage=new Date,cartes=List(bson2),capteurs=List(),commentaire=None)))
      val action=Action.async{implicit request => f.controller.addCardsToModule(info)}
      val r=call(action,req)

      status(r) must equalTo(SEE_OTHER)
      header("Location",r) must beSome(("/inventary/modules/form/typeCards"))
      val s=session(r)
      s.get("module") must not equalTo(None)
      val jsonModule=Json.parse(s.get("module").getOrElse(""))
      (jsonModule\"cartes").as[List[BSONObjectID]] must contain(bson)
      (jsonModule\"cartes").as[List[BSONObjectID]] must contain(bson2)

      there was one(f.cardsDaoMock).fold(any[JsObject],any[JsObject],org.mockito.Matchers.eq(List[BSONObjectID]()))(any[(List[BSONObjectID],Cards)=>List[BSONObjectID]])(any[ExecutionContext])
    }

    "send 500 internal error if mongoDB error when verify module _id" in new WithApplication{
      val f=fixture
      val info=SelectInfo(List[String](bson.stringify),"Envoyer")
      val futureMock=mock[Future[List[BSONObjectID]]]
      val throwable=mock[Throwable]

      f.cardsDaoMock.fold(any[JsObject],any[JsObject],org.mockito.Matchers.eq(List[BSONObjectID]()))(any[(List[BSONObjectID],Cards)=>List[BSONObjectID]])(any[ExecutionContext]) returns futureMock
      futureMock.map(any[List[BSONObjectID]=>List[BSONObjectID]])(any[ExecutionContext]) returns futureMock
      futureMock.recover(any[PartialFunction[Throwable,Result]])(any[ExecutionContext]) answers {value => future{value.asInstanceOf[PartialFunction[Throwable,Result]](throwable)}}

      val req=FakeRequest("POST","url")
      val action=Action.async{implicit request => f.controller.addCardsToModule(info)}
      val r=call(action,req)

      status(r) must equalTo(INTERNAL_SERVER_ERROR)

      there was one(f.cardsDaoMock).fold(any[JsObject],any[JsObject],org.mockito.Matchers.eq(List[BSONObjectID]()))(any[(List[BSONObjectID],Cards)=>List[BSONObjectID]])(any[ExecutionContext])
      there was one(futureMock).map(any[List[BSONObjectID]=>List[BSONObjectID]])(any[ExecutionContext])
      there was one(futureMock).recover(any[PartialFunction[Throwable,Result]])(any[ExecutionContext])
    }
  }

  "When method addSensorsToModule is called, ModuleManager" should{
    "send redirect to /inventary/modules/form/typeSensors and add sensors to module in session" in new WithApplication{
      val f=fixture
      val info=SelectInfo(List[String](bson.stringify),"Envoyer")

      f.sensorsDaoMock.fold(any[JsObject],any[JsObject],org.mockito.Matchers.eq(List[BSONObjectID]()))(any[(List[BSONObjectID],Sensor)=>List[BSONObjectID]])(any[ExecutionContext]) returns future{List(bson)}

      val req=FakeRequest("POST","url")
      val action=Action.async{implicit request => f.controller.addSensorsToModule(info)}
      val r=call(action,req)

      status(r) must equalTo(SEE_OTHER)
      header("Location",r) must beSome(("/inventary/modules/form/typeSensors"))
      val s=session(r)
      s.get("module") must not equalTo(None)
      val jsonModule=Json.parse(s.get("module").getOrElse(""))
      (jsonModule\"capteurs").as[List[BSONObjectID]] must contain(bson)

      there was one(f.sensorsDaoMock).fold(any[JsObject],any[JsObject],org.mockito.Matchers.eq(List[BSONObjectID]()))(any[(List[BSONObjectID],Sensor)=>List[BSONObjectID]])(any[ExecutionContext])
    }

    "send redirect to /inventary/modules/form/validate and add sensors to module in session" in new WithApplication{
      val f=fixture
      val info=SelectInfo(List[String](bson.stringify),"Passer à l'étape suivante")

      f.sensorsDaoMock.fold(any[JsObject],any[JsObject],org.mockito.Matchers.eq(List[BSONObjectID]()))(any[(List[BSONObjectID],Sensor)=>List[BSONObjectID]])(any[ExecutionContext]) returns future{List(bson)}

      val req=FakeRequest("POST","url")
      val action=Action.async{implicit request => f.controller.addSensorsToModule(info)}
      val r=call(action,req)

      status(r) must equalTo(SEE_OTHER)
      header("Location",r) must beSome(("/inventary/modules/form/validate"))
      val s=session(r)
      s.get("module") must not equalTo(None)
      val jsonModule=Json.parse(s.get("module").getOrElse(""))
      (jsonModule\"capteurs").as[List[BSONObjectID]] must contain(bson)

      there was one(f.sensorsDaoMock).fold(any[JsObject],any[JsObject],org.mockito.Matchers.eq(List[BSONObjectID]()))(any[(List[BSONObjectID],Sensor)=>List[BSONObjectID]])(any[ExecutionContext])
    }

    "send redirect to /inventary/modules/form/typeSensors and add sensors to module in session without delete sensors add before" in new WithApplication{
      val f=fixture
      val info=SelectInfo(List[String](bson.stringify),"Envoyer")

      f.sensorsDaoMock.fold(any[JsObject],any[JsObject],org.mockito.Matchers.eq(List[BSONObjectID]()))(any[(List[BSONObjectID],Sensor)=>List[BSONObjectID]])(any[ExecutionContext]) returns future{List(bson)}

      val req=FakeRequest("POST","url").withSession("module"->Module.toStrings(Module(id="",types="",dateAssemblage=new Date,cartes=List(),capteurs=List(bson2),commentaire=None)))
      val action=Action.async{implicit request => f.controller.addSensorsToModule(info)}
      val r=call(action,req)

      status(r) must equalTo(SEE_OTHER)
      header("Location",r) must beSome(("/inventary/modules/form/typeSensors"))
      val s=session(r)
      s.get("module") must not equalTo(None)
      val jsonModule=Json.parse(s.get("module").getOrElse(""))
      (jsonModule\"capteurs").as[List[BSONObjectID]] must contain(bson)
      (jsonModule\"capteurs").as[List[BSONObjectID]] must contain(bson2)

      there was one(f.sensorsDaoMock).fold(any[JsObject],any[JsObject],org.mockito.Matchers.eq(List[BSONObjectID]()))(any[(List[BSONObjectID],Sensor)=>List[BSONObjectID]])(any[ExecutionContext])
    }

    "send 500 internal error if mongoDB error when verify module _id" in new WithApplication{
      val f=fixture
      val info=SelectInfo(List[String](bson.stringify),"Envoyer")
      val futureMock=mock[Future[List[BSONObjectID]]]
      val throwable=mock[Throwable]

      f.sensorsDaoMock.fold(any[JsObject],any[JsObject],org.mockito.Matchers.eq(List[BSONObjectID]()))(any[(List[BSONObjectID],Sensor)=>List[BSONObjectID]])(any[ExecutionContext]) returns futureMock
      futureMock.map(any[List[BSONObjectID]=>List[BSONObjectID]])(any[ExecutionContext]) returns futureMock
      futureMock.recover(any[PartialFunction[Throwable,Result]])(any[ExecutionContext]) answers {value => future{value.asInstanceOf[PartialFunction[Throwable,Result]](throwable)}}

      val req=FakeRequest("POST","url")
      val action=Action.async{implicit request => f.controller.addSensorsToModule(info)}
      val r=call(action,req)

      status(r) must equalTo(INTERNAL_SERVER_ERROR)

      there was one(f.sensorsDaoMock).fold(any[JsObject],any[JsObject],org.mockito.Matchers.eq(List[BSONObjectID]()))(any[(List[BSONObjectID],Sensor)=>List[BSONObjectID]])(any[ExecutionContext])
      there was one(futureMock).map(any[List[BSONObjectID]=>List[BSONObjectID]])(any[ExecutionContext])
      there was one(futureMock).recover(any[PartialFunction[Throwable,Result]])(any[ExecutionContext])
    }
  }

  "When method printForm was called, ModuleManager" should{
    "send page with a form" in new WithApplication{
      val f=fixture

      f.moduleDaoMock.findListType() returns future{Stream[BSONDocument]()}

      val req=FakeRequest("POST","/inventary/modules/form/").withSession("user" -> """{"login":"test"}""").withSession("moduleForm"->"insert")
      val action=Action.async{implicit request=>f.controller.printForm(ModuleManager.form,Results.Ok,req.session)}
      val r=call(action,req)

      status(r) must equalTo(OK)
      val content=contentAsString(r)
      content must contain("<input id=\"types\" name=\"types\" class=\"form-control\" list=\"list_type\" autofocus=\"autofocus\" type=\"text\" autocomplete=\"off\" value=\"\"/>")
      content must contain("<input type=\"text\" id=\"id\" name=\"id\" value=\"\" class=\"form-control\"/>")
      content must contain("<input type=\"date\" id=\"dateAssemblage\" name=\"dateAssemblage\" value=\"\" class=\"form-control\"/>")
      content must contain("<textarea id=\"commentaire\" name=\"commentaire\" class=\"form-control\"></textarea>")

      there was one(f.moduleDaoMock).findListType()
    }

    "send internal server error if mongoDB error when find list type name" in new WithApplication{
      val f=fixture
      val futureMock=mock[Future[Stream[BSONDocument]]]
      val throwable=mock[Throwable]

      f.moduleDaoMock.findListType() returns futureMock
      futureMock.map(any[Stream[BSONDocument]=>Stream[BSONDocument]])(any[ExecutionContext]) returns futureMock
      futureMock.recover(any[PartialFunction[Throwable,Result]])(any[ExecutionContext]) answers {value => future{value.asInstanceOf[PartialFunction[Throwable,Result]](throwable)}}

      val req=FakeRequest("POST","/inventary/modules/form/").withSession("user" -> """{"login":"test"}""")
      val action=Action.async{implicit request=>f.controller.printForm(ModuleManager.form,Results.Ok,req.session)}
      val r=call(action,req)

      status(r) must equalTo(INTERNAL_SERVER_ERROR)

      there was one(f.moduleDaoMock).findListType()
      there was one(futureMock).map(any[Stream[BSONDocument]=>Stream[BSONDocument]])(any[ExecutionContext])
      there was one(futureMock).recover(any[PartialFunction[Throwable,Result]])(any[ExecutionContext])
    }
  }

  "When method findCardsInfo was called, ModuleManager" should{
    "get cards selected and their associat information" in new WithApplication{
      val f=fixture
      val matcher=Json.obj("delete"->false,"_id"->Json.obj("$in"->JsArray(Seq(BSONObjectIDFormat.writes(bson)))))
      val matcher2=Json.obj("delete"->false,"_id"->Json.obj("$in"->JsArray(Seq(BSONObjectIDFormat.writes(bson2)))))
      val typeCards=TypeCards(bson2,"modele","type",false)
      val firmware=Firmware(bson3,"firm","version")
      val cards=mock[Cards]
      val module=Module(bson4,"id","types",new Date,List(bson),List(),None)

      f.cardsDaoMock.fold(org.mockito.Matchers.eq(matcher),any[JsObject],org.mockito.Matchers.eq(HashSet[BSONObjectID]()))(any[(HashSet[BSONObjectID],Cards)=>HashSet[BSONObjectID]])(any[ExecutionContext]) returns future{HashSet(bson2)}
      f.typeCardsDaoMock.findAll(org.mockito.Matchers.eq(matcher2),any[JsObject])(any[ExecutionContext]) returns future{List(typeCards)}
      f.firmwareDaoMock.findAll(any[JsObject],any[JsObject])(any[ExecutionContext]) returns future{List(firmware)}
      f.cardsDaoMock.findAll(org.mockito.Matchers.eq(matcher),any[JsObject])(any[ExecutionContext]) returns future{List(cards)}

      val data=f.controller.findCardsInfo(module)
      val value=Await.result(data,Duration.Inf)
      value must equalTo((List(typeCards),List(cards),List(firmware)))

      there was one(f.cardsDaoMock).fold(org.mockito.Matchers.eq(matcher),any[JsObject],org.mockito.Matchers.eq(HashSet[BSONObjectID]()))(any[(HashSet[BSONObjectID],Cards)=>HashSet[BSONObjectID]])(any[ExecutionContext])
      there was one(f.typeCardsDaoMock).findAll(org.mockito.Matchers.eq(matcher2),any[JsObject])(any[ExecutionContext])
      there was one(f.firmwareDaoMock).findAll(any[JsObject],any[JsObject])(any[ExecutionContext])
      there was one(f.cardsDaoMock).findAll(org.mockito.Matchers.eq(matcher),any[JsObject])(any[ExecutionContext])
    }

    "get cards selected but not on an other module and their associat information" in new WithApplication{
      val f=fixture
      val bson5=BSONObjectID.generate
      val matcher=Json.obj("delete"->false,"_id"->Json.obj("$in"->JsArray(Seq(BSONObjectIDFormat.writes(bson)))))
      val matcher2=Json.obj("delete"->false,"_id"->Json.obj("$in"->JsArray(Seq(BSONObjectIDFormat.writes(bson2)))))
      val typeCards=TypeCards(bson2,"modele","type",false)
      val firmware=Firmware(bson3,"firm","version")
      val cards=mock[Cards]
      val module=Module(bson4,"id","types",new Date,List(bson,bson5),List(),None)

      f.cardsDaoMock.fold(org.mockito.Matchers.eq(matcher),any[JsObject],org.mockito.Matchers.eq(HashSet[BSONObjectID]()))(any[(HashSet[BSONObjectID],Cards)=>HashSet[BSONObjectID]])(any[ExecutionContext]) returns future{HashSet(bson2)}
      f.typeCardsDaoMock.findAll(org.mockito.Matchers.eq(matcher2),any[JsObject])(any[ExecutionContext]) returns future{List(typeCards)}
      f.firmwareDaoMock.findAll(any[JsObject],any[JsObject])(any[ExecutionContext]) returns future{List(firmware)}
      f.cardsDaoMock.findAll(org.mockito.Matchers.eq(matcher),any[JsObject])(any[ExecutionContext]) returns future{List(cards)}

      val data=f.controller.findCardsInfo(module,List(bson5))
      val value=Await.result(data,Duration.Inf)
      value must equalTo((List(typeCards),List(cards),List(firmware)))

      there was one(f.cardsDaoMock).fold(org.mockito.Matchers.eq(matcher),any[JsObject],org.mockito.Matchers.eq(HashSet[BSONObjectID]()))(any[(HashSet[BSONObjectID],Cards)=>HashSet[BSONObjectID]])(any[ExecutionContext])
      there was one(f.typeCardsDaoMock).findAll(org.mockito.Matchers.eq(matcher2),any[JsObject])(any[ExecutionContext])
      there was one(f.firmwareDaoMock).findAll(any[JsObject],any[JsObject])(any[ExecutionContext])
      there was one(f.cardsDaoMock).findAll(org.mockito.Matchers.eq(matcher),any[JsObject])(any[ExecutionContext])
    }
  }

  "When method findSensorsInfo was called, ModuleManager" should{
    "get sensors selected and their associat information" in new WithApplication{
      val f=fixture
      val matcher=Json.obj("delete"->false,"_id"->Json.obj("$in"->JsArray(Seq(BSONObjectIDFormat.writes(bson)))))
      val matcher2=Json.obj("delete"->false,"_id"->Json.obj("$in"->JsArray(Seq(BSONObjectIDFormat.writes(bson2)))))
      val typeSensors=TypeSensor(bson2,"type","modele","fab",1,List(bson4),false)
      val typeMesure=TypeMesure(bson3,"tension","volt")
      val sensor=mock[Sensor]
      val module=Module(bson4,"id","types",new Date,List(),List(bson),None)
      val espece=Espece(bson4,"esp1",bson3,2.3f,3.4f)

      f.sensorsDaoMock.fold(org.mockito.Matchers.eq(matcher),any[JsObject],org.mockito.Matchers.eq(HashSet[BSONObjectID]()))(any[(HashSet[BSONObjectID],Sensor)=>HashSet[BSONObjectID]])(any[ExecutionContext]) returns future{HashSet(bson2)}
      f.typeSensorsDaoMock.findAll(org.mockito.Matchers.eq(matcher2),any[JsObject])(any[ExecutionContext]) returns future{List(typeSensors)}
      f.typeMesureDaoMock.findAll(any[JsObject],any[JsObject])(any[ExecutionContext]) returns future{List(typeMesure)}
      f.sensorsDaoMock.findAll(org.mockito.Matchers.eq(matcher),any[JsObject])(any[ExecutionContext]) returns future{List(sensor)}
      f.especeDaoMock.findAll(org.mockito.Matchers.eq(Json.obj("_id"->Json.obj("$in"->List(bson4)))),any[JsObject])(any[ExecutionContext]) returns future{List(espece)}

      val data=f.controller.findSensorsInfo(module)
      val value=Await.result(data,Duration.Inf)
      value must equalTo((List(typeSensors),List(espece),List(typeMesure),List(sensor)))

      there was one(f.sensorsDaoMock).fold(org.mockito.Matchers.eq(matcher),any[JsObject],org.mockito.Matchers.eq(HashSet[BSONObjectID]()))(any[(HashSet[BSONObjectID],Sensor)=>HashSet[BSONObjectID]])(any[ExecutionContext])
      there was one(f.typeSensorsDaoMock).findAll(org.mockito.Matchers.eq(matcher2),any[JsObject])(any[ExecutionContext])
      there was one(f.typeMesureDaoMock).findAll(any[JsObject],any[JsObject])(any[ExecutionContext])
      there was one(f.sensorsDaoMock).findAll(org.mockito.Matchers.eq(matcher),any[JsObject])(any[ExecutionContext])
      there was one(f.especeDaoMock).findAll(org.mockito.Matchers.eq(Json.obj("_id"->Json.obj("$in"->List(bson4)))),any[JsObject])(any[ExecutionContext])
    }

    "get sensors selected but not on an other module and their associat information" in new WithApplication{
      val f=fixture
      val bson5=BSONObjectID.generate
      val matcher=Json.obj("delete"->false,"_id"->Json.obj("$in"->JsArray(Seq(BSONObjectIDFormat.writes(bson)))))
      val matcher2=Json.obj("delete"->false,"_id"->Json.obj("$in"->JsArray(Seq(BSONObjectIDFormat.writes(bson2)))))
      val typeSensors=TypeSensor(bson2,"type","modele","fab",1,List(bson4),false)
      val typeMesure=TypeMesure(bson3,"tension","volt")
      val espece=Espece(bson4,"esp1",bson3,2.3f,3.4f)
      val sensor=mock[Sensor]
      val module=Module(bson4,"id","types",new Date,List(),List(bson,bson5),None)

      f.sensorsDaoMock.fold(org.mockito.Matchers.eq(matcher),any[JsObject],org.mockito.Matchers.eq(HashSet[BSONObjectID]()))(any[(HashSet[BSONObjectID],Sensor)=>HashSet[BSONObjectID]])(any[ExecutionContext]) returns future{HashSet(bson2)}
      f.typeSensorsDaoMock.findAll(org.mockito.Matchers.eq(matcher2),any[JsObject])(any[ExecutionContext]) returns future{List(typeSensors)}
      f.typeMesureDaoMock.findAll(any[JsObject],any[JsObject])(any[ExecutionContext]) returns future{List(typeMesure)}
      f.sensorsDaoMock.findAll(org.mockito.Matchers.eq(matcher),any[JsObject])(any[ExecutionContext]) returns future{List(sensor)}
      f.especeDaoMock.findAll(org.mockito.Matchers.eq(Json.obj("_id"->Json.obj("$in"->List(bson4)))),any[JsObject])(any[ExecutionContext]) returns future{List(espece)}

      val data=f.controller.findSensorsInfo(module,List(bson5))
      val value=Await.result(data,Duration.Inf)
      value must equalTo((List(typeSensors),List(espece),List(typeMesure),List(sensor)))

      there was one(f.sensorsDaoMock).fold(org.mockito.Matchers.eq(matcher),any[JsObject],org.mockito.Matchers.eq(HashSet[BSONObjectID]()))(any[(HashSet[BSONObjectID],Sensor)=>HashSet[BSONObjectID]])(any[ExecutionContext])
      there was one(f.typeSensorsDaoMock).findAll(org.mockito.Matchers.eq(matcher2),any[JsObject])(any[ExecutionContext])
      there was one(f.typeMesureDaoMock).findAll(any[JsObject],any[JsObject])(any[ExecutionContext])
      there was one(f.sensorsDaoMock).findAll(org.mockito.Matchers.eq(matcher),any[JsObject])(any[ExecutionContext])
      there was one(f.especeDaoMock).findAll(org.mockito.Matchers.eq(Json.obj("_id"->Json.obj("$in"->List(bson4)))),any[JsObject])(any[ExecutionContext])
    }
  }

  "When method getPreviousPage is called, ModulesManager" should{
    "return the url if the url is found and match with the pattern" in new WithApplication{
      val f=fixture
      val url="http://hostname/inventary/modules?types=typ"

      val req=FakeRequest(GET,"url")
      val r=f.controller.getPreviousPage("http://hostname/",Some(url))(req)

      r must equalTo(url)
    }

    "return the url to /inventary/modules if the url is found and not match with the pattern" in new WithApplication{
      val f=fixture
      val url="http://hostname/inventary/modules/"+bson.stringify

      val req=FakeRequest(GET,"url")
      val r=f.controller.getPreviousPage("http://hostname/",Some(url))(req)

      r must equalTo("/inventary/modules")
    }

    "return the url to /inventary/modules if the url is not found" in new WithApplication{
      val f=fixture

      val req=FakeRequest(GET,"url")
      val r=f.controller.getPreviousPage("http://hostname/",None)(req)

      r must equalTo("/inventary/modules")
    }

    "return the url to /inventary/modules if the url is not start with the host name" in new WithApplication{
      val f=fixture
      val url="http://hostname2/inventary/modules"

      val req=FakeRequest(GET,"url")
      val r=f.controller.getPreviousPage("http://hostname/",Some(url))(req)

      r must equalTo("/inventary/modules")
    }
  }

  "When method previousPage is called, ModulesManager" should {
    "return the url if the hostname and the url are found and the url match with the pattern" in new WithApplication {
      val f = fixture
      val url = "http://hostname/inventary/modules?types=typ"

      f.configMock.getString(org.mockito.Matchers.eq("hostname"), any[Option[Set[String]]]) returns Some("http://hostname/")

      val req = FakeRequest(GET, "url").withHeaders(("Referer" -> url))
      val r = f.controller.previousPage(req)

      r must equalTo(url)

      there was one(f.configMock).getString(org.mockito.Matchers.eq("hostname"), any[Option[Set[String]]])
    }

    "throw an exception if the hostname not found" in new WithApplication {
      val f = fixture

      f.configMock.getString(org.mockito.Matchers.eq("hostname"), any[Option[Set[String]]]) returns None

      val req = FakeRequest(GET, "url")
      f.controller.previousPage(req) must throwA[Exception]

      there was one(f.configMock).getString(org.mockito.Matchers.eq("hostname"), any[Option[Set[String]]])
    }
  }
}