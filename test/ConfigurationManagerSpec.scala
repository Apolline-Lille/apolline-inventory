import java.util.Date

import controllers.{InfoMesureForm, ModuleManagerLike, ConfigurationForm, ConfigurationManagerLike}
import models._
import org.junit.runner.RunWith
import org.specs2.matcher.{MatchResult, Expectable, Matcher}
import org.specs2.mock.Mockito
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner
import play.api.libs.json.{JsArray, JsObject, Json}
import play.api.mvc.{Results, Result}
import play.api.test.Helpers._
import play.api.test.{FakeRequest, WithApplication}
import play.modules.reactivemongo.json.BSONFormats.BSONObjectIDFormat
import reactivemongo.bson.BSONObjectID

import scala.collection.immutable.HashSet
import scala.concurrent.{ExecutionContext, Future}
import scala.concurrent._

@RunWith(classOf[JUnitRunner])
class ConfigurationManagerSpec extends Specification with Mockito{

  class ConfigurationManagerTest extends ConfigurationManagerLike

  case class contains(a: String, b: Int) extends Matcher[String]() {
    override def apply[S <: String](t: Expectable[S]): MatchResult[S] = result(a.r.findAllMatchIn(t.value).size == b, "okMessage", "not found " + b + " times but " + a.r.findAllMatchIn(t.value).size + " times " + a, t)
  }

  case class matchRegex(a: String) extends Matcher[String]() {
    override def apply[S <: String](t: Expectable[S]): MatchResult[S] = result(a.r.findFirstIn(t.value).nonEmpty, "okMessage", t.value + "not found " + a, t)
  }

  def fixture=new {
    val sensorDaoMock = mock[SensorDao]
    val typeSensorDaoMock=mock[TypeSensorDao]
    val typeMesureDaoMock=mock[TypeMesureDao]
    val moduleManagerMock = mock[ModuleManagerLike]
    val controller = new ConfigurationManagerTest{
      override val sensorsDao=sensorDaoMock
      override val typeSensorsDao=typeSensorDaoMock
      override val typeMesureDao=typeMesureDaoMock
      override val moduleManager=moduleManagerMock
    }

    def applyFoundFunction() {
      moduleManagerMock.doIfModuleFound(org.mockito.Matchers.eq(bson))(any[Module => Future[Result]])(any[Unit => Future[Result]]) answers { (params, _) => params match {
        case Array(_, p: (Module => Future[Result]), _) => p.apply(Module(bson,"id","type",date,List(bson2),List(bson3,bson4),Some("un com")))
      }}
    }

    def applyNotFoundFunction() {
      moduleManagerMock.doIfModuleFound(org.mockito.Matchers.eq(bson))(any[Module => Future[Result]])(any[Unit => Future[Result]]) answers { (params, _) => params match {
        case Array(_,_, p: (Unit => Future[Result])) => p.apply()
      }}
    }
  }

  val date=new Date(115,3,22)
  val date2=new Date(115,3,23)

  val bson=BSONObjectID.generate
  val bson2=BSONObjectID.generate
  val bson3=BSONObjectID.generate
  val bson4=BSONObjectID.generate

  "When user is not connected, ModuleManager" should {
    "redirect to login for resource /inventary/modules/:id/configuration" in new WithApplication {
      route(FakeRequest(GET, "/inventary/modules/"+bson.stringify+"/configuration")).map(
        r => {
          status(r) must equalTo(SEE_OTHER)
          header("Location", r) must equalTo(Some("/login"))
        }
      ).getOrElse(
          failure("Pas de retour de la fonction")
        )
    }

    "redirect to login for resource /inventary/modules/:id/configuration" in new WithApplication {
      route(FakeRequest(POST, "/inventary/modules/"+bson.stringify+"/configuration")).map(
        r => {
          status(r) must equalTo(SEE_OTHER)
          header("Location", r) must equalTo(Some("/login"))
        }
      ).getOrElse(
          failure("Pas de retour de la fonction")
        )
    }

    "redirect to login for resource /inventary/modules/:id/configuration/sensors" in new WithApplication {
      route(FakeRequest(GET, "/inventary/modules/"+bson.stringify+"/configuration/sensors")).map(
        r => {
          status(r) must equalTo(SEE_OTHER)
          header("Location", r) must equalTo(Some("/login"))
        }
      ).getOrElse(
          failure("Pas de retour de la fonction")
        )
    }

    "redirect to login for resource /inventary/modules/:id/configuration/sensors/:id2" in new WithApplication {
      route(FakeRequest(GET, "/inventary/modules/"+bson.stringify+"/configuration/sensors/"+bson2.stringify)).map(
        r => {
          status(r) must equalTo(SEE_OTHER)
          header("Location", r) must equalTo(Some("/login"))
        }
      ).getOrElse(
          failure("Pas de retour de la fonction")
        )
    }
  }

  "When user is on resource /inventary/modules/:id/configuration, ConfigurationManager" should{
    "send 200 Ok page with a prefilled form" in new WithApplication{
      val f=fixture

      f.applyFoundFunction()

      val r=f.controller.formConfiguration(bson.stringify).apply(FakeRequest(GET, "/inventary/modules/"+bson.stringify+"/configuration").withSession("user" -> """{"login":"test"}"""))
      status(r) must equalTo(OK)
      val content=contentAsString(r)
      content must contain("<h4>type / id</h4>")
      content must contain("<span class=\"bold\">Commentaires</span> : un com")
      content must matchRegex("<span class=\"bold\">Date d&#x27;assemblage</span> : \\d{2}/\\d{2}/\\d{4}")
      content must contain("<input type=\"text\" id=\"port\" name=\"port\" value=\"\" autofocus=\"autofocus\" autocomplete=\"off\" class=\"form-control\"/>")
      content must contain("<input id=\"timeout\" name=\"timeout\" class=\"form-control\" type=\"number\" autocomplete=\"off\" value=\"10000\"/>")
      content must contain("<input id=\"baud\" name=\"baud\" class=\"form-control\" type=\"number\" autocomplete=\"off\" value=\"9600\"/>")
      content must contain("<input id=\"bits\" name=\"bits\" class=\"form-control\" type=\"number\" autocomplete=\"off\" value=\"8\"/>")
      content must contain("<option value=\"0\" selected=\"selected\">None</option>")
      content must contain("<option value=\"1\" >Odd</option>")
      content must contain("<option value=\"2\" >Even</option>")
      content must contain("<option value=\"3\" >Mark</option>")
      content must contain("<option value=\"4\" >Space</option>")
      content must contain("<input id=\"stopBits\" name=\"stopBits\" class=\"form-control\" type=\"number\" autocomplete=\"off\" value=\"1\"/>")
      content must contain("<input id=\"timeFilter\" name=\"timeFilter\" class=\"form-control\" type=\"number\" autocomplete=\"off\" value=\"1000\"/>")
      content must contain("<input type=\"text\" id=\"types\" name=\"types\" value=\"\" class=\"form-control\"/>")

      there was one(f.moduleManagerMock).doIfModuleFound(org.mockito.Matchers.eq(bson))(any[Module=>Future[Result]])(any[Unit=>Future[Result]])
    }

    "send redirect if module not found" in new WithApplication{
      val f=fixture

      f.applyNotFoundFunction()

      val r=f.controller.formConfiguration(bson.stringify).apply(FakeRequest(GET, "/inventary/modules/"+bson.stringify+"/configuration").withSession("user" -> """{"login":"test"}"""))
      status(r) must equalTo(SEE_OTHER)
      header("Location",r) must beSome("/inventary/modules")

      there was one(f.moduleManagerMock).doIfModuleFound(org.mockito.Matchers.eq(bson))(any[Module=>Future[Result]])(any[Unit=>Future[Result]])
    }

    "send redirect after insert configuration" in new WithApplication{
      val data=Json.obj("port"->"/dev/ttyUSB0","timeout"->10000,"baud"->9600,"bits"->8,"stopBits"->1,"parity"->0,"timeFilter"->1000,"types"->"ADC")
      val f=fixture

      f.applyFoundFunction()

      val r=f.controller.insertFormConfiguration(bson.stringify).apply(FakeRequest(POST, "/inventary/modules/"+bson.stringify+"/configuration").withJsonBody(data).withSession("user" -> """{"login":"test"}"""))
      status(r) must equalTo(SEE_OTHER)
      header("Location",r) must beSome("/inventary/modules/"+bson.stringify+"/configuration/sensors")
      val s=session(r)
      s.get("configForm") must beSome("insert")
      s.get("config") must beSome(Json.stringify(data))

      there was one(f.moduleManagerMock).doIfModuleFound(org.mockito.Matchers.eq(bson))(any[Module=>Future[Result]])(any[Unit=>Future[Result]])
    }

    "send bad request if form is send with no data" in new WithApplication{
      val f=fixture

      f.applyFoundFunction()

      val r=f.controller.insertFormConfiguration(bson.stringify).apply(FakeRequest(POST, "/inventary/modules/"+bson.stringify+"/configuration").withSession("user" -> """{"login":"test"}"""))
      status(r) must equalTo(BAD_REQUEST)
      contentAsString(r) must contains("<span class=\"control-label errors\">This field is required</span>",8)

      there was one(f.moduleManagerMock).doIfModuleFound(org.mockito.Matchers.eq(bson))(any[Module=>Future[Result]])(any[Unit=>Future[Result]])
    }

    "send bad request if number ar not valid" in new WithApplication{
      val data=Json.obj("port"->"/dev/ttyUSB0","timeout"->"a","baud"->"a","bits"->"a","stopBits"->"a","parity"->"a","timeFilter"->"a","types"->"ADC")
      val f=fixture

      f.applyFoundFunction()

      val r=f.controller.insertFormConfiguration(bson.stringify).apply(FakeRequest(POST, "/inventary/modules/"+bson.stringify+"/configuration").withJsonBody(data).withSession("user" -> """{"login":"test"}"""))
      status(r) must equalTo(BAD_REQUEST)
      contentAsString(r) must contains("<span class=\"control-label errors\">Numeric value expected</span>",6)

      there was one(f.moduleManagerMock).doIfModuleFound(org.mockito.Matchers.eq(bson))(any[Module=>Future[Result]])(any[Unit=>Future[Result]])
    }

    "send redirect if module not found" in new WithApplication{
      val f=fixture

      f.applyNotFoundFunction()

      val r=f.controller.insertFormConfiguration(bson.stringify).apply(FakeRequest(GET, "/inventary/modules/"+bson.stringify+"/configuration").withSession("user" -> """{"login":"test"}"""))
      status(r) must equalTo(SEE_OTHER)
      header("Location",r) must beSome("/inventary/modules")

      there was one(f.moduleManagerMock).doIfModuleFound(org.mockito.Matchers.eq(bson))(any[Module=>Future[Result]])(any[Unit=>Future[Result]])
    }
  }

  "When user is on resource /inventary/modules/:id/configuration/sensors, ConfigurationManager" should{
    "send 200 OK page with the message 'Aucun résultat trouvé'" in new WithApplication{
      val f=fixture

      f.applyFoundFunction()
      f.sensorDaoMock.fold(any[JsObject],any[JsObject],org.mockito.Matchers.eq(HashSet[BSONObjectID]()))(any[(HashSet[BSONObjectID],Sensor)=>HashSet[BSONObjectID]])(any[ExecutionContext]) returns future{HashSet[BSONObjectID]()}
      f.typeSensorDaoMock.findAll(any[JsObject],any[JsObject])(any[ExecutionContext]) returns future{List()}
      f.sensorDaoMock.findAll(any[JsObject],any[JsObject])(any[ExecutionContext]) returns future{List()}
      f.typeMesureDaoMock.findAll(any[JsObject],any[JsObject])(any[ExecutionContext]) returns future{List()}

      val r=f.controller.listSensors(bson.stringify).apply(FakeRequest(GET, "/inventary/modules/"+bson.stringify+"/configuration/sensors").withSession("user" -> """{"login":"test"}"""))

      status(r) must equalTo(OK)
      val content=contentAsString(r)
      content must contain("<h4>type / id</h4>")
      content must contain("<span class=\"bold\">Commentaires</span> : un com")
      content must matchRegex("<span class=\"bold\">Date d&#x27;assemblage</span> : \\d{2}/\\d{2}/\\d{4}")
      content must contain("<h3 style=\"text-align:center\">Aucun résultat trouvé</h3>")

      there was one(f.moduleManagerMock).doIfModuleFound(org.mockito.Matchers.eq(bson))(any[Module=>Future[Result]])(any[Unit=>Future[Result]])
      there was one(f.sensorDaoMock).fold(any[JsObject],any[JsObject],org.mockito.Matchers.eq(HashSet[BSONObjectID]()))(any[(HashSet[BSONObjectID],Sensor)=>HashSet[BSONObjectID]])(any[ExecutionContext])
      there was one(f.typeSensorDaoMock).findAll(any[JsObject],any[JsObject])(any[ExecutionContext])
      there was one(f.sensorDaoMock).findAll(any[JsObject],any[JsObject])(any[ExecutionContext])
      there was one(f.typeMesureDaoMock).findAll(any[JsObject],any[JsObject])(any[ExecutionContext])
    }

    "send 200 OK page with results" in new WithApplication{
      val f=fixture
      val typeSensor=List(
        TypeSensor(bson2,"typeSensor","nomTypeSensor",bson3,"fab",1,List("esp"),2.3f,3.4f),
        TypeSensor(bson3,"typeSensor2","nomTypeSensor2",bson2,"fab2",2,List("esp2"),3.4f,2.3f)
      )
      val typeMesure=List(
        TypeMesure(bson3,"mesure1","unite1"),
        TypeMesure(bson2,"mesure2","unite2")
      )
      val sensor=List(
        Sensor(bson3,"id",bson3,None,date,None,true,None),
        Sensor(bson4,"id2",bson2,Some(date),date2,Some(date),false,Some("un com2"))
      )

      f.applyFoundFunction()
      f.sensorDaoMock.fold(any[JsObject],any[JsObject],org.mockito.Matchers.eq(HashSet[BSONObjectID]()))(any[(HashSet[BSONObjectID],Sensor)=>HashSet[BSONObjectID]])(any[ExecutionContext]) returns future{HashSet[BSONObjectID]()}
      f.typeSensorDaoMock.findAll(any[JsObject],any[JsObject])(any[ExecutionContext]) returns future{typeSensor}
      f.sensorDaoMock.findAll(any[JsObject],any[JsObject])(any[ExecutionContext]) returns future{sensor}
      f.typeMesureDaoMock.findAll(any[JsObject],any[JsObject])(any[ExecutionContext]) returns future{typeMesure}

      val r=f.controller.listSensors(bson.stringify).apply(FakeRequest(GET, "/inventary/modules/"+bson.stringify+"/configuration/sensors").withSession("user" -> """{"login":"test"}"""))

      status(r) must equalTo(OK)
      val content=contentAsString(r)
      content must contain("<h4>type / id</h4>")
      content must contain("<span class=\"bold\">Commentaires</span> : un com")
      content must matchRegex("<span class=\"bold\">Date d&#x27;assemblage</span> : \\d{2}/\\d{2}/\\d{4}")
      content must contain("<h5 class=\"bold\">typeSensor / nomTypeSensor</h5>")
      content must contain("<span class=\"bold\">Fabricant</span> : fab")
      content must contain("<span class=\"bold\">Nombre de signaux</span> : 1 (mesure1)")
      content must contain("<span class=\"bold\">Signal minimum</span> : 2.3 unite1")
      content must contain("<span class=\"bold\">Signal maximum</span> : 3.4 unite1")
      content must contain("<span class=\"bold\">Espèces</span> : esp")
      content must contain("<td>id2</td>")
      content must contain("<td>23/04/2015</td>")
      content must contains("<td>22/04/2015</td>",3)
      content must contain("<h5 class=\"bold\">typeSensor2 / nomTypeSensor2</h5>")
      content must contain("<span class=\"bold\">Fabricant</span> : fab2")
      content must contain("<span class=\"bold\">Nombre de signaux</span> : 2 (mesure2)")
      content must contain("<span class=\"bold\">Signal minimum</span> : 3.4 unite2")
      content must contain("<span class=\"bold\">Signal maximum</span> : 2.3 unite2")
      content must contain("<span class=\"bold\">Espèces</span> : esp2")
      content must contain("<td>id</td>")
      content must contain("<td>Hors service</td>")

      there was one(f.moduleManagerMock).doIfModuleFound(org.mockito.Matchers.eq(bson))(any[Module=>Future[Result]])(any[Unit=>Future[Result]])
      there was one(f.sensorDaoMock).fold(any[JsObject],any[JsObject],org.mockito.Matchers.eq(HashSet[BSONObjectID]()))(any[(HashSet[BSONObjectID],Sensor)=>HashSet[BSONObjectID]])(any[ExecutionContext])
      there was one(f.typeSensorDaoMock).findAll(any[JsObject],any[JsObject])(any[ExecutionContext])
      there was one(f.sensorDaoMock).findAll(any[JsObject],any[JsObject])(any[ExecutionContext])
      there was one(f.typeMesureDaoMock).findAll(any[JsObject],any[JsObject])(any[ExecutionContext])
    }

    "send redirect if module not found" in new WithApplication{
      val f=fixture

      f.applyNotFoundFunction()

      val r=f.controller.listSensors(bson.stringify).apply(FakeRequest(GET, "/inventary/modules/"+bson.stringify+"/configuration/sensors").withSession("user" -> """{"login":"test"}"""))
      status(r) must equalTo(SEE_OTHER)
      header("Location",r) must beSome("/inventary/modules")

      there was one(f.moduleManagerMock).doIfModuleFound(org.mockito.Matchers.eq(bson))(any[Module=>Future[Result]])(any[Unit=>Future[Result]])
    }
  }

  "When user is on resource /inventary/modules/:id/configuration/sensors/:id2, ConfigurationManager" should{
    "send 200 Ok page with a form" in new WithApplication{
      val f=fixture
      val sensor=mock[Sensor]

      f.applyFoundFunction()
      f.typeMesureDaoMock.findListMesure(org.mockito.Matchers.eq("mesure")) returns future{Stream()}
      f.typeMesureDaoMock.findListUnite(org.mockito.Matchers.eq("unite")) returns future{Stream()}
      f.sensorDaoMock.findOne(org.mockito.Matchers.eq(Json.obj("delete"->false,"_id"->bson2)))(any[ExecutionContext]) returns future{Some(sensor)}

      val r=f.controller.formInfoMesure(bson.stringify,bson2.stringify).apply(FakeRequest(GET, "/inventary/modules/"+bson.stringify+"/configuration/sensors/"+bson2.stringify).withSession("user" -> """{"login":"test"}"""))

      status(r) must equalTo(OK)
      val content=contentAsString(r)
      content must contain("<h4>type / id</h4>")
      content must contain("<span class=\"bold\">Commentaires</span> : un com")
      content must matchRegex("<span class=\"bold\">Date d&#x27;assemblage</span> : \\d{2}/\\d{2}/\\d{4}")
      content must contain("<input id=\"index\" name=\"index\" class=\"form-control\" type=\"number\" autofocus=\"autofocus\" autocomplete=\"off\" value=\"\"/>")
      content must contain("<input type=\"text\" id=\"id\" name=\"id\" value=\"\" autofocus=\"autofocus\" autocomplete=\"off\" class=\"form-control\"/>")
      content must contain("<input id=\"mesure\" name=\"mesure\" class=\"form-control\" list=\"list_mesure\" type=\"text\" autocomplete=\"off\" value=\"\"/>")
      content must contain("<input id=\"unite\" name=\"unite\" class=\"form-control\" list=\"list_unite\" type=\"text\" autocomplete=\"off\" value=\"\"/>")

      there was one(f.moduleManagerMock).doIfModuleFound(org.mockito.Matchers.eq(bson))(any[Module=>Future[Result]])(any[Unit=>Future[Result]])
      there was one(f.typeMesureDaoMock).findListMesure(org.mockito.Matchers.eq("mesure"))
      there was one(f.typeMesureDaoMock).findListUnite(org.mockito.Matchers.eq("unite"))
      there was one(f.sensorDaoMock).findOne(org.mockito.Matchers.eq(Json.obj("delete"->false,"_id"->bson2)))(any[ExecutionContext])
    }

    "send bad request if form is submit with empty field" in new WithApplication {
      val f= fixture
      val sensor=mock[Sensor]

      f.applyFoundFunction()
      f.typeMesureDaoMock.findListMesure(org.mockito.Matchers.eq("mesure")) returns future{Stream()}
      f.typeMesureDaoMock.findListUnite(org.mockito.Matchers.eq("unite")) returns future{Stream()}
      f.sensorDaoMock.findOne(org.mockito.Matchers.eq(Json.obj("delete"->false,"_id"->bson2)))(any[ExecutionContext]) returns future{Some(sensor)}

      val r=f.controller.insertInfoMesure(bson.stringify,bson2.stringify).apply(FakeRequest(POST, "/inventary/modules/"+bson.stringify+"/configuration/sensors/"+bson2.stringify).withSession("user" -> """{"login":"test"}"""))

      status(r) must equalTo(BAD_REQUEST)
      val content=contentAsString(r)
      content must contain("<h4>type / id</h4>")
      content must contain("<span class=\"bold\">Commentaires</span> : un com")
      content must matchRegex("<span class=\"bold\">Date d&#x27;assemblage</span> : \\d{2}/\\d{2}/\\d{4}")
      content must contains("<span class=\"control-label errors\">This field is required</span>",4)

      there was one(f.moduleManagerMock).doIfModuleFound(org.mockito.Matchers.eq(bson))(any[Module=>Future[Result]])(any[Unit=>Future[Result]])
      there was one(f.typeMesureDaoMock).findListMesure(org.mockito.Matchers.eq("mesure"))
      there was one(f.typeMesureDaoMock).findListUnite(org.mockito.Matchers.eq("unite"))
      there was one(f.sensorDaoMock).findOne(org.mockito.Matchers.eq(Json.obj("delete"->false,"_id"->bson2)))(any[ExecutionContext])
    }

    "send bad request if the index is not valid" in new WithApplication{
      val f= fixture
      val data="""{"index":"a","id":"id","mesure":"mesure","unite":"unite"}"""
      val sensor=mock[Sensor]

      f.applyFoundFunction()
      f.typeMesureDaoMock.findListMesure(org.mockito.Matchers.eq("mesure")) returns future{Stream()}
      f.typeMesureDaoMock.findListUnite(org.mockito.Matchers.eq("unite")) returns future{Stream()}
      f.sensorDaoMock.findOne(org.mockito.Matchers.eq(Json.obj("delete"->false,"_id"->bson2)))(any[ExecutionContext]) returns future{Some(sensor)}

      val r=f.controller.insertInfoMesure(bson.stringify,bson2.stringify).apply(FakeRequest(POST, "/inventary/modules/"+bson.stringify+"/configuration/sensors/"+bson2.stringify).withJsonBody(Json.parse(data)).withSession("user" -> """{"login":"test"}"""))

      status(r) must equalTo(BAD_REQUEST)
      val content=contentAsString(r)
      content must contain("<span class=\"control-label errors\">Numeric value expected</span>")

      there was one(f.moduleManagerMock).doIfModuleFound(org.mockito.Matchers.eq(bson))(any[Module=>Future[Result]])(any[Unit=>Future[Result]])
      there was one(f.typeMesureDaoMock).findListMesure(org.mockito.Matchers.eq("mesure"))
      there was one(f.typeMesureDaoMock).findListUnite(org.mockito.Matchers.eq("unite"))
      there was one(f.sensorDaoMock).findOne(org.mockito.Matchers.eq(Json.obj("delete"->false,"_id"->bson2)))(any[ExecutionContext])
    }

    "send redirect after insert mesure information into session" in new WithApplication{
      val f= fixture
      val data="""{"index":"0","id":"id","mesure":"mesure","unite":"unite"}"""
      val sensor=mock[Sensor]

      f.applyFoundFunction()
      f.sensorDaoMock.findOne(org.mockito.Matchers.eq(Json.obj("delete"->false,"_id"->bson2)))(any[ExecutionContext]) returns future{Some(sensor)}

      val r=f.controller.insertInfoMesure(bson.stringify,bson2.stringify).apply(FakeRequest(POST, "/inventary/modules/"+bson.stringify+"/configuration/sensors/"+bson2.stringify).withJsonBody(Json.parse(data)).withSession("user" -> """{"login":"test"}"""))

      status(r) must equalTo(SEE_OTHER)
      header("Location",r) must beSome("/inventary/modules/"+bson.stringify+"/configuration/sensors")
      val s=session(r)
      s.get("infoMesure") must beSome("[{\"sensor\":\""+bson2.stringify+"\",\"info\":{\"index\":0,\"id\":\"id\",\"mesure\":\"mesure\",\"unite\":\"unite\"}}]")

      there was one(f.moduleManagerMock).doIfModuleFound(org.mockito.Matchers.eq(bson))(any[Module=>Future[Result]])(any[Unit=>Future[Result]])
      there was one(f.sensorDaoMock).findOne(org.mockito.Matchers.eq(Json.obj("delete"->false,"_id"->bson2)))(any[ExecutionContext])
    }
  }

  "When method createJsonInfoMesure is called, ConfigurationManager" should{
    "send new JsArray with mesure information" in new WithApplication{
      val f=fixture
      val infoMesure=InfoMesureForm(0,"id","mesure","unite")
      val infoMesureObj=Json.parse("""{"index":0,"id":"id","mesure":"mesure","unite":"unite"}""")

      val r=f.controller.createJsonInfoMesure(bson.stringify,infoMesure)(FakeRequest(POST, "/inventary/modules/"+bson.stringify+"/configuration/sensors/"+bson2.stringify))

      r must equalTo(JsArray(List(Json.obj("sensor"->bson.stringify,"info"->infoMesureObj))))
    }

    "send JsArray with the new mesure information" in new WithApplication{
      val f=fixture
      val infoMesure=InfoMesureForm(0,"id","mesure","unite")
      val infoMesureObj=Json.parse("""{"index":0,"id":"id","mesure":"mesure","unite":"unite"}""")
      val objSession="""{"sensor":""""+bson2.stringify+"""","info":{"index":1,"id":"id1","mesure":"mesure1","unite":"unite1"}}"""

      val r=f.controller.createJsonInfoMesure(bson.stringify,infoMesure)(FakeRequest(POST, "/inventary/modules/"+bson.stringify+"/configuration/sensors/"+bson2.stringify).withSession("infoMesure"->("["+objSession+"]")))

      r must equalTo(JsArray(List(Json.parse(objSession),Json.obj("sensor"->bson.stringify,"info"->infoMesureObj))))
    }
  }

  "When method verifyModuleAndSensorFound is called, ConfigurationManager" should{
    "send redirect if module not found" in new WithApplication{
      val f=fixture
      val func=mock[Module=>Future[Result]]

      f.applyNotFoundFunction()

      val r=f.controller.verifyModuleAndSensorFound(bson.stringify,bson2.stringify)(func)(FakeRequest(GET, "/inventary/modules/"+bson.stringify+"/configuration/sensors/"+bson2.stringify).withSession("user" -> """{"login":"test"}"""))
      status(r) must equalTo(SEE_OTHER)
      header("Location",r) must beSome("/inventary/modules")

      there was one(f.moduleManagerMock).doIfModuleFound(org.mockito.Matchers.eq(bson))(any[Module=>Future[Result]])(any[Unit=>Future[Result]])
    }

    "send redirect if sensor not found" in new WithApplication{
      val f=fixture
      val func=mock[Module=>Future[Result]]

      f.applyFoundFunction()
      f.sensorDaoMock.findOne(org.mockito.Matchers.eq(Json.obj("delete"->false,"_id"->bson2)))(any[ExecutionContext]) returns future{None}

      val r=f.controller.verifyModuleAndSensorFound(bson.stringify,bson2.stringify)(func)(FakeRequest(GET, "/inventary/modules/"+bson.stringify+"/configuration/sensors/"+bson2.stringify).withSession("user" -> """{"login":"test"}"""))
      status(r) must equalTo(SEE_OTHER)
      header("Location",r) must beSome("/inventary/modules/"+bson.stringify+"/configuration/sensors")

      there was one(f.moduleManagerMock).doIfModuleFound(org.mockito.Matchers.eq(bson))(any[Module=>Future[Result]])(any[Unit=>Future[Result]])
      there was one(f.sensorDaoMock).findOne(org.mockito.Matchers.eq(Json.obj("delete"->false,"_id"->bson2)))(any[ExecutionContext])
    }

    "execute function if module and sensor found" in new WithApplication{
      val f=fixture
      val func=mock[Module=>Future[Result]]

      f.applyFoundFunction()
      f.sensorDaoMock.findOne(org.mockito.Matchers.eq(Json.obj("delete"->false,"_id"->bson2)))(any[ExecutionContext]) returns future{Some(mock[Sensor])}
      func.apply(any[Module]) returns future{Results.Ok("exec func")}

      val r=f.controller.verifyModuleAndSensorFound(bson.stringify,bson2.stringify)(func)(FakeRequest(GET, "/inventary/modules/"+bson.stringify+"/configuration/sensors/"+bson2.stringify).withSession("user" -> """{"login":"test"}"""))
      status(r) must equalTo(OK)
      contentAsString(r) must equalTo("exec func")

      there was one(f.moduleManagerMock).doIfModuleFound(org.mockito.Matchers.eq(bson))(any[Module=>Future[Result]])(any[Unit=>Future[Result]])
      there was one(f.sensorDaoMock).findOne(org.mockito.Matchers.eq(Json.obj("delete"->false,"_id"->bson2)))(any[ExecutionContext])
      there was one(func).apply(any[Module])
    }
  }
}
