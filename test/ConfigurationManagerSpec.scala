import java.io.{ByteArrayOutputStream, OutputStream}
import java.util.Date
import java.util.zip.{ZipEntry, ZipOutputStream}

import controllers.{InfoMesureForm, ModuleManagerLike, ConfigurationForm, ConfigurationManagerLike}
import models._
import org.junit.runner.RunWith
import org.mockito.ArgumentCaptor
import org.specs2.matcher.{MatchResult, Expectable, Matcher}
import org.specs2.mock.Mockito
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner
import play.api.libs.json.{Writes, JsArray, JsObject, Json}
import play.api.mvc.{Results, Result}
import play.api.test.Helpers._
import play.api.test.{FakeRequest, WithApplication}
import play.modules.reactivemongo.json.BSONFormats.BSONObjectIDFormat
import reactivemongo.bson.BSONObjectID
import reactivemongo.core.commands.{LastError, GetLastError}

import scala.collection.immutable.HashSet
import scala.concurrent.duration.Duration
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
    val informationMesureDaoMock=mock[InformationMesureDao]
    val configurationDaoMock=mock[ConfigurationDao]
    val moduleDaoMock=mock[ModuleDao]
    val moduleManagerMock = mock[ModuleManagerLike]
    val zipOutputStreamBuilderMock=mock[ZipOutputStreamBuilder]
    val controller = new ConfigurationManagerTest{
      override val sensorsDao=sensorDaoMock
      override val typeSensorsDao=typeSensorDaoMock
      override val typeMesureDao=typeMesureDaoMock
      override val moduleManager=moduleManagerMock
      override val informationMesureDao=informationMesureDaoMock
      override val configurationDao=configurationDaoMock
      override val moduleDao=moduleDaoMock
      override val zipOutputStreamBuilder=zipOutputStreamBuilderMock
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

    "redirect to login for resource /inventary/modules/:id/configuration/sensors/:id2" in new WithApplication {
      route(FakeRequest(POST, "/inventary/modules/"+bson.stringify+"/configuration/sensors/"+bson2.stringify)).map(
        r => {
          status(r) must equalTo(SEE_OTHER)
          header("Location", r) must equalTo(Some("/login"))
        }
      ).getOrElse(
          failure("Pas de retour de la fonction")
        )
    }

    "redirect to login for resource /inventary/modules/:id/configuration/validation" in new WithApplication {
      route(FakeRequest(GET, "/inventary/modules/"+bson.stringify+"/configuration/validation")).map(
        r => {
          status(r) must equalTo(SEE_OTHER)
          header("Location", r) must equalTo(Some("/login"))
        }
      ).getOrElse(
          failure("Pas de retour de la fonction")
        )
    }

    "redirect to login for resource /inventary/modules/:id/configuration/validation" in new WithApplication {
      route(FakeRequest(POST, "/inventary/modules/"+bson.stringify+"/configuration/validation")).map(
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
      val s=session(r)
      s.get("configForm") must beSome("insert")

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

  "When user is on resource /inventary/modules/:id/configuration/:id2/update, ConfigurationManager" should {
    "send 200 Ok page with a prefilled form" in new WithApplication {
      val f = fixture
      val infos=List(bson3,bson4)
      val controller=mock[ConfigurationManagerTest]

      controller.configurationDao returns f.configurationDaoMock
      controller.moduleManager returns f.moduleManagerMock
      org.mockito.Mockito.doCallRealMethod().when(controller).formUpdate(bson.stringify,bson2.stringify)
      controller.form returns f.controller.form
      controller.formatsForm returns f.controller.formatsForm
      controller.findInformationForForm(infos) returns future{
        List(
          Json.obj("sensor"->bson4.stringify,"info"->Json.obj("index"->1,"id"->"id2","mesure"->"Tension2","unite"->"Volt2")),
          Json.obj("sensor"->bson3.stringify,"info"->Json.obj("index"->0,"id"->"id","mesure"->"Tension","unite"->"Volt"))
        )
      }
      f.applyFoundFunction()
      f.configurationDaoMock.findOne(org.mockito.Matchers.eq(Json.obj("_id"->bson2)))(any[ExecutionContext]) returns future{Some(Configuration(_id=bson2,port="/dev/ttyUSB0",types="ADC",infoMesure=infos))}

      val r = controller.formUpdate(bson.stringify,bson2.stringify).apply(FakeRequest(GET, "/inventary/modules/" + bson.stringify + "/configuration/"+bson2.stringify+"/update").withSession("user" -> """{"login":"test"}"""))
      status(r) must equalTo(OK)
      val content = contentAsString(r)
      content must contain("<h4>type / id</h4>")
      content must contain("<span class=\"bold\">Commentaires</span> : un com")
      content must matchRegex("<span class=\"bold\">Date d&#x27;assemblage</span> : \\d{2}/\\d{2}/\\d{4}")
      content must contain("<input type=\"text\" id=\"port\" name=\"port\" value=\"/dev/ttyUSB0\" autofocus=\"autofocus\" autocomplete=\"off\" class=\"form-control\"/>")
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
      content must contain("<input type=\"text\" id=\"types\" name=\"types\" value=\"ADC\" class=\"form-control\"/>")
      val s = session(r)
      s.get("configForm") must beSome("update")
      s.get("config") must beSome("""{"port":"/dev/ttyUSB0","timeout":10000,"baud":9600,"bits":8,"stopBits":1,"parity":0,"timeFilter":1000,"types":"ADC"}""")
      s.get("infoMesure") must beSome("[{\"sensor\":\""+bson4.stringify+"\",\"info\":{\"index\":1,\"id\":\"id2\",\"mesure\":\"Tension2\",\"unite\":\"Volt2\"}},{\"sensor\":\""+bson3.stringify+"\",\"info\":{\"index\":0,\"id\":\"id\",\"mesure\":\"Tension\",\"unite\":\"Volt\"}}]")
      s.get("configUpdate") must beSome(bson2.stringify)

      there was one(f.moduleManagerMock).doIfModuleFound(org.mockito.Matchers.eq(bson))(any[Module => Future[Result]])(any[Unit => Future[Result]])
      there was one(controller).findInformationForForm(infos)
      there was one(f.configurationDaoMock).findOne(org.mockito.Matchers.eq(Json.obj("_id"->bson2)))(any[ExecutionContext])
    }

    "send redirect if module not found" in new WithApplication {
      val f = fixture

      f.applyNotFoundFunction()

      val r = f.controller.formUpdate(bson.stringify,bson2.stringify).apply(FakeRequest(GET, "/inventary/modules/" + bson.stringify + "/configuration/"+bson2.stringify+"/update").withSession("user" -> """{"login":"test"}"""))
      status(r) must equalTo(SEE_OTHER)
      header("Location", r) must beSome("/inventary/modules")

      there was one(f.moduleManagerMock).doIfModuleFound(org.mockito.Matchers.eq(bson))(any[Module => Future[Result]])(any[Unit => Future[Result]])
    }

    "send redirect if configuration not found" in new WithApplication{
      val f = fixture

      f.applyFoundFunction()
      f.configurationDaoMock.findOne(org.mockito.Matchers.eq(Json.obj("_id"->bson2)))(any[ExecutionContext]) returns future{None}

      val r = f.controller.formUpdate(bson.stringify,bson2.stringify).apply(FakeRequest(GET, "/inventary/modules/" + bson.stringify + "/configuration/"+bson2.stringify+"/update").withSession("user" -> """{"login":"test"}"""))
      status(r) must equalTo(SEE_OTHER)

      there was one(f.moduleManagerMock).doIfModuleFound(org.mockito.Matchers.eq(bson))(any[Module => Future[Result]])(any[Unit => Future[Result]])
      there was one(f.configurationDaoMock).findOne(org.mockito.Matchers.eq(Json.obj("_id"->bson2)))(any[ExecutionContext])
    }
  }

  "When user is on resource /inventary/modules/:id/configuration/update, ConfigurationManager" should {
    "send 200 Ok page with a prefilled form" in new WithApplication {
      val f = fixture
      val data="""{"port":"/dev/ttyUSB0","timeout":10000,"baud":9600,"bits":8,"stopBits":1,"parity":0,"timeFilter":1000,"types":"ADC"}"""

      f.applyFoundFunction()

      val r = f.controller.formUpdateCurrent(bson.stringify).apply(FakeRequest(GET, "/inventary/modules/" + bson.stringify + "/configuration/update").withSession("user" -> """{"login":"test"}""", "config"->data))
      status(r) must equalTo(OK)
      val content = contentAsString(r)
      content must contain("<h4>type / id</h4>")
      content must contain("<span class=\"bold\">Commentaires</span> : un com")
      content must matchRegex("<span class=\"bold\">Date d&#x27;assemblage</span> : \\d{2}/\\d{2}/\\d{4}")
      content must contain("<input type=\"text\" id=\"port\" name=\"port\" value=\"/dev/ttyUSB0\" autofocus=\"autofocus\" autocomplete=\"off\" class=\"form-control\"/>")
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
      content must contain("<input type=\"text\" id=\"types\" name=\"types\" value=\"ADC\" class=\"form-control\"/>")

      there was one(f.moduleManagerMock).doIfModuleFound(org.mockito.Matchers.eq(bson))(any[Module => Future[Result]])(any[Unit => Future[Result]])
    }

    "send redirect if module not found" in new WithApplication {
      val f = fixture

      f.applyNotFoundFunction()

      val r = f.controller.formUpdateCurrent(bson.stringify).apply(FakeRequest(GET, "/inventary/modules/" + bson.stringify + "/configuration/update").withSession("user" -> """{"login":"test"}"""))
      status(r) must equalTo(SEE_OTHER)
      header("Location", r) must beSome("/inventary/modules")

      there was one(f.moduleManagerMock).doIfModuleFound(org.mockito.Matchers.eq(bson))(any[Module => Future[Result]])(any[Unit => Future[Result]])
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

  "When user is on resource /inventary/modules/:id/configuration/validation, ConfigurationManager" should{
    "send redirect if module not found" in new WithApplication{
      val f=fixture

      f.applyNotFoundFunction()

      val r=f.controller.formValidation(bson.stringify).apply(FakeRequest(GET,"/inventary/modules/"+bson.stringify+"/configuration/validation").withSession("user" -> """{"login":"test"}"""))

      status(r) must equalTo(SEE_OTHER)
      header("Location",r) must beSome("/inventary/modules")

      there was one(f.moduleManagerMock).doIfModuleFound(org.mockito.Matchers.eq(bson))(any[Module=>Future[Result]])(any[Unit=>Future[Result]])
    }

    "send 200 Ok page with the summary of the validation" in new WithApplication{
      val f=fixture
      val sessionConfig="""{"port":"/dev/ttyUSB0","timeout":10,"baud":9600,"bits":8,"parity":0,"stopBits":1,"timeFilter":1000,"types":"ADC"}"""
      val sessionInfoMesure="""[{"sensor":""""+bson2.stringify+"""","info":{"index":0,"id":"id","mesure":"mesure","unite":"unite"}}]"""
      val typeSensor=List(TypeSensor(bson3,"typeSensor2","nomTypeSensor2",bson2,"fab2",2,List("esp2"),3.4f,2.3f))
      val typeMesure=List(TypeMesure(bson2,"mesure2","unite2"))
      val sensor=List(Sensor(bson2,"id",bson3,None,date,None,true,None))

      f.applyFoundFunction()
      f.sensorDaoMock.fold(any[JsObject],any[JsObject],org.mockito.Matchers.eq(HashSet[BSONObjectID]()))(any[(HashSet[BSONObjectID],Sensor)=>HashSet[BSONObjectID]])(any[ExecutionContext]) returns future{HashSet[BSONObjectID]()}
      f.typeSensorDaoMock.findAll(any[JsObject],any[JsObject])(any[ExecutionContext]) returns future{typeSensor}
      f.sensorDaoMock.findAll(any[JsObject],any[JsObject])(any[ExecutionContext]) returns future{sensor}
      f.typeMesureDaoMock.findAll(any[JsObject],any[JsObject])(any[ExecutionContext]) returns future{typeMesure}

      val req=FakeRequest(GET,"/inventary/modules/"+bson.stringify+"/configuration/validation").withSession(("user"  -> """{"login":"test"}"""), ("config" -> sessionConfig) , ("infoMesure" -> sessionInfoMesure))
      val r=f.controller.formValidation(bson.stringify).apply(req)

      status(r) must equalTo(OK)
      val content=contentAsString(r)
      content must contain("<h4>type / id</h4>")
      content must contain("<span class=\"bold\">Commentaires</span> : un com")
      content must matchRegex("<span class=\"bold\">Date d&#x27;assemblage</span> : \\d{2}/\\d{2}/\\d{4}")
      content must contain("<span class=\"bold\">Port</span> : /dev/ttyUSB0")
      content must contain("<span class=\"bold\">Délai d&#x27;attente de la connection</span> : 10")
      content must contain("<span class=\"bold\">Baud</span> : 9600")
      content must contain("<span class=\"bold\">Bits de donnée</span> : 8")
      content must contain("<span class=\"bold\">Parité</span> : 0")
      content must contain("<span class=\"bold\">Stop de bits</span> : 1")
      content must contain("<span class=\"bold\">Temps de filtrage des données</span> : 1000")
      content must contain("<span class=\"bold\">Type de configuration</span> : ADC")
      content must contain("<td>0</td>")
      content must contain("<td>id</td>")
      content must contain("<td>mesure</td>")
      content must contain("<td>unite</td>")
      content must contain("<span class=\"bold\">typeSensor2 / nomTypeSensor2</span><br/>id")

      there was one(f.moduleManagerMock).doIfModuleFound(org.mockito.Matchers.eq(bson))(any[Module=>Future[Result]])(any[Unit=>Future[Result]])
      there was one(f.sensorDaoMock).fold(any[JsObject],any[JsObject],org.mockito.Matchers.eq(HashSet[BSONObjectID]()))(any[(HashSet[BSONObjectID],Sensor)=>HashSet[BSONObjectID]])(any[ExecutionContext])
      there was one(f.typeSensorDaoMock).findAll(any[JsObject],any[JsObject])(any[ExecutionContext])
      there was one(f.sensorDaoMock).findAll(any[JsObject],any[JsObject])(any[ExecutionContext])
      there was one(f.typeMesureDaoMock).findAll(any[JsObject],any[JsObject])(any[ExecutionContext])
    }

    "send bad request with errors" in new WithApplication{
      val f=fixture
      val sessionConfig="""{"port":"","timeout":10,"baud":9600,"bits":8,"parity":0,"stopBits":1,"timeFilter":1000,"types":""}"""

      f.applyFoundFunction()
      f.sensorDaoMock.fold(any[JsObject],any[JsObject],org.mockito.Matchers.eq(HashSet[BSONObjectID]()))(any[(HashSet[BSONObjectID],Sensor)=>HashSet[BSONObjectID]])(any[ExecutionContext]) returns future{HashSet[BSONObjectID]()}
      f.typeSensorDaoMock.findAll(any[JsObject],any[JsObject])(any[ExecutionContext]) returns future{List()}
      f.sensorDaoMock.findAll(any[JsObject],any[JsObject])(any[ExecutionContext]) returns future{List()}
      f.typeMesureDaoMock.findAll(any[JsObject],any[JsObject])(any[ExecutionContext]) returns future{List()}

      val req=FakeRequest(GET,"/inventary/modules/"+bson.stringify+"/configuration/validation").withSession(("user"  -> """{"login":"test"}"""), ("config" -> sessionConfig) , ("infoMesure" -> "[]"))
      val r=f.controller.formValidation(bson.stringify).apply(req)

      status(r) must equalTo(BAD_REQUEST)
      val content=contentAsString(r)
      content must contain("<div class=\"alert alert-danger\" role=\"alert\">Le port ou le type configuration ne sont pas définis</div>")
      content must contain("<div class=\"alert alert-danger\" role=\"alert\">La configuration doit contenir au moins une information de mesure</div>")

      there was one(f.moduleManagerMock).doIfModuleFound(org.mockito.Matchers.eq(bson))(any[Module=>Future[Result]])(any[Unit=>Future[Result]])
      there was one(f.sensorDaoMock).fold(any[JsObject],any[JsObject],org.mockito.Matchers.eq(HashSet[BSONObjectID]()))(any[(HashSet[BSONObjectID],Sensor)=>HashSet[BSONObjectID]])(any[ExecutionContext])
      there was one(f.typeSensorDaoMock).findAll(any[JsObject],any[JsObject])(any[ExecutionContext])
      there was one(f.sensorDaoMock).findAll(any[JsObject],any[JsObject])(any[ExecutionContext])
      there was one(f.typeMesureDaoMock).findAll(any[JsObject],any[JsObject])(any[ExecutionContext])
    }

    "send bad request if validate configuration with error" in new WithApplication{
      val f=fixture
      val sessionConfig="""{"port":"","timeout":10,"baud":9600,"bits":8,"parity":0,"stopBits":1,"timeFilter":1000,"types":""}"""

      f.applyFoundFunction()
      f.sensorDaoMock.fold(any[JsObject],any[JsObject],org.mockito.Matchers.eq(HashSet[BSONObjectID]()))(any[(HashSet[BSONObjectID],Sensor)=>HashSet[BSONObjectID]])(any[ExecutionContext]) returns future{HashSet[BSONObjectID]()}
      f.typeSensorDaoMock.findAll(any[JsObject],any[JsObject])(any[ExecutionContext]) returns future{List()}
      f.sensorDaoMock.findAll(any[JsObject],any[JsObject])(any[ExecutionContext]) returns future{List()}
      f.typeMesureDaoMock.findAll(any[JsObject],any[JsObject])(any[ExecutionContext]) returns future{List()}

      val req=FakeRequest(POST,"/inventary/modules/"+bson.stringify+"/configuration/validation").withSession(("user"  -> """{"login":"test"}"""), ("config" -> sessionConfig) , ("infoMesure" -> "[]"))
      val r=f.controller.validation(bson.stringify).apply(req)

      status(r) must equalTo(BAD_REQUEST)
      val content=contentAsString(r)
      content must contain("<div class=\"alert alert-danger\" role=\"alert\">Le port ou le type configuration ne sont pas définis</div>")
      content must contain("<div class=\"alert alert-danger\" role=\"alert\">La configuration doit contenir au moins une information de mesure</div>")

      there was one(f.moduleManagerMock).doIfModuleFound(org.mockito.Matchers.eq(bson))(any[Module=>Future[Result]])(any[Unit=>Future[Result]])
      there was one(f.sensorDaoMock).fold(any[JsObject],any[JsObject],org.mockito.Matchers.eq(HashSet[BSONObjectID]()))(any[(HashSet[BSONObjectID],Sensor)=>HashSet[BSONObjectID]])(any[ExecutionContext])
      there was one(f.typeSensorDaoMock).findAll(any[JsObject],any[JsObject])(any[ExecutionContext])
      there was one(f.sensorDaoMock).findAll(any[JsObject],any[JsObject])(any[ExecutionContext])
      there was one(f.typeMesureDaoMock).findAll(any[JsObject],any[JsObject])(any[ExecutionContext])
    }

    "send redirect after insert configuration" in new WithApplication{
      val f=fixture
      val sessionConfig="""{"port":"/dev/ttyUSB0","timeout":10000,"baud":9600,"bits":8,"parity":0,"stopBits":1,"timeFilter":1000,"types":"ADC"}"""
      val sessionInfoMesure="""[{"sensor":""""+bson2.stringify+"""","info":{"index":0,"id":"id","mesure":"mesure","unite":"unite"}}]"""
      val lastError=mock[LastError]

      f.applyFoundFunction()
      f.typeMesureDaoMock.findOne(org.mockito.Matchers.eq(Json.obj("nom"->"mesure","unite"->"unite")))(any[ExecutionContext]) returns future{Some(TypeMesure(bson2,"mesure","unite"))}
      f.informationMesureDaoMock.findOne(any[JsObject])(any[ExecutionContext]) returns future{None}
      f.informationMesureDaoMock.insert(any[InformationMesure],any[GetLastError])(any[ExecutionContext]) returns future{lastError}
      f.configurationDaoMock.insert(any[Configuration],any[GetLastError])(any[ExecutionContext]) returns future{lastError}
      f.moduleDaoMock.updateById(org.mockito.Matchers.eq(bson),any[JsObject],any[GetLastError])(any[Writes[JsObject]],any[ExecutionContext]) returns future{lastError}

      val req=FakeRequest(POST,"/inventary/modules/"+bson.stringify+"/configuration/validation").withSession(("user"  -> """{"login":"test"}"""), ("config" -> sessionConfig) , ("infoMesure" -> sessionInfoMesure))
      val r=f.controller.validation(bson.stringify).apply(req)

      status(r) must equalTo(SEE_OTHER)
      header("Location",r) must beSome("/inventary/modules/"+bson.stringify)
      val s=session(r)
      s.get("config") must beNone
      s.get("configForm") must beNone
      s.get("infoMesure") must beNone

      val captor = ArgumentCaptor.forClass(classOf[InformationMesure])
      val captorConfig=ArgumentCaptor.forClass(classOf[Configuration])
      val captorObj=ArgumentCaptor.forClass(classOf[JsObject])

      there was one(f.moduleManagerMock).doIfModuleFound(org.mockito.Matchers.eq(bson))(any[Module=>Future[Result]])(any[Unit=>Future[Result]])
      there was one(f.typeMesureDaoMock).findOne(org.mockito.Matchers.eq(Json.obj("nom"->"mesure","unite"->"unite")))(any[ExecutionContext])
      there was one(f.informationMesureDaoMock).findOne(any[JsObject])(any[ExecutionContext])
      there was one(f.informationMesureDaoMock).insert(captor.capture(),any[GetLastError])(any[ExecutionContext])
      there was one(f.configurationDaoMock).insert(captorConfig.capture(),any[GetLastError])(any[ExecutionContext])
      there was one(f.moduleDaoMock).updateById(org.mockito.Matchers.eq(bson),captorObj.capture(),any[GetLastError])(any[Writes[JsObject]],any[ExecutionContext])

      val arg=captor.getValue
      arg must equalTo(InformationMesure(arg._id,0,"id",bson2,bson2))

      val configCapt=captorConfig.getValue
      configCapt must equalTo(Configuration(_id=configCapt._id,port="/dev/ttyUSB0",types="ADC",infoMesure=List(arg._id)))
      captorObj.getValue must equalTo(Json.obj("$push"->Json.obj("configuration"->configCapt._id)))
    }

    "send redirect if module not found" in new WithApplication{
      val f=fixture

      f.applyNotFoundFunction()

      val r=f.controller.validation(bson.stringify).apply(FakeRequest(POST,"/inventary/modules/"+bson.stringify+"/configuration/validation").withSession("user" -> """{"login":"test"}"""))

      status(r) must equalTo(SEE_OTHER)
      header("Location",r) must beSome("/inventary/modules")

      there was one(f.moduleManagerMock).doIfModuleFound(org.mockito.Matchers.eq(bson))(any[Module=>Future[Result]])(any[Unit=>Future[Result]])
    }
  }

  "When user is on resource /inventary/modules/:id/configuration/download, ConfigurationManager" should{
    "return 404 not found if module not found" in new WithApplication{
      val f=fixture

      f.applyNotFoundFunction()

      val r=f.controller.downloadConfiguration(bson.stringify).apply(FakeRequest(GET,"/inventary/modules/"+bson.stringify+"/configuration/download").withSession("user" -> """{"login":"test"}"""))

      status(r) must equalTo(NOT_FOUND)

      there was one(f.moduleManagerMock).doIfModuleFound(org.mockito.Matchers.eq(bson))(any[Module=>Future[Result]])(any[Unit=>Future[Result]])
    }

    "return a result with a configurations zip" in new WithApplication{
      val f=fixture
      val zip=mock[ZipOutputStream]
      val config=List(Configuration(port="/dev/ttyUSB0",types="ADC",infoMesure=List(bson3)),Configuration(port="/dev/ttyUSB0",types="wasp",infoMesure=List(bson4)))
      val info=List(InformationMesure(bson3,0,"id",bson2,bson3),InformationMesure(bson4,0,"id2",bson3,bson2))

      f.applyFoundFunction()
      f.configurationDaoMock.findAll(any[JsObject],any[JsObject])(any[ExecutionContext]) returns future{config}
      f.informationMesureDaoMock.findAll(any[JsObject],any[JsObject])(any[ExecutionContext]) returns future{info}
      f.zipOutputStreamBuilderMock.createZipOutputStream(any[OutputStream]) returns zip
      org.mockito.Mockito.doNothing().when(zip).putNextEntry(org.mockito.Matchers.eq(new ZipEntry("ADC.properties")))
      org.mockito.Mockito.doNothing().when(zip).putNextEntry(org.mockito.Matchers.eq(new ZipEntry("wasp.properties")))
      org.mockito.Mockito.doNothing().when(zip).write(org.mockito.Matchers.eq(("device=/dev/ttyUSB0\n").getBytes))
      org.mockito.Mockito.doNothing().when(zip).write(org.mockito.Matchers.eq("timeout=10000\n".getBytes))
      org.mockito.Mockito.doNothing().when(zip).write(org.mockito.Matchers.eq(("baud=9600\n").getBytes))
      org.mockito.Mockito.doNothing().when(zip).write(org.mockito.Matchers.eq(("bits=8\n").getBytes))
      org.mockito.Mockito.doNothing().when(zip).write(org.mockito.Matchers.eq(("stopBits=1\n").getBytes))
      org.mockito.Mockito.doNothing().when(zip).write(org.mockito.Matchers.eq(("parity=0\n").getBytes))
      org.mockito.Mockito.doNothing().when(zip).write(org.mockito.Matchers.eq(("timeFilter=1000\n").getBytes))
      org.mockito.Mockito.doNothing().when(zip).write(org.mockito.Matchers.eq(("type=ADC\n").getBytes))
      org.mockito.Mockito.doNothing().when(zip).write(org.mockito.Matchers.eq(("type=wasp\n").getBytes))
      org.mockito.Mockito.doNothing().when(zip).write(org.mockito.Matchers.eq(("moduleId="+bson.stringify+"\n").getBytes))
      org.mockito.Mockito.doNothing().when(zip).write(org.mockito.Matchers.eq(("sensors={\\\n").getBytes))
      org.mockito.Mockito.doNothing().when(zip).write(org.mockito.Matchers.eq(("\""+bson3.stringify+"\":[\\\n").getBytes))
      org.mockito.Mockito.doNothing().when(zip).write(org.mockito.Matchers.eq(("\""+bson2.stringify+"\":[\\\n").getBytes))
      org.mockito.Mockito.doNothing().when(zip).write(org.mockito.Matchers.eq("{\"index\":0,".getBytes))
      org.mockito.Mockito.doNothing().when(zip).write(org.mockito.Matchers.eq(("\"infoId\":\""+bson3.stringify+"\"}").getBytes))
      org.mockito.Mockito.doNothing().when(zip).write(org.mockito.Matchers.eq(("\"infoId\":\""+bson4.stringify+"\"}").getBytes))
      org.mockito.Mockito.doNothing().when(zip).write(org.mockito.Matchers.eq("\\\n]".getBytes))
      org.mockito.Mockito.doNothing().when(zip).write(org.mockito.Matchers.eq(("}").getBytes))
      org.mockito.Mockito.doNothing().when(zip).closeEntry()
      org.mockito.Mockito.doNothing().when(zip).close()

      val r=f.controller.downloadConfiguration(bson.stringify).apply(FakeRequest(GET,"/inventary/modules/"+bson.stringify+"/configuration/download").withSession("user" -> """{"login":"test"}"""))

      status(r) must equalTo(OK)
      contentAsBytes(r) must equalTo((new ByteArrayOutputStream).toByteArray)
      header("Content-Type",r) must beSome("application/zip")
      header("Content-Disposition",r) must beSome("attachment; filename=configuration.zip")

      there was one(f.configurationDaoMock).findAll(any[JsObject],any[JsObject])(any[ExecutionContext])
      there was one(f.informationMesureDaoMock).findAll(any[JsObject],any[JsObject])(any[ExecutionContext])
      there was one(f.zipOutputStreamBuilderMock).createZipOutputStream(any[OutputStream])
      there was 2.times(zip).putNextEntry(any[ZipEntry])
      there was 2.times(zip).write(org.mockito.Matchers.eq(("device=/dev/ttyUSB0\n").getBytes))
      there was 2.times(zip).write(org.mockito.Matchers.eq("timeout=10000\n".getBytes))
      there was 2.times(zip).write(org.mockito.Matchers.eq(("baud=9600\n").getBytes))
      there was 2.times(zip).write(org.mockito.Matchers.eq(("bits=8\n").getBytes))
      there was 2.times(zip).write(org.mockito.Matchers.eq(("stopBits=1\n").getBytes))
      there was 2.times(zip).write(org.mockito.Matchers.eq(("parity=0\n").getBytes))
      there was 2.times(zip).write(org.mockito.Matchers.eq(("timeFilter=1000\n").getBytes))
      there was one(zip).write(org.mockito.Matchers.eq(("type=ADC\n").getBytes))
      there was one(zip).write(org.mockito.Matchers.eq(("type=wasp\n").getBytes))
      there was 2.times(zip).write(org.mockito.Matchers.eq(("moduleId="+bson.stringify+"\n").getBytes))
      there was 2.times(zip).write(org.mockito.Matchers.eq(("sensors={\\\n").getBytes))
      there was one(zip).write(org.mockito.Matchers.eq(("\""+bson3.stringify+"\":[\\\n").getBytes))
      there was one(zip).write(org.mockito.Matchers.eq(("\""+bson2.stringify+"\":[\\\n").getBytes))
      there was 2.times(zip).write(org.mockito.Matchers.eq("{\"index\":0,".getBytes))
      there was one(zip).write(org.mockito.Matchers.eq(("\"infoId\":\""+bson3.stringify+"\"}").getBytes))
      there was one(zip).write(org.mockito.Matchers.eq(("\"infoId\":\""+bson4.stringify+"\"}").getBytes))
      there was 2.times(zip).write(org.mockito.Matchers.eq("\\\n]".getBytes))
      there was 2.times(zip).write(org.mockito.Matchers.eq(("}").getBytes))
      there was 2.times(zip).closeEntry()
      there was one(zip).close()
      there was one(f.moduleManagerMock).doIfModuleFound(org.mockito.Matchers.eq(bson))(any[Module=>Future[Result]])(any[Unit=>Future[Result]])
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

  "When method getConfiguration is called, ConfigurationManager" should{
    "return the default configuration if configuration is not found in the session" in new WithApplication{
      val f=fixture

      val r=f.controller.getConfiguration(FakeRequest(GET,"url"))

      r must equalTo(ConfigurationForm(port="",types=""))
    }

    "return the default configuration if configuration is not valid" in new WithApplication{
      val f=fixture

      val r=f.controller.getConfiguration(FakeRequest(GET,"url").withSession("config"->"{}"))

      r must equalTo(ConfigurationForm(port="",types=""))
    }

    "return the configuration in session" in new WithApplication{
      val f=fixture
      val session="""{"port":"/dev/ttyUSB0","timeout":10,"baud":9600,"bits":8,"parity":0,"stopBits":1,"timeFilter":1000,"types":"ADC"}"""

      val r=f.controller.getConfiguration(FakeRequest(GET,"url").withSession("config"->session))

      r must equalTo(ConfigurationForm("/dev/ttyUSB0",10,9600,8,1,0,1000,"ADC"))
    }
  }

  "When method getInfoMesure is called, ConfigurationManager" should{
    "return an empty list if informations mesure are not in session" in new WithApplication{
      val f=fixture

      val r=f.controller.getInfoMesure(FakeRequest(GET,"url"))

      r must equalTo(List())
    }

    "return the list of informations mesure in session" in new WithApplication{
      val f=fixture
      val session="""[{"sensor":""""+bson.stringify+"""","info":{"index":0,"id":"id","mesure":"mesure","unite":"unite"}}]"""

      val r=f.controller.getInfoMesure(FakeRequest(GET,"url").withSession("infoMesure"->session))

      r must equalTo(List((bson,InfoMesureForm(0,"id","mesure","unite"))))
    }
  }

  "When method verifyConfiguration is called, ConfigurationManager" should{
    "return the list of errors message pass in paramater if configuration not have error" in new WithApplication{
      val f=fixture
      val errors=List("une erreur")

      val r=f.controller.verifyConfiguration(ConfigurationForm(port="/dev/ttyUSB0",types="ADC"),errors)

      r must equalTo(errors)
    }

    "return the list of errors with new error if port is undefined" in new WithApplication{
      val f=fixture
      val errors=List("une erreur","Le port ou le type configuration ne sont pas définis")

      val r=f.controller.verifyConfiguration(ConfigurationForm(port="",types="ADC"),List("une erreur"))

      r must equalTo(errors)
    }

    "return the list of errors with new error if type is undefined" in new WithApplication{
      val f=fixture
      val errors=List("une erreur","Le port ou le type configuration ne sont pas définis")

      val r=f.controller.verifyConfiguration(ConfigurationForm(port="/dev/ttyUSB0",types=""),List("une erreur"))

      r must equalTo(errors)
    }
  }

  "When method verifyInfoMesure is called, ConfigurationManager" should{
    "return the list of errors message pass in paramater if the list of mesure informations is not empty" in new WithApplication{
      val f=fixture
      val errors=List("une erreur")
      val list=List((bson,mock[InfoMesureForm]))

      val r=f.controller.verifyInfoMesure(list,errors)

      r must equalTo(errors)
    }

    "return the list of errors with new error if the list of mesure informations is empty" in new WithApplication{
      val f=fixture
      val errors=List("une erreur","La configuration doit contenir au moins une information de mesure")

      val r=f.controller.verifyInfoMesure(List(),List("une erreur"))

      r must equalTo(errors)
    }
  }

  "When method display_validation is called, ConfigurationManager" should{
    "send page with the summary of the validation" in new WithApplication{
      val f=fixture
      val config=ConfigurationForm("/dev/ttyUSB0",10,9600,8,1,0,1000,"ADC")
      val infoMesure=List((bson2,InfoMesureForm(0,"id","mesure","unite")))
      val typeSensor=List(TypeSensor(bson3,"typeSensor2","nomTypeSensor2",bson2,"fab2",2,List("esp2"),3.4f,2.3f))
      val typeMesure=List(TypeMesure(bson2,"mesure2","unite2"))
      val module=Module(bson,"id","type",date,List(),List(),Some("un com"))
      val sensor=List(Sensor(bson2,"id",bson3,None,date,None,true,None))

      f.sensorDaoMock.fold(any[JsObject],any[JsObject],org.mockito.Matchers.eq(HashSet[BSONObjectID]()))(any[(HashSet[BSONObjectID],Sensor)=>HashSet[BSONObjectID]])(any[ExecutionContext]) returns future{HashSet[BSONObjectID]()}
      f.typeSensorDaoMock.findAll(any[JsObject],any[JsObject])(any[ExecutionContext]) returns future{typeSensor}
      f.sensorDaoMock.findAll(any[JsObject],any[JsObject])(any[ExecutionContext]) returns future{sensor}
      f.typeMesureDaoMock.findAll(any[JsObject],any[JsObject])(any[ExecutionContext]) returns future{typeMesure}

      val req=FakeRequest(GET,"/inventary/modules/"+bson.stringify+"/configuration/validation").withSession(("user"  -> """{"login":"test"}"""))
      val r=f.controller.display_validation(Results.Ok,module,config,infoMesure,List())(req)

      status(r) must equalTo(OK)
      val content=contentAsString(r)
      content must contain("<h4>type / id</h4>")
      content must contain("<span class=\"bold\">Commentaires</span> : un com")
      content must matchRegex("<span class=\"bold\">Date d&#x27;assemblage</span> : \\d{2}/\\d{2}/\\d{4}")
      content must contain("<span class=\"bold\">Port</span> : /dev/ttyUSB0")
      content must contain("<span class=\"bold\">Délai d&#x27;attente de la connection</span> : 10")
      content must contain("<span class=\"bold\">Baud</span> : 9600")
      content must contain("<span class=\"bold\">Bits de donnée</span> : 8")
      content must contain("<span class=\"bold\">Parité</span> : 0")
      content must contain("<span class=\"bold\">Stop de bits</span> : 1")
      content must contain("<span class=\"bold\">Temps de filtrage des données</span> : 1000")
      content must contain("<span class=\"bold\">Type de configuration</span> : ADC")
      content must contain("<td>0</td>")
      content must contain("<td>id</td>")
      content must contain("<td>mesure</td>")
      content must contain("<td>unite</td>")
      content must contain("<span class=\"bold\">typeSensor2 / nomTypeSensor2</span><br/>id")

      there was one(f.sensorDaoMock).fold(any[JsObject],any[JsObject],org.mockito.Matchers.eq(HashSet[BSONObjectID]()))(any[(HashSet[BSONObjectID],Sensor)=>HashSet[BSONObjectID]])(any[ExecutionContext])
      there was one(f.typeSensorDaoMock).findAll(any[JsObject],any[JsObject])(any[ExecutionContext])
      there was one(f.sensorDaoMock).findAll(any[JsObject],any[JsObject])(any[ExecutionContext])
      there was one(f.typeMesureDaoMock).findAll(any[JsObject],any[JsObject])(any[ExecutionContext])
    }

    "send page with errors" in new WithApplication{
      val f=fixture
      val config=ConfigurationForm(port="",types="")
      val module=Module(bson2,"idMod","typMod",date,List(),List(),Some("un com"))

      f.sensorDaoMock.fold(any[JsObject],any[JsObject],org.mockito.Matchers.eq(HashSet[BSONObjectID]()))(any[(HashSet[BSONObjectID],Sensor)=>HashSet[BSONObjectID]])(any[ExecutionContext]) returns future{HashSet[BSONObjectID]()}
      f.typeSensorDaoMock.findAll(any[JsObject],any[JsObject])(any[ExecutionContext]) returns future{List()}
      f.sensorDaoMock.findAll(any[JsObject],any[JsObject])(any[ExecutionContext]) returns future{List()}
      f.typeMesureDaoMock.findAll(any[JsObject],any[JsObject])(any[ExecutionContext]) returns future{List()}

      val req=FakeRequest(GET,"/inventary/modules/"+bson.stringify+"/configuration/validation").withSession(("user"  -> """{"login":"test"}"""))
      val r=f.controller.display_validation(Results.BadRequest,module,config,List(),List("Une erreur"))(req)

      status(r) must equalTo(BAD_REQUEST)
      val content=contentAsString(r)
      content must contain("<div class=\"alert alert-danger\" role=\"alert\">Une erreur</div>")

      there was one(f.sensorDaoMock).fold(any[JsObject],any[JsObject],org.mockito.Matchers.eq(HashSet[BSONObjectID]()))(any[(HashSet[BSONObjectID],Sensor)=>HashSet[BSONObjectID]])(any[ExecutionContext])
      there was one(f.typeSensorDaoMock).findAll(any[JsObject],any[JsObject])(any[ExecutionContext])
      there was one(f.sensorDaoMock).findAll(any[JsObject],any[JsObject])(any[ExecutionContext])
      there was one(f.typeMesureDaoMock).findAll(any[JsObject],any[JsObject])(any[ExecutionContext])
    }
  }

  "When method insertInformation is called, ConfigurationManager" should{
    "return an empty list if the list of informations is empty" in new WithApplication{
      val f=fixture

      f.controller.insertInformation(List()) must equalTo(List())
    }

    "return a list with id of type mesure" in new WithApplication{
      val f=fixture
      val listInfo=List(
        (bson,InfoMesureForm(0,"id","Tension","Volt")),
        (bson2,InfoMesureForm(1,"id2","Tension2","Volt2"))
      )
      val lastError=mock[LastError]

      f.typeMesureDaoMock.findOne(org.mockito.Matchers.eq(Json.obj("nom"->"Tension","unite"->"Volt")))(any[ExecutionContext]) returns future{Some(TypeMesure(bson,"Tension","Volt"))}
      f.typeMesureDaoMock.findOne(org.mockito.Matchers.eq(Json.obj("nom"->"Tension2","unite"->"Volt2")))(any[ExecutionContext]) returns future{Some(TypeMesure(bson2,"Tension2","Volt2"))}
      f.informationMesureDaoMock.findOne(any[JsObject])(any[ExecutionContext]) returns future{None}
      f.informationMesureDaoMock.insert(any[InformationMesure],any[GetLastError])(any[ExecutionContext]) returns (future{lastError},future{lastError})

      val res=f.controller.insertInformation(listInfo)

      val captor = ArgumentCaptor.forClass(classOf[InformationMesure])

      there was one(f.typeMesureDaoMock).findOne(org.mockito.Matchers.eq(Json.obj("nom"->"Tension","unite"->"Volt")))(any[ExecutionContext])
      there was one(f.typeMesureDaoMock).findOne(org.mockito.Matchers.eq(Json.obj("nom"->"Tension2","unite"->"Volt2")))(any[ExecutionContext])
      there was 2.times(f.informationMesureDaoMock).findOne(any[JsObject])(any[ExecutionContext])
      there was 2.times(f.informationMesureDaoMock).insert(captor.capture(),any[GetLastError])(any[ExecutionContext])

      val args=captor.getAllValues
      args.get(0) must equalTo(InformationMesure(args.get(0)._id,1,"id2",bson2,bson2)) or equalTo(InformationMesure(args.get(0)._id,0,"id",bson,bson))
      args.get(1) must equalTo(InformationMesure(args.get(1)._id,1,"id2",bson2,bson2)) or equalTo(InformationMesure(args.get(1)._id,0,"id",bson,bson))

      Await.result(Future.sequence(res),Duration.Inf) must equalTo(List(args.get(0)._id,args.get(1)._id))
    }
  }

  "When method findIdTypeMesure is called, ConfigurationManager" should{
    "return the _id of the mesure" in new WithApplication{
      val f=fixture
      val mesure=TypeMesure(bson,"Tension","Volt")
      val infoMesure=mock[InfoMesureForm]

      f.controller.findIdTypeMesure(Some(mesure),infoMesure) must equalTo(bson)
    }

    "return the _id of the mesure" in new WithApplication{
      val f=fixture
      val infoMesure=InfoMesureForm(0,"id","Tension","Unite")

      f.typeMesureDaoMock.insert(any[TypeMesure],any[GetLastError])(any[ExecutionContext]) returns mock[Future[LastError]]

      var res=f.controller.findIdTypeMesure(None,infoMesure)

      val captor = ArgumentCaptor.forClass(classOf[TypeMesure])

      there was one(f.typeMesureDaoMock).insert(captor.capture(),any[GetLastError])(any[ExecutionContext])

      res must equalTo(captor.getValue._id)
    }
  }

  "When method insertConfiguration is called, ConfigurationManager" should{
    "Send redirect after insert configuration" in new WithApplication{
      val f=fixture
      val listInfo=List(
        (bson,InfoMesureForm(0,"id","Tension","Volt")),
        (bson2,InfoMesureForm(1,"id2","Tension2","Volt2"))
      )
      val config=ConfigurationForm(port="/dev/ttyUSB0",types="ADC")
      val lastError=mock[LastError]

      f.typeMesureDaoMock.findOne(org.mockito.Matchers.eq(Json.obj("nom"->"Tension","unite"->"Volt")))(any[ExecutionContext]) returns future{Some(TypeMesure(bson,"Tension","Volt"))}
      f.typeMesureDaoMock.findOne(org.mockito.Matchers.eq(Json.obj("nom"->"Tension2","unite"->"Volt2")))(any[ExecutionContext]) returns future{Some(TypeMesure(bson2,"Tension2","Volt2"))}
      f.informationMesureDaoMock.findOne(any[JsObject])(any[ExecutionContext]) returns future{None}
      f.informationMesureDaoMock.insert(any[InformationMesure],any[GetLastError])(any[ExecutionContext]) returns future{lastError}
      f.configurationDaoMock.insert(any[Configuration],any[GetLastError])(any[ExecutionContext]) returns future{lastError}
      f.moduleDaoMock.updateById(org.mockito.Matchers.eq(bson),any[JsObject],any[GetLastError])(any[Writes[JsObject]],any[ExecutionContext]) returns future{lastError}

      val req=FakeRequest(POST,"/inventary/modules/"+bson.stringify+"/configuration/validation").withSession(("user"  -> """{"login":"test"}"""),("config"->""),("configForm"->""),("infoMesure"->""))
      val res=f.controller.insertConfiguration(bson.stringify,config,listInfo)(req)

      status(res) must equalTo(SEE_OTHER)
      header("Location",res) must beSome("/inventary/modules/"+bson.stringify)
      val s=session(res)
      s.get("config") must beNone
      s.get("configForm") must beNone
      s.get("infoMesure") must beNone

      val captor = ArgumentCaptor.forClass(classOf[InformationMesure])
      val captorConfig=ArgumentCaptor.forClass(classOf[Configuration])
      val captorObj=ArgumentCaptor.forClass(classOf[JsObject])

      there was one(f.typeMesureDaoMock).findOne(org.mockito.Matchers.eq(Json.obj("nom"->"Tension","unite"->"Volt")))(any[ExecutionContext])
      there was one(f.typeMesureDaoMock).findOne(org.mockito.Matchers.eq(Json.obj("nom"->"Tension2","unite"->"Volt2")))(any[ExecutionContext])
      there was 2.times(f.informationMesureDaoMock).findOne(any[JsObject])(any[ExecutionContext])
      there was 2.times(f.informationMesureDaoMock).insert(captor.capture(),any[GetLastError])(any[ExecutionContext])
      there was one(f.configurationDaoMock).insert(captorConfig.capture(),any[GetLastError])(any[ExecutionContext])
      there was one(f.moduleDaoMock).updateById(org.mockito.Matchers.eq(bson),captorObj.capture(),any[GetLastError])(any[Writes[JsObject]],any[ExecutionContext])

      val args=captor.getAllValues
      args.get(0) must equalTo(InformationMesure(args.get(0)._id,1,"id2",bson2,bson2)) or equalTo(InformationMesure(args.get(0)._id,0,"id",bson,bson))
      args.get(1) must equalTo(InformationMesure(args.get(1)._id,1,"id2",bson2,bson2)) or equalTo(InformationMesure(args.get(1)._id,0,"id",bson,bson))

      val configCapt=captorConfig.getValue
      configCapt must equalTo(Configuration(_id=configCapt._id,port="/dev/ttyUSB0",types="ADC",infoMesure=List(args.get(0)._id,args.get(1)._id))) or equalTo(Configuration(_id=configCapt._id,port="/dev/ttyUSB0",types="ADC",infoMesure=List(args.get(1)._id,args.get(0)._id)))
      captorObj.getValue must equalTo(Json.obj("$push"->Json.obj("configuration"->configCapt._id)))
    }
  }

  "When method updateConfiguration is called, ConfigurationManager" should{
    "Send redirect after update configuration" in new WithApplication{
      val f=fixture
      val listInfo=List(
        (bson,InfoMesureForm(0,"id","Tension","Volt")),
        (bson2,InfoMesureForm(1,"id2","Tension2","Volt2"))
      )
      val config=ConfigurationForm(port="/dev/ttyUSB0",types="ADC")
      val lastError=mock[LastError]

      f.typeMesureDaoMock.findOne(org.mockito.Matchers.eq(Json.obj("nom"->"Tension","unite"->"Volt")))(any[ExecutionContext]) returns future{Some(TypeMesure(bson,"Tension","Volt"))}
      f.typeMesureDaoMock.findOne(org.mockito.Matchers.eq(Json.obj("nom"->"Tension2","unite"->"Volt2")))(any[ExecutionContext]) returns future{Some(TypeMesure(bson2,"Tension2","Volt2"))}
      f.informationMesureDaoMock.findOne(any[JsObject])(any[ExecutionContext]) returns future{None}
      f.informationMesureDaoMock.insert(any[InformationMesure],any[GetLastError])(any[ExecutionContext]) returns future{lastError}
      f.configurationDaoMock.updateById(org.mockito.Matchers.eq(bson2),any[Configuration],any[GetLastError])(any[Writes[Configuration]],any[ExecutionContext]) returns future{lastError}

      val req=FakeRequest(POST,"/inventary/modules/"+bson.stringify+"/configuration/validation").withSession(("user"  -> """{"login":"test"}"""),("config"->""),("configForm"->""),("infoMesure"->""))
      val res=f.controller.updateConfiguration(bson.stringify,bson2.stringify,config,listInfo)(req)

      status(res) must equalTo(SEE_OTHER)
      header("Location",res) must beSome("/inventary/modules/"+bson.stringify)
      val s=session(res)
      s.get("config") must beNone
      s.get("configForm") must beNone
      s.get("infoMesure") must beNone

      val captor = ArgumentCaptor.forClass(classOf[InformationMesure])
      val captorConfig=ArgumentCaptor.forClass(classOf[Configuration])

      there was one(f.typeMesureDaoMock).findOne(org.mockito.Matchers.eq(Json.obj("nom"->"Tension","unite"->"Volt")))(any[ExecutionContext])
      there was one(f.typeMesureDaoMock).findOne(org.mockito.Matchers.eq(Json.obj("nom"->"Tension2","unite"->"Volt2")))(any[ExecutionContext])
      there was 2.times(f.informationMesureDaoMock).findOne(any[JsObject])(any[ExecutionContext])
      there was 2.times(f.informationMesureDaoMock).insert(captor.capture(),any[GetLastError])(any[ExecutionContext])
      there was one(f.configurationDaoMock).updateById(org.mockito.Matchers.eq(bson2),captorConfig.capture(),any[GetLastError])(any[Writes[Configuration]],any[ExecutionContext])

      val args=captor.getAllValues
      args.get(0) must equalTo(InformationMesure(args.get(0)._id,1,"id2",bson2,bson2)) or equalTo(InformationMesure(args.get(0)._id,0,"id",bson,bson))
      args.get(1) must equalTo(InformationMesure(args.get(1)._id,1,"id2",bson2,bson2)) or equalTo(InformationMesure(args.get(1)._id,0,"id",bson,bson))

      val configCapt=captorConfig.getValue
      configCapt must equalTo(Configuration(_id=configCapt._id,port="/dev/ttyUSB0",types="ADC",infoMesure=List(args.get(0)._id,args.get(1)._id))) or equalTo(Configuration(_id=configCapt._id,port="/dev/ttyUSB0",types="ADC",infoMesure=List(args.get(1)._id,args.get(0)._id)))
    }
  }

  "When method write_info_mesure_properties is called, ConfigurationManager" should{
    "do nothing if the list is empty" in new WithApplication{
      val f=fixture
      val zip=mock[ZipOutputStream]

      f.controller.write_info_mesure_properties(zip,List(),null,0)

      there was no(zip).write(any[Array[Byte]])
    }

    "write informations mesure if sensor is equal to previous sensor" in new WithApplication{
      val f=fixture
      val zip=mock[ZipOutputStream]
      val listInfo=List(InformationMesure(bson,0,"id",bson2,bson3))

      org.mockito.Mockito.doNothing().when(zip).write(org.mockito.Matchers.eq("{\"index\":0,".getBytes))
      org.mockito.Mockito.doNothing().when(zip).write(org.mockito.Matchers.eq(("\"infoId\":\""+bson.stringify+"\"}").getBytes))
      org.mockito.Mockito.doNothing().when(zip).write(org.mockito.Matchers.eq("\\\n]".getBytes))

      f.controller.write_info_mesure_properties(zip,listInfo,bson2,0)

      there was one(zip).write(org.mockito.Matchers.eq("{\"index\":0,".getBytes))
      there was one(zip).write(org.mockito.Matchers.eq(("\"infoId\":\""+bson.stringify+"\"}").getBytes))
      there was one(zip).write(org.mockito.Matchers.eq("\\\n]".getBytes))
    }

    "write informations mesure if sensor is equal to previous sensor with 2 elements in the list" in new WithApplication{
      val f=fixture
      val zip=mock[ZipOutputStream]
      val listInfo=List(InformationMesure(bson,0,"id",bson2,bson3),InformationMesure(bson4,1,"id2",bson2,bson3))

      org.mockito.Mockito.doNothing().when(zip).write(org.mockito.Matchers.eq("{\"index\":0,".getBytes))
      org.mockito.Mockito.doNothing().when(zip).write(org.mockito.Matchers.eq(("\"infoId\":\""+bson.stringify+"\"}").getBytes))
      org.mockito.Mockito.doNothing().when(zip).write(org.mockito.Matchers.eq(",\\\n".getBytes))
      org.mockito.Mockito.doNothing().when(zip).write(org.mockito.Matchers.eq("{\"index\":1,".getBytes))
      org.mockito.Mockito.doNothing().when(zip).write(org.mockito.Matchers.eq(("\"infoId\":\""+bson4.stringify+"\"}").getBytes))
      org.mockito.Mockito.doNothing().when(zip).write(org.mockito.Matchers.eq("\\\n]".getBytes))

      f.controller.write_info_mesure_properties(zip,listInfo,bson2,0)

      there was one(zip).write(org.mockito.Matchers.eq("{\"index\":0,".getBytes))
      there was one(zip).write(org.mockito.Matchers.eq(("\"infoId\":\""+bson.stringify+"\"}").getBytes))
      there was one(zip).write(org.mockito.Matchers.eq(",\\\n".getBytes))
      there was one(zip).write(org.mockito.Matchers.eq("{\"index\":1,".getBytes))
      there was one(zip).write(org.mockito.Matchers.eq(("\"infoId\":\""+bson4.stringify+"\"}").getBytes))
      there was one(zip).write(org.mockito.Matchers.eq("\\\n]".getBytes))
    }

    "write informations mesure if sensor is not equal to previous sensor" in new WithApplication{
      val f=fixture
      val zip=mock[ZipOutputStream]
      val listInfo=List(InformationMesure(bson,0,"id",bson2,bson3))

      org.mockito.Mockito.doNothing().when(zip).write(org.mockito.Matchers.eq(("\\\n],\\\n\""+bson2.stringify+"\":[\\\n").getBytes))
      org.mockito.Mockito.doNothing().when(zip).write(org.mockito.Matchers.eq("{\"index\":0,".getBytes))
      org.mockito.Mockito.doNothing().when(zip).write(org.mockito.Matchers.eq(("\"infoId\":\""+bson.stringify+"\"}").getBytes))
      org.mockito.Mockito.doNothing().when(zip).write(org.mockito.Matchers.eq("\\\n]".getBytes))

      f.controller.write_info_mesure_properties(zip,listInfo,bson4,0)

      there was one(zip).write(org.mockito.Matchers.eq(("\\\n],\\\n\""+bson2.stringify+"\":[\\\n").getBytes))
      there was one(zip).write(org.mockito.Matchers.eq("{\"index\":0,".getBytes))
      there was one(zip).write(org.mockito.Matchers.eq(("\"infoId\":\""+bson.stringify+"\"}").getBytes))
      there was one(zip).write(org.mockito.Matchers.eq("\\\n]".getBytes))
    }
  }

  "When method write_sensors_properties is called, ConfigurationManager" should{
    "do nothing if the list is empty" in new WithApplication{
      val f=fixture
      val zip=mock[ZipOutputStream]

      f.controller.write_sensors_properties(zip,List(),null)

      there was no(zip).write(any[Array[Byte]])
    }

    "write the end of an array if the list is empty and sensors informations are write" in new WithApplication{
      val f=fixture
      val zip=mock[ZipOutputStream]

      org.mockito.Mockito.doNothing().when(zip).write(org.mockito.Matchers.eq(("\\\n]").getBytes))

      f.controller.write_sensors_properties(zip,List(),bson)

      there was one(zip).write(org.mockito.Matchers.eq(("\\\n]").getBytes))
    }

    "write sensor informations if have one elements in the list" in new WithApplication{
      val f=fixture
      val zip=mock[ZipOutputStream]
      val listInfo=List(InformationMesure(bson,0,"id",bson2,bson3))

      org.mockito.Mockito.doNothing().when(zip).write(org.mockito.Matchers.eq(("\""+bson2.stringify+"\":[\\\n").getBytes))
      org.mockito.Mockito.doNothing().when(zip).write(org.mockito.Matchers.eq("{\"index\":0,".getBytes))
      org.mockito.Mockito.doNothing().when(zip).write(org.mockito.Matchers.eq(("\"infoId\":\""+bson.stringify+"\"}").getBytes))
      org.mockito.Mockito.doNothing().when(zip).write(org.mockito.Matchers.eq("\\\n]".getBytes))

      f.controller.write_sensors_properties(zip,listInfo,null)

      there was one(zip).write(org.mockito.Matchers.eq(("\""+bson2.stringify+"\":[\\\n").getBytes))
      there was one(zip).write(org.mockito.Matchers.eq("{\"index\":0,".getBytes))
      there was one(zip).write(org.mockito.Matchers.eq(("\"infoId\":\""+bson.stringify+"\"}").getBytes))
      there was one(zip).write(org.mockito.Matchers.eq("\\\n]".getBytes))
    }

    "write sensor informations if have many elements in the list" in new WithApplication{
      val f=fixture
      val zip=mock[ZipOutputStream]
      val listInfo=List(InformationMesure(bson,0,"id",bson2,bson3),InformationMesure(bson4,1,"id2",bson3,bson2))

      org.mockito.Mockito.doNothing().when(zip).write(org.mockito.Matchers.eq(("\""+bson2.stringify+"\":[\\\n").getBytes))
      org.mockito.Mockito.doNothing().when(zip).write(org.mockito.Matchers.eq("{\"index\":0,".getBytes))
      org.mockito.Mockito.doNothing().when(zip).write(org.mockito.Matchers.eq(("\"infoId\":\""+bson.stringify+"\"}").getBytes))
      org.mockito.Mockito.doNothing().when(zip).write(org.mockito.Matchers.eq(("\\\n],\\\n\""+bson3.stringify+"\":[\\\n").getBytes))
      org.mockito.Mockito.doNothing().when(zip).write(org.mockito.Matchers.eq("{\"index\":1,".getBytes))
      org.mockito.Mockito.doNothing().when(zip).write(org.mockito.Matchers.eq(("\"infoId\":\""+bson4.stringify+"\"}").getBytes))
      org.mockito.Mockito.doNothing().when(zip).write(org.mockito.Matchers.eq("\\\n]".getBytes))

      f.controller.write_sensors_properties(zip,listInfo,null)

      there was one(zip).write(org.mockito.Matchers.eq(("\""+bson2.stringify+"\":[\\\n").getBytes))
      there was one(zip).write(org.mockito.Matchers.eq("{\"index\":0,".getBytes))
      there was one(zip).write(org.mockito.Matchers.eq(("\"infoId\":\""+bson.stringify+"\"}").getBytes))
      there was one(zip).write(org.mockito.Matchers.eq(("\\\n],\\\n\""+bson3.stringify+"\":[\\\n").getBytes))
      there was one(zip).write(org.mockito.Matchers.eq("{\"index\":1,".getBytes))
      there was one(zip).write(org.mockito.Matchers.eq(("\"infoId\":\""+bson4.stringify+"\"}").getBytes))
      there was one(zip).write(org.mockito.Matchers.eq("\\\n]".getBytes))
    }
  }

  "When method write_configuration is called, ConfigurationManager" should{
    "Write all configuration informations in the zip" in new WithApplication{
      val f=fixture
      val zip=mock[ZipOutputStream]
      val config=Configuration(port="/dev/ttyUSB0",types="ADC",infoMesure = List())

      org.mockito.Mockito.doNothing().when(zip).write(org.mockito.Matchers.eq(("device=/dev/ttyUSB0\n").getBytes))
      org.mockito.Mockito.doNothing().when(zip).write(org.mockito.Matchers.eq("timeout=10000\n".getBytes))
      org.mockito.Mockito.doNothing().when(zip).write(org.mockito.Matchers.eq(("baud=9600\n").getBytes))
      org.mockito.Mockito.doNothing().when(zip).write(org.mockito.Matchers.eq(("bits=8\n").getBytes))
      org.mockito.Mockito.doNothing().when(zip).write(org.mockito.Matchers.eq(("stopBits=1\n").getBytes))
      org.mockito.Mockito.doNothing().when(zip).write(org.mockito.Matchers.eq(("parity=0\n").getBytes))
      org.mockito.Mockito.doNothing().when(zip).write(org.mockito.Matchers.eq(("timeFilter=1000\n").getBytes))
      org.mockito.Mockito.doNothing().when(zip).write(org.mockito.Matchers.eq(("type=ADC\n").getBytes))

      f.controller.write_configuration(zip,config)

      there was one(zip).write(org.mockito.Matchers.eq(("device=/dev/ttyUSB0\n").getBytes))
      there was one(zip).write(org.mockito.Matchers.eq("timeout=10000\n".getBytes))
      there was one(zip).write(org.mockito.Matchers.eq(("baud=9600\n").getBytes))
      there was one(zip).write(org.mockito.Matchers.eq(("bits=8\n").getBytes))
      there was one(zip).write(org.mockito.Matchers.eq(("stopBits=1\n").getBytes))
      there was one(zip).write(org.mockito.Matchers.eq(("parity=0\n").getBytes))
      there was one(zip).write(org.mockito.Matchers.eq(("timeFilter=1000\n").getBytes))
      there was one(zip).write(org.mockito.Matchers.eq(("type=ADC\n").getBytes))
    }
  }

  "When method create_zip is called, ConfigurationManager" should{
    "create an empty zip if not have configuration" in new WithApplication{
      val f=fixture
      val zip=mock[ZipOutputStream]

      f.zipOutputStreamBuilderMock.createZipOutputStream(any) returns zip
      org.mockito.Mockito.doNothing().when(zip).close()

      f.controller.create_zip(bson.stringify,List(),List())

      there was one(f.zipOutputStreamBuilderMock).createZipOutputStream(any)
      there was no(zip).write(any[Array[Byte]])
      there was no(zip).putNextEntry(any[ZipEntry])
      there was no(zip).closeEntry()
      there was one(zip).close()
    }

    "create a zip with files generate by the configurations" in new WithApplication{
      val f=fixture
      val zip=mock[ZipOutputStream]
      val config=List(Configuration(port="/dev/ttyUSB0",types="ADC",infoMesure=List(bson3)),Configuration(port="/dev/ttyUSB0",types="wasp",infoMesure=List(bson4)))
      val info=List(InformationMesure(bson3,0,"id",bson2,bson3),InformationMesure(bson4,0,"id2",bson3,bson2))

      f.zipOutputStreamBuilderMock.createZipOutputStream(any[OutputStream]) returns zip
      org.mockito.Mockito.doNothing().when(zip).putNextEntry(org.mockito.Matchers.eq(new ZipEntry("ADC.properties")))
      org.mockito.Mockito.doNothing().when(zip).putNextEntry(org.mockito.Matchers.eq(new ZipEntry("wasp.properties")))
      org.mockito.Mockito.doNothing().when(zip).write(org.mockito.Matchers.eq(("device=/dev/ttyUSB0\n").getBytes))
      org.mockito.Mockito.doNothing().when(zip).write(org.mockito.Matchers.eq("timeout=10000\n".getBytes))
      org.mockito.Mockito.doNothing().when(zip).write(org.mockito.Matchers.eq(("baud=9600\n").getBytes))
      org.mockito.Mockito.doNothing().when(zip).write(org.mockito.Matchers.eq(("bits=8\n").getBytes))
      org.mockito.Mockito.doNothing().when(zip).write(org.mockito.Matchers.eq(("stopBits=1\n").getBytes))
      org.mockito.Mockito.doNothing().when(zip).write(org.mockito.Matchers.eq(("parity=0\n").getBytes))
      org.mockito.Mockito.doNothing().when(zip).write(org.mockito.Matchers.eq(("timeFilter=1000\n").getBytes))
      org.mockito.Mockito.doNothing().when(zip).write(org.mockito.Matchers.eq(("type=ADC\n").getBytes))
      org.mockito.Mockito.doNothing().when(zip).write(org.mockito.Matchers.eq(("type=wasp\n").getBytes))
      org.mockito.Mockito.doNothing().when(zip).write(org.mockito.Matchers.eq(("moduleId="+bson.stringify+"\n").getBytes))
      org.mockito.Mockito.doNothing().when(zip).write(org.mockito.Matchers.eq(("sensors={\\\n").getBytes))
      org.mockito.Mockito.doNothing().when(zip).write(org.mockito.Matchers.eq(("\""+bson3.stringify+"\":[\\\n").getBytes))
      org.mockito.Mockito.doNothing().when(zip).write(org.mockito.Matchers.eq(("\""+bson2.stringify+"\":[\\\n").getBytes))
      org.mockito.Mockito.doNothing().when(zip).write(org.mockito.Matchers.eq("{\"index\":0,".getBytes))
      org.mockito.Mockito.doNothing().when(zip).write(org.mockito.Matchers.eq(("\"infoId\":\""+bson3.stringify+"\"}").getBytes))
      org.mockito.Mockito.doNothing().when(zip).write(org.mockito.Matchers.eq(("\"infoId\":\""+bson4.stringify+"\"}").getBytes))
      org.mockito.Mockito.doNothing().when(zip).write(org.mockito.Matchers.eq("\\\n]".getBytes))
      org.mockito.Mockito.doNothing().when(zip).write(org.mockito.Matchers.eq(("}").getBytes))
      org.mockito.Mockito.doNothing().when(zip).closeEntry()
      org.mockito.Mockito.doNothing().when(zip).close()

      f.controller.create_zip(bson.stringify,config,info)

      there was one(f.zipOutputStreamBuilderMock).createZipOutputStream(any[OutputStream])
      there was 2.times(zip).putNextEntry(any[ZipEntry])
      there was 2.times(zip).write(org.mockito.Matchers.eq(("device=/dev/ttyUSB0\n").getBytes))
      there was 2.times(zip).write(org.mockito.Matchers.eq("timeout=10000\n".getBytes))
      there was 2.times(zip).write(org.mockito.Matchers.eq(("baud=9600\n").getBytes))
      there was 2.times(zip).write(org.mockito.Matchers.eq(("bits=8\n").getBytes))
      there was 2.times(zip).write(org.mockito.Matchers.eq(("stopBits=1\n").getBytes))
      there was 2.times(zip).write(org.mockito.Matchers.eq(("parity=0\n").getBytes))
      there was 2.times(zip).write(org.mockito.Matchers.eq(("timeFilter=1000\n").getBytes))
      there was one(zip).write(org.mockito.Matchers.eq(("type=ADC\n").getBytes))
      there was one(zip).write(org.mockito.Matchers.eq(("type=wasp\n").getBytes))
      there was 2.times(zip).write(org.mockito.Matchers.eq(("moduleId="+bson.stringify+"\n").getBytes))
      there was 2.times(zip).write(org.mockito.Matchers.eq(("sensors={\\\n").getBytes))
      there was one(zip).write(org.mockito.Matchers.eq(("\""+bson3.stringify+"\":[\\\n").getBytes))
      there was one(zip).write(org.mockito.Matchers.eq(("\""+bson2.stringify+"\":[\\\n").getBytes))
      there was 2.times(zip).write(org.mockito.Matchers.eq("{\"index\":0,".getBytes))
      there was one(zip).write(org.mockito.Matchers.eq(("\"infoId\":\""+bson3.stringify+"\"}").getBytes))
      there was one(zip).write(org.mockito.Matchers.eq(("\"infoId\":\""+bson4.stringify+"\"}").getBytes))
      there was 2.times(zip).write(org.mockito.Matchers.eq("\\\n]".getBytes))
      there was 2.times(zip).write(org.mockito.Matchers.eq(("}").getBytes))
      there was 2.times(zip).closeEntry()
      there was one(zip).close()
    }
  }

  "When method findInformationForForm is called, ConfigurationManager" should{
    "return a list with sensor and its mesure informations" in new WithApplication{
      val f=fixture
      val infos=List(bson,bson2)
      val listInfos=List(
        InformationMesure(bson,0,"id",bson3,bson4),
        InformationMesure(bson2,1,"id2",bson4,bson3)
      )
      val mesures=List(
        TypeMesure(bson4,"Tension","Volt"),
        TypeMesure(bson3,"Tension2","Volt2")
      )

      f.informationMesureDaoMock.findAll(org.mockito.Matchers.eq(Json.obj("_id"->Json.obj("$in"->infos))),any[JsObject])(any[ExecutionContext]) returns future{listInfos}
      f.typeMesureDaoMock.findAll(org.mockito.Matchers.eq(Json.obj("_id"->Json.obj("$in"->List(bson3,bson4)))),any[JsObject])(any[ExecutionContext]) returns future{mesures}

      val res=f.controller.findInformationForForm(infos)

      Await.result(res,Duration.Inf) must equalTo(List(
        Json.obj("sensor"->bson4.stringify,"info"->Json.obj("index"->1,"id"->"id2","mesure"->"Tension2","unite"->"Volt2")),
        Json.obj("sensor"->bson3.stringify,"info"->Json.obj("index"->0,"id"->"id","mesure"->"Tension","unite"->"Volt"))
      ))

      there was one(f.informationMesureDaoMock).findAll(org.mockito.Matchers.eq(Json.obj("_id"->Json.obj("$in"->infos))),any[JsObject])(any[ExecutionContext])
      there was one(f.typeMesureDaoMock).findAll(org.mockito.Matchers.eq(Json.obj("_id"->Json.obj("$in"->List(bson3,bson4)))),any[JsObject])(any[ExecutionContext])
    }
  }

  "return an empty list if not have mesure informations" in new WithApplication{
    val f=fixture
    val res=f.controller.findInformationForForm(List())

    Await.result(res,Duration.Inf) must equalTo(List())

    there was no(f.informationMesureDaoMock).findAll(any[JsObject],any[JsObject])(any[ExecutionContext])
    there was no(f.typeMesureDaoMock).findAll(any[JsObject],any[JsObject])(any[ExecutionContext])
  }
}
