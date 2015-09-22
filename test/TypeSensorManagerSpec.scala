import java.util.Date

import controllers._
import models._
import org.junit.runner.RunWith
import org.specs2.matcher.{MatchResult, Expectable, Matcher}
import org.specs2.mock.Mockito
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner
import play.api.data.Form
import play.api.http.Writeable
import play.api.i18n.Lang
import play.api.libs.json.{JsArray, Writes, Json, JsObject}
import play.api.mvc._
import play.api.test._
import play.api.test.Helpers._
import play.modules.reactivemongo.json.BSONFormats.BSONObjectIDFormat
import reactivemongo.bson.{BSONDocument, BSONObjectID}
import reactivemongo.core.commands.{GetLastError, LastError}

import scala.concurrent._
import scala.concurrent.duration.Duration

@RunWith(classOf[JUnitRunner])
class TypeSensorManagerSpec extends Specification with Mockito{
  class TypeSensorManagerTest extends TypeSensorManagerLike

  case class matchRegex(a: String) extends Matcher[String](){
    override def apply[S <: String](t: Expectable[S]): MatchResult[S] = result(a.r.findFirstIn(t.value).nonEmpty, "okMessage", "not found "+a,t)
  }

  case class contains(a: String,b:Int) extends Matcher[String](){
    override def apply[S <: String](t: Expectable[S]): MatchResult[S] = result(a.r.findAllMatchIn(t.value).size==b, "okMessage", "not found "+b+" times but "+a.r.findAllMatchIn(t.value).size+" times "+a,t)
  }

  val bson=BSONObjectID.generate
  val bson2=BSONObjectID.generate
  val bson3=BSONObjectID.generate
  val bson4=BSONObjectID.generate

  def fixture=new{
    val typeSensorDaoMock = mock[TypeSensorDao]
    val typeMesureDaoMock = mock[TypeMesureDao]
    val sensorDaoMock = mock[SensorDao]
    val especeDaoMock = mock[EspeceDao]
    val controller = new TypeSensorManagerTest {
      override val typeSensorDao: TypeSensorDao = typeSensorDaoMock
      override val typeMesureDao: TypeMesureDao = typeMesureDaoMock
      override val sensorDao: SensorDao = sensorDaoMock
      override val especeDao: EspeceDao=especeDaoMock
    }

    def listDataEmptyStream:Unit={
      typeSensorDaoMock.findListModele(any[String]) returns future{Stream[BSONDocument]()}
      typeSensorDaoMock.findListFabricant(any[String]) returns future{Stream[BSONDocument]()}
      typeSensorDaoMock.findListType(any[String]) returns future{Stream[BSONDocument]()}
    }

    def verifyCallListData:Unit={
      there was one(typeSensorDaoMock).findListModele(any[String])
      there was one(typeSensorDaoMock).findListFabricant(any[String])
      there was one(typeSensorDaoMock).findListType(any[String])
    }
  }

  "When user is not connected, TypeSensorManager" should {
    "redirect to login for resource /inventary/sensors" in new WithApplication{
      route(FakeRequest(GET, "/inventary/sensors")).map(
        r=>{
          status(r) must equalTo(SEE_OTHER)
          header("Location",r) must equalTo(Some("/login"))
        }
      ).getOrElse(
        failure("Pas de retour de la fonction")
      )
    }

    "redirect to login for GET resource /inventary/sensors/type" in new WithApplication{
      route(FakeRequest(GET, "/inventary/sensors/type")).map(
        r=>{
          status(r) must equalTo(SEE_OTHER)
          header("Location",r) must equalTo(Some("/login"))
        }
      ).getOrElse(
        failure("Pas de retour de la fonction")
      )
    }

    "redirect to login for POST resource /inventary/sensors/type" in new WithApplication{
      route(FakeRequest(POST, "/inventary/sensors/type")).map(
        r=>{
          status(r) must equalTo(SEE_OTHER)
          header("Location",r) must equalTo(Some("/login"))
        }
      ).getOrElse(
        failure("Pas de retour de la fonction")
      )
    }

    "redirect to login for GET resource /inventary/sensors/:id/update" in new WithApplication{
      route(FakeRequest(GET, "/inventary/sensors/"+bson.stringify+"/update")).map(
        r=>{
          status(r) must equalTo(SEE_OTHER)
          header("Location",r) must equalTo(Some("/login"))
        }
      ).getOrElse(
        failure("Pas de retour de la fonction")
      )
    }

    "redirect to login for POST resource /inventary/sensors/:id/update" in new WithApplication{
      route(FakeRequest(POST, "/inventary/sensors/"+bson.stringify+"/update")).map(
        r=>{
          status(r) must equalTo(SEE_OTHER)
          header("Location",r) must equalTo(Some("/login"))
        }
      ).getOrElse(
        failure("Pas de retour de la fonction")
      )
    }

    "redirect to login for GET resource /inventary/sensors/:id/delete" in new WithApplication{
      route(FakeRequest(GET, "/inventary/sensors/"+bson.stringify+"/delete")).map(
        r=>{
          status(r) must equalTo(SEE_OTHER)
          header("Location",r) must equalTo(Some("/login"))
        }
      ).getOrElse(
        failure("Pas de retour de la fonction")
      )
    }
  }

  "When user is on the resource /inventary/sensors , TypeSensorManager" should {
    "send 200 on OK with the message 'Aucun résultat trouvé'" in new WithApplication {
      val f = fixture

      f.sensorDaoMock.countByType() returns future {
        Stream[BSONDocument]()
      }
      f.typeSensorDaoMock.findAll(any[JsObject], any[JsObject])(any[ExecutionContext]) returns future {
        List[TypeSensor]()
      }
      f.typeMesureDaoMock.findAll(any[JsObject], any[JsObject])(any[ExecutionContext]) returns future {
        List[TypeMesure]()
      }
      f.especeDaoMock.findAll(any[JsObject],any[JsObject])(any[ExecutionContext]) returns future{
        List[Espece]()
      }
      f.typeSensorDaoMock.findAllType(any[BSONDocument]) returns future {
        Stream[BSONDocument]()
      }

      f.sensorDaoMock.countUsedSensors(any[List[TypeSensor]]) returns future{List()}

      val r = f.controller.inventary().apply(FakeRequest(GET, "/inventary/sensors").withSession("user" -> """{"login":"test"}"""))
      status(r) must equalTo(OK)
      contentType(r) must beSome.which(_ == "text/html")
      val content = contentAsString(r)
      content must contain("<title>Inventaire des capteurs</title>")
      content must contain("<h3 style=\"text-align:center\">Aucun résultat trouvé</h3>")

      there was one(f.sensorDaoMock).countByType()
      there was one(f.typeSensorDaoMock).findAll(any[JsObject], any[JsObject])(any[ExecutionContext])
      there was one(f.typeMesureDaoMock).findAll(any[JsObject], any[JsObject])(any[ExecutionContext])
      there was one(f.especeDaoMock).findAll(any[JsObject],any[JsObject])(any[ExecutionContext])
      there was one(f.typeSensorDaoMock).findAllType(any[BSONDocument])
      there was one(f.sensorDaoMock).countUsedSensors(List())
    }

    "send 200 on OK with 1 result" in new WithApplication {
      val f = fixture
      val typeSensor=List(TypeSensor(bson, "type1", "modele1", "fab1", 1, List(bson3)))

      f.sensorDaoMock.countByType() returns future {
        Stream[BSONDocument](BSONDocument("_id" -> bson, "count" -> 5))
      }
      f.typeSensorDaoMock.findAll(any[JsObject], any[JsObject])(any[ExecutionContext]) returns future {
        typeSensor
      }
      f.typeMesureDaoMock.findAll(any[JsObject], any[JsObject])(any[ExecutionContext]) returns future {
        List[TypeMesure](TypeMesure(bson2, "mesure1", "unite1"))
      }
      f.especeDaoMock.findAll(any[JsObject],any[JsObject])(any[ExecutionContext]) returns future{
        List[Espece](Espece(bson3,"esp1",bson2,2.3f,4.5f))
      }
      f.typeSensorDaoMock.findAllType(any[BSONDocument]) returns future {
        Stream[BSONDocument]()
      }

      f.sensorDaoMock.countUsedSensors(org.mockito.Matchers.eq(typeSensor)) returns future{List((bson,3))}

      val r = f.controller.inventary().apply(FakeRequest(GET, "/inventary/sensors").withSession("user" -> """{"login":"test"}"""))
      status(r) must equalTo(OK)
      contentType(r) must beSome.which(_ == "text/html")
      val content = contentAsString(r)
      content must contain("<title>Inventaire des capteurs</title>")
      content must not contain ("<h3 style=\"text-align:center\">Aucun résultat trouvé</h3>")
      content must contain("type1")
      content must contain("modele1")
      content must matchRegex("<span class=\"bold\">\\s*Fabricant\\s*</span>\\s*:\\s*fab1")
      content must matchRegex("<span class=\"bold\">\\s*Nombre de signaux\\s*</span>\\s*:\\s*1")
      content must contain("<td>esp1</td>")
      content must contain("<td>mesure1</td>")
      content must contain("<td>2.3 unite1</td>")
      content must contain("<td>4.5 unite1</td>")
      content must matchRegex("<span class=\"bold\">\\s*Stock\\s*</span>\\s*:\\s*2 / 5")

      there was one(f.sensorDaoMock).countByType()
      there was one(f.typeSensorDaoMock).findAll(any[JsObject], any[JsObject])(any[ExecutionContext])
      there was one(f.typeMesureDaoMock).findAll(any[JsObject], any[JsObject])(any[ExecutionContext])
      there was one(f.especeDaoMock).findAll(any[JsObject],any[JsObject])(any[ExecutionContext])
      there was one(f.typeSensorDaoMock).findAllType(any[BSONDocument])
      there was one(f.sensorDaoMock).countUsedSensors(org.mockito.Matchers.eq(typeSensor))
    }

    "send 200 on OK with 2 results" in new WithApplication {
      val f = fixture
      val typeSensor=List(
        TypeSensor(bson, "type1", "modele1","fab1", 1, List(bson3)),
        TypeSensor(bson3, "type2", "modele2","fab2", 2, List(bson2))
      )

      f.sensorDaoMock.countByType() returns future {
        Stream[BSONDocument](BSONDocument("_id" -> bson, "count" -> 5), BSONDocument("_id" -> bson3, "count" -> 0))
      }

      f.typeSensorDaoMock.findAll(any[JsObject], any[JsObject])(any[ExecutionContext]) returns future {
       typeSensor
      }
      f.typeMesureDaoMock.findAll(any[JsObject], any[JsObject])(any[ExecutionContext]) returns future {
        List[TypeMesure](TypeMesure(bson2, "mesure1", "unite1"), TypeMesure(bson4, "mesure2", "unite2"))
      }
      f.especeDaoMock.findAll(any[JsObject],any[JsObject])(any[ExecutionContext]) returns future{
        List[Espece](Espece(bson3,"esp1",bson2,2.3f,4.5f),Espece(bson2,"esp2",bson4,1.2f,3.4f))
      }
      f.typeSensorDaoMock.findAllType(any[BSONDocument]) returns future {
        Stream[BSONDocument]()
      }

      f.sensorDaoMock.countUsedSensors(org.mockito.Matchers.eq(typeSensor)) returns future{List((bson,3),(bson3,0))}

      val r = f.controller.inventary().apply(FakeRequest(GET, "/inventary/sensors").withSession("user" -> """{"login":"test"}"""))

      status(r) must equalTo(OK)
      contentType(r) must beSome.which(_ == "text/html")
      val content = contentAsString(r)
      content must contain("<title>Inventaire des capteurs</title>")
      content must not contain ("<h3 style=\"text-align:center\">Aucun résultat trouvé</h3>")
      content must contain("type1")
      content must contain("modele1")
      content must matchRegex("<span class=\"bold\">\\s*Fabricant\\s*</span>\\s*:\\s*fab1")
      content must matchRegex("<span class=\"bold\">\\s*Nombre de signaux\\s*</span>\\s*:\\s*1")
      content must matchRegex("<span class=\"bold\">\\s*Stock\\s*</span>\\s*:\\s*2 / 5")
      content must contain("<td>esp1</td>")
      content must contain("<td>mesure1</td>")
      content must contain("<td>2.3 unite1</td>")
      content must contain("<td>4.5 unite1</td>")

      content must contain("type2")
      content must contain("modele2")
      content must matchRegex("<span class=\"bold\">\\s*Fabricant\\s*</span>\\s*:\\s*fab2")
      content must matchRegex("<span class=\"bold\">\\s*Nombre de signaux\\s*</span>\\s*:\\s*2")
      content must contain("<td>esp2</td>")
      content must contain("<td>mesure2</td>")
      content must contain("<td>1.2 unite2</td>")
      content must contain("<td>3.4 unite2</td>")
      content must matchRegex("<span class=\"bold\">\\s*Stock\\s*</span>\\s*:\\s*0 / 0")

      there was one(f.sensorDaoMock).countByType()
      there was one(f.typeSensorDaoMock).findAll(any[JsObject], any[JsObject])(any[ExecutionContext])
      there was one(f.typeMesureDaoMock).findAll(any[JsObject], any[JsObject])(any[ExecutionContext])
      there was one(f.especeDaoMock).findAll(any[JsObject],any[JsObject])(any[ExecutionContext])
      there was one(f.typeSensorDaoMock).findAllType(any[BSONDocument])
      there was one(f.sensorDaoMock).countUsedSensors(org.mockito.Matchers.eq(typeSensor))
    }

  }

  "When method getInventaryTypeSensor is called, TypeSensorManager" should{

    "Call function for print result without filter" in new WithApplication {
      val f = fixture
      val func=mock[(List[TypeSensor],List[TypeMesure],List[Espece],List[BSONDocument],List[(BSONObjectID,Int)],List[BSONDocument])=>Result]

      f.sensorDaoMock.countByType() returns future {Stream[BSONDocument]()}
      f.typeSensorDaoMock.findAll(org.mockito.Matchers.eq(Json.obj("_id"->bson)), any[JsObject])(any[ExecutionContext]) returns future {List[TypeSensor]()}
      f.typeMesureDaoMock.findAll(any[JsObject], any[JsObject])(any[ExecutionContext]) returns future {List[TypeMesure]()}
      f.especeDaoMock.findAll(any[JsObject],any[JsObject])(any[ExecutionContext]) returns future{List[Espece]()}
      f.typeSensorDaoMock.findAllType(BSONDocument("_id"->bson)) returns future {Stream[BSONDocument]()}
      f.sensorDaoMock.countUsedSensors(org.mockito.Matchers.eq(List())) returns future{List()}
      func.apply(any[List[TypeSensor]],any[List[TypeMesure]],any[List[Espece]],any[List[BSONDocument]],any[List[(BSONObjectID,Int)]],any[List[BSONDocument]]) returns Results.Ok("call function")

      val req=FakeRequest(GET, "/inventary/sensors").withSession("user" -> """{"login":"test"}""")
      val action = Action.async{f.controller.getInventaryTypeSensor(Json.obj("_id"->bson),"","")(func)}
      val r=call(action,req)
      status(r) must equalTo(OK)
      contentAsString(r) must equalTo("call function")

      there was one(f.sensorDaoMock).countByType()
      there was one(f.typeSensorDaoMock).findAll(org.mockito.Matchers.eq(Json.obj("_id"->bson)), any[JsObject])(any[ExecutionContext])
      there was one(f.typeMesureDaoMock).findAll(any[JsObject], any[JsObject])(any[ExecutionContext])
      there was one(f.typeSensorDaoMock).findAllType(org.mockito.Matchers.eq(BSONDocument("_id"->bson)))
      there was one(f.sensorDaoMock).countUsedSensors(org.mockito.Matchers.eq(List()))
      there was one(func).apply(any[List[TypeSensor]],any[List[TypeMesure]],any[List[Espece]],any[List[BSONDocument]],any[List[(BSONObjectID,Int)]],any[List[BSONDocument]])
      there was one(f.especeDaoMock).findAll(any[JsObject],any[JsObject])(any[ExecutionContext])
    }

    "Call function for print result with type filter" in new WithApplication {
      val f = fixture
      val func=mock[(List[TypeSensor],List[TypeMesure],List[Espece],List[BSONDocument],List[(BSONObjectID,Int)],List[BSONDocument])=>Result]

      f.sensorDaoMock.countByType() returns future {Stream[BSONDocument]()}
      f.typeSensorDaoMock.findAll(org.mockito.Matchers.eq(Json.obj("_id"->bson,"nomType"->"typ")), any[JsObject])(any[ExecutionContext]) returns future {List[TypeSensor]()}
      f.typeMesureDaoMock.findAll(any[JsObject], any[JsObject])(any[ExecutionContext]) returns future {List[TypeMesure]()}
      f.especeDaoMock.findAll(any[JsObject],any[JsObject])(any[ExecutionContext]) returns future{List[Espece]()}
      f.typeSensorDaoMock.findAllType(org.mockito.Matchers.eq(BSONDocument("_id"->bson))) returns future {Stream[BSONDocument]()}
      f.sensorDaoMock.countUsedSensors(org.mockito.Matchers.eq(List())) returns future{List()}
      func.apply(any[List[TypeSensor]],any[List[TypeMesure]],any[List[Espece]],any[List[BSONDocument]],any[List[(BSONObjectID,Int)]],any[List[BSONDocument]]) returns Results.Ok("call function")

      val req=FakeRequest(GET, "/inventary/sensors").withSession("user" -> """{"login":"test"}""")
      val action = Action.async{f.controller.getInventaryTypeSensor(Json.obj("_id"->bson),"typ","")(func)}
      val r=call(action,req)
      status(r) must equalTo(OK)
      contentAsString(r) must equalTo("call function")

      there was one(f.sensorDaoMock).countByType()
      there was one(f.typeSensorDaoMock).findAll(org.mockito.Matchers.eq(Json.obj("_id"->bson,"nomType"->"typ")), any[JsObject])(any[ExecutionContext])
      there was one(f.typeMesureDaoMock).findAll(any[JsObject], any[JsObject])(any[ExecutionContext])
      there was one(f.especeDaoMock).findAll(any[JsObject],any[JsObject])(any[ExecutionContext])
      there was one(f.typeSensorDaoMock).findAllType(org.mockito.Matchers.eq(BSONDocument("_id"->bson)))
      there was one(f.sensorDaoMock).countUsedSensors(org.mockito.Matchers.eq(List()))
      there was one(func).apply(any[List[TypeSensor]],any[List[TypeMesure]],any[List[Espece]],any[List[BSONDocument]],any[List[(BSONObjectID,Int)]],any[List[BSONDocument]])
    }

    "Call function for print result with modele filter" in new WithApplication {
      val f = fixture
      val func=mock[(List[TypeSensor],List[TypeMesure],List[Espece],List[BSONDocument],List[(BSONObjectID,Int)],List[BSONDocument])=>Result]

      f.sensorDaoMock.countByType() returns future {Stream[BSONDocument]()}
      f.typeSensorDaoMock.findAll(org.mockito.Matchers.eq(Json.obj("_id"->bson,"modele"->Json.obj("$regex"->".*mod.*"))), any[JsObject])(any[ExecutionContext]) returns future {List[TypeSensor]()}
      f.typeMesureDaoMock.findAll(any[JsObject], any[JsObject])(any[ExecutionContext]) returns future {List[TypeMesure]()}
      f.especeDaoMock.findAll(any[JsObject],any[JsObject])(any[ExecutionContext]) returns future{List[Espece]()}
      f.typeSensorDaoMock.findAllType(org.mockito.Matchers.eq(BSONDocument("_id"->bson))) returns future {Stream[BSONDocument]()}
      f.sensorDaoMock.countUsedSensors(org.mockito.Matchers.eq(List())) returns future{List()}
      func.apply(any[List[TypeSensor]],any[List[TypeMesure]],any[List[Espece]],any[List[BSONDocument]],any[List[(BSONObjectID,Int)]],any[List[BSONDocument]]) returns Results.Ok("call function")

      val req=FakeRequest(GET, "/inventary/sensors").withSession("user" -> """{"login":"test"}""")
      val action = Action.async{f.controller.getInventaryTypeSensor(Json.obj("_id"->bson),"","mod")(func)}
      val r=call(action,req)
      status(r) must equalTo(OK)
      contentAsString(r) must equalTo("call function")

      there was one(f.sensorDaoMock).countByType()
      there was one(f.typeSensorDaoMock).findAll(org.mockito.Matchers.eq(Json.obj("_id"->bson,"modele"->Json.obj("$regex"->".*mod.*"))), any[JsObject])(any[ExecutionContext])
      there was one(f.typeMesureDaoMock).findAll(any[JsObject], any[JsObject])(any[ExecutionContext])
      there was one(f.especeDaoMock).findAll(any[JsObject],any[JsObject])(any[ExecutionContext])
      there was one(f.typeSensorDaoMock).findAllType(org.mockito.Matchers.eq(BSONDocument("_id"->bson)))
      there was one(f.sensorDaoMock).countUsedSensors(org.mockito.Matchers.eq(List()))
      there was one(func).apply(any[List[TypeSensor]],any[List[TypeMesure]],any[List[Espece]],any[List[BSONDocument]],any[List[(BSONObjectID,Int)]],any[List[BSONDocument]])
    }

    "Call function for print result with type and modele filter" in new WithApplication {
      val f = fixture
      val func=mock[(List[TypeSensor],List[TypeMesure],List[Espece],List[BSONDocument],List[(BSONObjectID,Int)],List[BSONDocument])=>Result]

      f.sensorDaoMock.countByType() returns future {Stream[BSONDocument]()}
      f.typeSensorDaoMock.findAll(org.mockito.Matchers.eq(Json.obj("_id"->bson,"nomType"->"typ","modele"->Json.obj("$regex"->".*mod.*"))), any[JsObject])(any[ExecutionContext]) returns future {List[TypeSensor]()}
      f.typeMesureDaoMock.findAll(any[JsObject], any[JsObject])(any[ExecutionContext]) returns future {List[TypeMesure]()}
      f.especeDaoMock.findAll(any[JsObject],any[JsObject])(any[ExecutionContext]) returns future{List[Espece]()}
      f.typeSensorDaoMock.findAllType(org.mockito.Matchers.eq(BSONDocument("_id"->bson))) returns future {Stream[BSONDocument]()}
      f.sensorDaoMock.countUsedSensors(org.mockito.Matchers.eq(List())) returns future{List()}
      func.apply(any[List[TypeSensor]],any[List[TypeMesure]],any[List[Espece]],any[List[BSONDocument]],any[List[(BSONObjectID,Int)]],any[List[BSONDocument]]) returns Results.Ok("call function")

      val req=FakeRequest(GET, "/inventary/sensors").withSession("user" -> """{"login":"test"}""")
      val action = Action.async{f.controller.getInventaryTypeSensor(Json.obj("_id"->bson),"typ","mod")(func)}
      val r=call(action,req)
      status(r) must equalTo(OK)
      contentAsString(r) must equalTo("call function")

      there was one(f.sensorDaoMock).countByType()
      there was one(f.typeSensorDaoMock).findAll(org.mockito.Matchers.eq(Json.obj("_id"->bson,"nomType"->"typ","modele"->Json.obj("$regex"->".*mod.*"))), any[JsObject])(any[ExecutionContext])
      there was one(f.typeMesureDaoMock).findAll(any[JsObject], any[JsObject])(any[ExecutionContext])
      there was one(f.especeDaoMock).findAll(any[JsObject],any[JsObject])(any[ExecutionContext])
      there was one(f.typeSensorDaoMock).findAllType(org.mockito.Matchers.eq(BSONDocument("_id"->bson)))
      there was one(f.sensorDaoMock).countUsedSensors(org.mockito.Matchers.eq(List()))
      there was one(func).apply(any[List[TypeSensor]],any[List[TypeMesure]],any[List[Espece]],any[List[BSONDocument]],any[List[(BSONObjectID,Int)]],any[List[BSONDocument]])
    }

    "send 500 internal error if mongoDB error when find type sensor" in new WithApplication{
      val f=fixture
      val futureMock=mock[Future[List[TypeSensor]]]
      val throwable=mock[Throwable]
      val func=mock[(List[TypeSensor],List[TypeMesure],List[Espece],List[BSONDocument],List[(BSONObjectID,Int)],List[BSONDocument])=>Result]

      f.sensorDaoMock.countByType() returns future{Stream[BSONDocument]()}
      f.typeSensorDaoMock.findAll(any[JsObject], any[JsObject])(any[ExecutionContext]) returns futureMock
      futureMock.flatMap(any[List[TypeSensor]=>Future[List[TypeSensor]]])(any[ExecutionContext]) returns futureMock
      futureMock.recover(any[PartialFunction[Throwable,Result]])(any[ExecutionContext]) answers (value => future{value.asInstanceOf[PartialFunction[Throwable,Result]](throwable)})
      f.typeMesureDaoMock.findAll(any[JsObject], any[JsObject])(any[ExecutionContext]) returns future{List[TypeMesure]()}
      f.typeSensorDaoMock.findAllType(any[BSONDocument]) returns future{Stream[BSONDocument]()}

      val req=FakeRequest(GET, "/inventary/sensors").withSession("user" -> """{"login":"test"}""")
      val action = Action.async{f.controller.getInventaryTypeSensor(Json.obj(),"","")(func)}
      val r=call(action,req)

      status(r) must equalTo(INTERNAL_SERVER_ERROR)

      there was one(f.sensorDaoMock).countByType()
      there was one(f.typeSensorDaoMock).findAll(any[JsObject], any[JsObject])(any[ExecutionContext])
      there was one(f.typeMesureDaoMock).findAll(any[JsObject], any[JsObject])(any[ExecutionContext])
      there was one(f.typeSensorDaoMock).findAllType(any[BSONDocument])
      there was one(futureMock).flatMap(any[List[TypeSensor]=>Future[List[TypeSensor]]])(any[ExecutionContext])
      there was one(futureMock).recover(any[PartialFunction[Throwable,Result]])(any[ExecutionContext])
      there was no(func).apply(any[List[TypeSensor]],any[List[TypeMesure]],any[List[Espece]],any[List[BSONDocument]],any[List[(BSONObjectID,Int)]],any[List[BSONDocument]])
    }

    "send 500 internal error if mongoDB error when find type mesure" in new WithApplication{
      val f=fixture
      val futureMock=mock[Future[List[TypeMesure]]]
      val throwable=mock[Throwable]
      val func=mock[(List[TypeSensor],List[TypeMesure],List[Espece],List[BSONDocument],List[(BSONObjectID,Int)],List[BSONDocument])=>Result]

      f.sensorDaoMock.countByType() returns future{Stream[BSONDocument]()}
      f.typeSensorDaoMock.findAll(any[JsObject], any[JsObject])(any[ExecutionContext]) returns future{List[TypeSensor]()}
      f.typeMesureDaoMock.findAll(any[JsObject], any[JsObject])(any[ExecutionContext]) returns futureMock
      f.especeDaoMock.findAll(any[JsObject],any[JsObject])(any[ExecutionContext]) returns future{List[Espece]()}
      futureMock.flatMap(any[List[TypeMesure]=>Future[List[TypeMesure]]])(any[ExecutionContext]) returns futureMock
      futureMock.recover(any[PartialFunction[Throwable,Result]])(any[ExecutionContext]) answers (value => future{value.asInstanceOf[PartialFunction[Throwable,Result]](throwable)})
      f.typeSensorDaoMock.findAllType(any[BSONDocument]) returns future{Stream[BSONDocument]()}

      val req=FakeRequest(GET, "/inventary/sensors").withSession("user" -> """{"login":"test"}""")
      val action = Action.async{f.controller.getInventaryTypeSensor(Json.obj(),"","")(func)}
      val r=call(action,req)

      status(r) must equalTo(INTERNAL_SERVER_ERROR)

      there was one(f.sensorDaoMock).countByType()
      there was one(f.typeSensorDaoMock).findAll(any[JsObject], any[JsObject])(any[ExecutionContext])
      there was one(f.typeMesureDaoMock).findAll(any[JsObject], any[JsObject])(any[ExecutionContext])
      there was one(f.especeDaoMock).findAll(any[JsObject],any[JsObject])(any[ExecutionContext])
      there was one(f.typeSensorDaoMock).findAllType(any[BSONDocument])
      there was one(futureMock).flatMap(any[List[TypeMesure]=>Future[List[TypeMesure]]])(any[ExecutionContext])
      there was one(futureMock).recover(any[PartialFunction[Throwable,Result]])(any[ExecutionContext])
      there was no(func).apply(any[List[TypeSensor]],any[List[TypeMesure]],any[List[Espece]],any[List[BSONDocument]],any[List[(BSONObjectID,Int)]],any[List[BSONDocument]])
    }

    "send 500 internal error if mongoDB error when find stock" in new WithApplication{
      val f=fixture
      val futureMock=mock[Future[Stream[BSONDocument]]]
      val throwable=mock[Throwable]
      val func=mock[(List[TypeSensor],List[TypeMesure],List[Espece],List[BSONDocument],List[(BSONObjectID,Int)],List[BSONDocument])=>Result]

      f.sensorDaoMock.countByType() returns futureMock
      futureMock.flatMap(any[Stream[BSONDocument]=>Future[Stream[BSONDocument]]])(any[ExecutionContext]) returns futureMock
      futureMock.recover(any[PartialFunction[Throwable,Result]])(any[ExecutionContext]) answers (value => future{value.asInstanceOf[PartialFunction[Throwable,Result]](throwable)})
      f.typeSensorDaoMock.findAll(any[JsObject], any[JsObject])(any[ExecutionContext]) returns future{List[TypeSensor]()}
      f.typeMesureDaoMock.findAll(any[JsObject], any[JsObject])(any[ExecutionContext]) returns future{List[TypeMesure]()}
      f.especeDaoMock.findAll(any[JsObject],any[JsObject])(any[ExecutionContext]) returns future{List[Espece]()}
      f.typeSensorDaoMock.findAllType(any[BSONDocument]) returns future{Stream[BSONDocument]()}

      val req=FakeRequest(GET, "/inventary/sensors").withSession("user" -> """{"login":"test"}""")
      val action = Action.async{f.controller.getInventaryTypeSensor(Json.obj(),"","")(func)}
      val r=call(action,req)

      status(r) must equalTo(INTERNAL_SERVER_ERROR)

      there was one(f.sensorDaoMock).countByType()
      there was one(f.typeSensorDaoMock).findAll(any[JsObject], any[JsObject])(any[ExecutionContext])
      there was one(f.typeMesureDaoMock).findAll(any[JsObject], any[JsObject])(any[ExecutionContext])
      there was one(f.especeDaoMock).findAll(any[JsObject],any[JsObject])(any[ExecutionContext])
      there was one(f.typeSensorDaoMock).findAllType(any[BSONDocument])
      there was one(futureMock).flatMap(any[Stream[BSONDocument]=>Future[Stream[BSONDocument]]])(any[ExecutionContext])
      there was one(futureMock).recover(any[PartialFunction[Throwable,Result]])(any[ExecutionContext])
      there was no(func).apply(any[List[TypeSensor]],any[List[TypeMesure]],any[List[Espece]],any[List[BSONDocument]],any[List[(BSONObjectID,Int)]],any[List[BSONDocument]])
    }

    "send 500 internal error if mongoDB error when find type name" in new WithApplication{
      val f=fixture
      val futureMock=mock[Future[Stream[BSONDocument]]]
      val throwable=mock[Throwable]
      val func=mock[(List[TypeSensor],List[TypeMesure],List[Espece],List[BSONDocument],List[(BSONObjectID,Int)],List[BSONDocument])=>Result]

      f.sensorDaoMock.countByType() returns future{Stream[BSONDocument]()}
      f.typeSensorDaoMock.findAll(any[JsObject], any[JsObject])(any[ExecutionContext]) returns future{List[TypeSensor]()}
      f.typeMesureDaoMock.findAll(any[JsObject], any[JsObject])(any[ExecutionContext]) returns future{List[TypeMesure]()}
      f.especeDaoMock.findAll(any[JsObject],any[JsObject])(any[ExecutionContext]) returns future{List[Espece]()}
      f.typeSensorDaoMock.findAllType(any[BSONDocument]) returns futureMock
      futureMock.flatMap(any[Stream[BSONDocument]=>Future[Stream[BSONDocument]]])(any[ExecutionContext]) returns futureMock
      futureMock.recover(any[PartialFunction[Throwable,Result]])(any[ExecutionContext]) answers (value => future{value.asInstanceOf[PartialFunction[Throwable,Result]](throwable)})

      val req=FakeRequest(GET, "/inventary/sensors").withSession("user" -> """{"login":"test"}""")
      val action = Action.async{f.controller.getInventaryTypeSensor(Json.obj(),"","")(func)}
      val r=call(action,req)

      status(r) must equalTo(INTERNAL_SERVER_ERROR)

      there was one(f.sensorDaoMock).countByType()
      there was one(f.typeSensorDaoMock).findAll(any[JsObject], any[JsObject])(any[ExecutionContext])
      there was one(f.typeMesureDaoMock).findAll(any[JsObject], any[JsObject])(any[ExecutionContext])
      there was one(f.especeDaoMock).findAll(any[JsObject],any[JsObject])(any[ExecutionContext])
      there was one(f.typeSensorDaoMock).findAllType(any[BSONDocument])
      there was one(futureMock).flatMap(any[Stream[BSONDocument]=>Future[Stream[BSONDocument]]])(any[ExecutionContext])
      there was one(futureMock).recover(any[PartialFunction[Throwable,Result]])(any[ExecutionContext])
      there was no(func).apply(any[List[TypeSensor]],any[List[TypeMesure]],any[List[Espece]],any[List[BSONDocument]],any[List[(BSONObjectID,Int)]],any[List[BSONDocument]])
    }
  }

  "When method printForm is called, TypeSensorManager" should{
    "print an empty form" in new WithApplication{
      val f=fixture

      f.listDataEmptyStream

      val req=FakeRequest(GET, "/inventary/sensors/type").withSession("user" -> """{"login":"test"}""")
      val action = Action.async{implicit request=>f.controller.printForm(Results.Ok,TypeSensorManager.form,mock[Call],req.session)}
      val r=call(action,req)


      status(r) must equalTo(OK)
      contentType(r) must beSome.which(_ == "text/html")
      val content = contentAsString(r)
      content must contain("<title>Inventaire des capteurs</title>")
      content must contain("<input id=\"model\" name=\"model\" class=\"form-control\" list=\"list_modele\" type=\"text\" autofocus=\"autofocus\" autocomplete=\"off\" value=\"\"/>")
      content must contain("<input id=\"types\" name=\"types\" class=\"form-control\" list=\"list_type\" type=\"text\" autocomplete=\"off\" value=\"\"/>")
      content must contain("<input id=\"fabricant\" name=\"fabricant\" class=\"form-control\" list=\"list_fabricant\" type=\"text\" autocomplete=\"off\" value=\"\"/>")
      content must contain("<input id=\"nbSignaux\" name=\"nbSignaux\" class=\"form-control\" type=\"number\" autocomplete=\"off\" value=\"\"/>")

      f.verifyCallListData
    }
  }

  "When user submit a form, TypeSensorManager" should {
    "send bad_request if the type of sensor not contain specie" in new WithApplication {
      val route = mock[Call]
      val function = mock[(TypeSensorForm, List[BSONObjectID]) => Future[Result]]
      val function2 = mock[TypeSensorForm => JsObject]
      val f = fixture

      val r = f.controller.submitForm("error", route)(function2)(function)(FakeRequest(POST, "url").withSession("user" -> """{"login":"test"}""").withSession("typeEspece"->"""[]"""))

      status(r) must equalTo(BAD_REQUEST)
      contentType(r) must beSome.which(_ == "text/html")
      val content = contentAsString(r)
      content must contain("<title>Inventaire des capteurs</title>")
      content must contain("<div class=\"alert alert-danger\" role=\"alert\">Un type de capteur doit contenir au moins une espèce</div>")
    }

    "Insert the type of sensor" in new WithApplication{
      val route = mock[Call]
      val function = mock[(TypeSensorForm, List[BSONObjectID]) => Future[Result]]
      val function2 = mock[TypeSensorForm => JsObject]
      val controller=mock[TypeSensorManagerTest]

      org.mockito.Mockito.doCallRealMethod().when(controller).submitForm(any[String],any[Call])(any[TypeSensorForm => JsObject])(any[(TypeSensorForm, List[BSONObjectID]) => Future[Result]])(any[Request[AnyContent]])
      controller.getEspeceOnSession(any[Request[AnyContent]]) returns List(EspeceForm("esp","mesure","unite",2.3f,3.4f))
      controller.actionWhenFormValid(anyString,any[Call],any[Option[TypeSensorForm]],any[List[EspeceForm]],any[TypeSensorForm => JsObject],any[(TypeSensorForm, List[BSONObjectID]) => Future[Result]])(any[Request[AnyContent]]) returns future{Results.Ok("ok")}

      val r = controller.submitForm("error", route)(function2)(function)(FakeRequest(POST, "url").withSession("user" -> """{"login":"test"}"""))

      status(r) must equalTo(OK)
      contentAsString(r) must equalTo("ok")

      there was one(controller).getEspeceOnSession(any[Request[AnyContent]])
      there was one(controller).actionWhenFormValid(anyString,any[Call],any[Option[TypeSensorForm]],any[List[EspeceForm]],any[TypeSensorForm => JsObject],any[(TypeSensorForm, List[BSONObjectID]) => Future[Result]])(any[Request[AnyContent]])
    }
  }

  "When method updateWithDeleteColumn is called, TypeSensorManager" should{
    "send 500 Internal Error if mongoDB error when find sensor type" in new WithApplication{
      val futureMock = mock[Future[Option[TypeSensor]]]
      val fix=fixture
      val throwable=mock[Throwable]

      fix.typeSensorDaoMock.findOne(any[JsObject])(any[ExecutionContext]) returns futureMock
      futureMock.flatMap(any[(Option[TypeSensor])=>Future[Option[TypeSensor]]])(any[ExecutionContext]) returns futureMock
      futureMock.recover(any[PartialFunction[Throwable,Result]])(any[ExecutionContext]) answers (vals => future{vals.asInstanceOf[PartialFunction[Throwable,Result]](throwable)})

      val r = fix.controller.updateWithDeleteColumn(Json.obj(),true)

      status(r) must equalTo(INTERNAL_SERVER_ERROR)

      there was one(fix.typeSensorDaoMock).findOne(any[JsObject])(any[ExecutionContext])
      there was one(futureMock).flatMap(any[(Option[TypeSensor])=>Future[Result]])(any[ExecutionContext])
      there was one(futureMock).recover(any[PartialFunction[Throwable,Result]])(any[ExecutionContext])
    }

    "send 500 Internal Error if mongoDB error when update sensor type" in new WithApplication{
      val futureMock=mock[Future[LastError]]
      val fix=fixture
      val throwable=mock[Throwable]

      fix.typeSensorDaoMock.findOne(any[JsObject])(any[ExecutionContext]) returns future{Some(TypeSensor(bson,"type1","modele1","fab1",1, List()))}
      fix.typeSensorDaoMock.updateById(any[BSONObjectID],any[TypeSensor],any[GetLastError])(any[Writes[TypeSensor]],any[ExecutionContext]) returns futureMock
      futureMock.map(any[LastError=>LastError])(any[ExecutionContext]) returns futureMock
      futureMock.recover(any[PartialFunction[Throwable,Result]])(any[ExecutionContext]) answers (vals => future{vals.asInstanceOf[PartialFunction[Throwable,Result]](throwable)})

      val r = fix.controller.updateWithDeleteColumn(Json.obj(),true)

      status(r) must equalTo(INTERNAL_SERVER_ERROR)

      there was one(fix.typeSensorDaoMock).findOne(any[JsObject])(any[ExecutionContext])
      there was one(fix.typeSensorDaoMock).updateById(any[BSONObjectID],any[TypeSensor],any[GetLastError])(any[Writes[TypeSensor]],any[ExecutionContext])
      there was one(futureMock).map(any[LastError=>LastError])(any[ExecutionContext])
      there was one(futureMock).recover(any[PartialFunction[Throwable,Results]])(any[ExecutionContext])
    }

    "send Redirect for type sensor not found" in new WithApplication{
      val fix=fixture

      fix.typeSensorDaoMock.findOne(any[JsObject])(any[ExecutionContext]) returns future{None}

      val r = fix.controller.updateWithDeleteColumn(Json.obj(),true)

      status(r) must equalTo(SEE_OTHER)
      header("Location",r) must equalTo(Some("/inventary/sensors"))

      there was one(fix.typeSensorDaoMock).findOne(any[JsObject])(any[ExecutionContext])
    }

    "send Redirect after update type sensor" in new WithApplication{
      val fix=fixture
      val lastError=mock[LastError]

      fix.typeSensorDaoMock.findOne(any[JsObject])(any[ExecutionContext]) returns future{Some(TypeSensor(bson,"type1","modele1","fab1",1, List()))}
      fix.typeSensorDaoMock.updateById(any[BSONObjectID],any[TypeSensor],any[GetLastError])(any[Writes[TypeSensor]],any[ExecutionContext]) returns future{lastError}

      val r = fix.controller.updateWithDeleteColumn(Json.obj(),true)

      status(r) must equalTo(SEE_OTHER)
      header("Location",r) must equalTo(Some("/inventary/sensors"))

      there was one(fix.typeSensorDaoMock).findOne(any[JsObject])(any[ExecutionContext])
      there was one(fix.typeSensorDaoMock).updateById(any[BSONObjectID],any[TypeSensor],any[GetLastError])(any[Writes[TypeSensor]],any[ExecutionContext])
    }
  }

  "When method actionWhenFormValid is called, TypeSensorManager" should{
    "send Bad_Request if not have sensor type information" in new WithApplication{
      val espece=List(EspeceForm("esp1","mesure1","unite1",2.3f,3.4f),EspeceForm("esp2","mesure2","unite2",4.5f,5.6f))
      val route=mock[Call]
      val function=mock[(TypeSensorForm,List[BSONObjectID])=>Future[Result]]
      val function2=mock[TypeSensorForm=>JsObject]
      val f=fixture
      val typeSensor=mock[TypeSensor]

      val r = f.controller.actionWhenFormValid("error message",route,None,espece,function2,function)(FakeRequest(POST,"url").withSession("user" -> """{"login":"test"}"""))

      status(r) must equalTo(BAD_REQUEST)
      contentType(r) must beSome.which(_ == "text/html")
      val content = contentAsString(r)
      content must contain("<div class=\"alert alert-danger\" role=\"alert\">Veuillez saisir les informations du type de capteur</div>")
    }

    "execute dedicated function" in new WithApplication{
      val espece=List(EspeceForm("esp1","mesure1","unite1",2.3f,3.4f),EspeceForm("esp2","mesure2","unite2",4.5f,5.6f))
      val formData = TypeSensorForm("mod","typ",1,"fab")
      val route=mock[Call]
      val function=mock[(TypeSensorForm,List[BSONObjectID])=>Future[Result]]
      val function2=mock[TypeSensorForm=>JsObject]
      val f=fixture
      val typeSensor=mock[TypeSensor]
      val controller=mock[TypeSensorManagerTest]

      controller.typeSensorDao returns f.typeSensorDaoMock
      function2.apply(org.mockito.Matchers.eq(formData)) returns Json.obj()
      f.typeSensorDaoMock.findAll(any[JsObject],any[JsObject])(any[ExecutionContext]) returns future{List()}
      function.apply(org.mockito.Matchers.eq(formData),any[List[BSONObjectID]]) returns future{Results.Ok("ok")}
      controller.insertEspeces(org.mockito.Matchers.eq(espece)) returns List[Future[BSONObjectID]]()
      org.mockito.Mockito.doCallRealMethod().when(controller).actionWhenFormValid(anyString,any[Call],any[Option[TypeSensorForm]],any[List[EspeceForm]],any[TypeSensorForm=>JsObject],any[(TypeSensorForm,List[BSONObjectID])=>Future[Result]])(any[Request[AnyContent]])

      val r = controller.actionWhenFormValid("error message",route,Some(formData),espece,function2,function)(FakeRequest(POST,"url").withSession("user" -> """{"login":"test"}"""))

      status(r) must equalTo(OK)
      contentAsString(r) must equalTo("ok")

      there was one(function2).apply(org.mockito.Matchers.eq(formData))
      there was one(f.typeSensorDaoMock).findAll(any[JsObject],any[JsObject])(any[ExecutionContext])
      there was one(function).apply(org.mockito.Matchers.eq(formData),any[List[BSONObjectID]])
      there was one(controller).insertEspeces(org.mockito.Matchers.eq(espece))
    }

    "send Bad_Request for existing type" in new WithApplication {
      val espece=List(EspeceForm("esp1","mesure1","unite1",2.3f,3.4f),EspeceForm("esp2","mesure2","unite2",4.5f,5.6f))
      val formData = TypeSensorForm("mod","typ",1,"fab")
      val route=mock[Call]
      val function=mock[(TypeSensorForm,List[BSONObjectID])=>Future[Result]]
      val function2=mock[TypeSensorForm=>JsObject]
      val f=fixture
      val typeSensor=mock[TypeSensor]

      function2.apply(org.mockito.Matchers.eq(formData)) returns Json.obj()
      f.typeSensorDaoMock.findAll(any[JsObject],any[JsObject])(any[ExecutionContext]) returns future{List(typeSensor)}

      val r = f.controller.actionWhenFormValid("error message",route,Some(formData),espece,function2,function)(FakeRequest(POST,"url").withSession("user" -> """{"login":"test"}"""))

      status(r) must equalTo(BAD_REQUEST)
      contentType(r) must beSome.which(_ == "text/html")
      val content = contentAsString(r)
      content must contain("<div class=\"alert alert-danger\" role=\"alert\">Ce type de capteur existe déjà</div>")

      there was one(f.typeSensorDaoMock).findAll(any[JsObject],any[JsObject])(any[ExecutionContext])
      there was one(function2).apply(org.mockito.Matchers.eq(formData))
    }

    "send Bad_Request for existing type but delete" in new WithApplication {
      val espece=List(EspeceForm("esp1","mesure1","unite1",2.3f,3.4f),EspeceForm("esp2","mesure2","unite2",4.5f,5.6f))
      val formData = TypeSensorForm("mod","typ",1,"fab")
      val route=mock[Call]
      val function=mock[(TypeSensorForm,List[BSONObjectID])=>Future[Result]]
      val function2=mock[TypeSensorForm=>JsObject]
      val f=fixture
      val typeSensor=mock[TypeSensor]

      typeSensor.delete returns true
      function2.apply(org.mockito.Matchers.eq(formData)) returns Json.obj()
      f.typeSensorDaoMock.findAll(any[JsObject],any[JsObject])(any[ExecutionContext]) returns future{List(typeSensor)}

      val r = f.controller.actionWhenFormValid("error message",route,Some(formData),espece,function2,function)(FakeRequest(POST,"url").withSession("user" -> """{"login":"test"}"""))

      status(r) must equalTo(BAD_REQUEST)
      contentType(r) must beSome.which(_ == "text/html")
      val content = contentAsString(r)
      content must contain("<div class=\"alert alert-danger\" role=\"alert\">error message</div>")

      there was one(f.typeSensorDaoMock).findAll(any[JsObject],any[JsObject])(any[ExecutionContext])
      there was one(function2).apply(org.mockito.Matchers.eq(formData))
    }

    "send 500 Internal error if mongoDB error when find all sensor type" in new WithApplication {
      val espece=List(EspeceForm("esp1","mesure1","unite1",2.3f,3.4f),EspeceForm("esp2","mesure2","unite2",4.5f,5.6f))
      val formData = TypeSensorForm("mod","typ",1,"fab")
      val route=mock[Call]
      val function=mock[(TypeSensorForm,List[BSONObjectID])=>Future[Result]]
      val function2=mock[TypeSensorForm=>JsObject]
      val futureMock=mock[Future[List[TypeSensor]]]
      val fix=fixture
      val throwable=mock[Throwable]

      function2.apply(org.mockito.Matchers.eq(formData)) returns Json.obj()
      fix.typeSensorDaoMock.findAll(any[JsObject],any[JsObject])(any[ExecutionContext]) returns futureMock
      futureMock.flatMap(any[List[TypeSensor]=>Future[List[TypeSensor]]])(any[ExecutionContext]) returns futureMock
      futureMock.recover(any[PartialFunction[Throwable,Result]])(any[ExecutionContext]) answers (vals => future{vals.asInstanceOf[PartialFunction[Throwable,Result]](throwable)})

      val r = fix.controller.actionWhenFormValid("error",route,Some(formData),espece,function2,function)(FakeRequest(POST,"url").withSession("user" -> """{"login":"test"}"""))

      status(r) must equalTo(INTERNAL_SERVER_ERROR)

      there was one(function2).apply(org.mockito.Matchers.eq(formData))
      there was one(fix.typeSensorDaoMock).findAll(any[JsObject],any[JsObject])(any[ExecutionContext])
      there was one(futureMock).flatMap(any[List[TypeSensor]=>Future[List[TypeSensor]]])(any[ExecutionContext])
      there was one(futureMock).recover(any[PartialFunction[Throwable,Result]])(any[ExecutionContext])
    }
  }

  "When user is on the resource /inventary/sensors/type , TypeSensorManager" should {
    "send 200 on OK with an empty form" in new WithApplication {
      val f=fixture

      f.listDataEmptyStream

      val r = f.controller.typePage.apply(FakeRequest(GET, "/inventary/sensors/type").withSession("user" -> """{"login":"test"}"""))

      status(r) must equalTo(OK)
      contentType(r) must beSome.which(_ == "text/html")
      val content = contentAsString(r)
      content must contain("<title>Inventaire des capteurs</title>")
      content must contain("<input id=\"model\" name=\"model\" class=\"form-control\" list=\"list_modele\" type=\"text\" autofocus=\"autofocus\" autocomplete=\"off\" value=\"\"/>")
      content must contain("<input id=\"types\" name=\"types\" class=\"form-control\" list=\"list_type\" type=\"text\" autocomplete=\"off\" value=\"\"/>")
      content must contain("<input id=\"fabricant\" name=\"fabricant\" class=\"form-control\" list=\"list_fabricant\" type=\"text\" autocomplete=\"off\" value=\"\"/>")
      content must contain("<input id=\"nbSignaux\" name=\"nbSignaux\" class=\"form-control\" type=\"number\" autocomplete=\"off\" value=\"\"/>")
      val s=session(r)
      s.get("typeForm") must beSome("insert")
      s.get("typeInfo") must beNone
      s.get("typeEspece") must beNone
      s.get("typeId") must beNone

      f.verifyCallListData
    }

    "send bad request if the form is submit with no error" in new WithApplication{
      val f=fixture

      f.listDataEmptyStream

      val r = f.controller.saveTypeInfo.apply(FakeRequest(POST, "/inventary/sensors/type").withSession("user" -> """{"login":"test"}"""))

      status(r) must equalTo(BAD_REQUEST)
      val content=contentAsString(r)
      content must contains("<span class=\"control-label errors\">This field is required</span>",4)

      f.verifyCallListData
    }

    "send bad request if the form is submit with not a number for the number of signal" in new WithApplication{
      val f=fixture
      val data="""{"types":"typ","model":"mod","fabricant":"fab","nbSignaux":"a"}"""

      f.listDataEmptyStream

      val r = f.controller.saveTypeInfo.apply(FakeRequest(POST, "/inventary/sensors/type").withJsonBody(Json.parse(data)).withSession("user" -> """{"login":"test"}"""))

      status(r) must equalTo(BAD_REQUEST)
      val content=contentAsString(r)
      content must contain("<span class=\"control-label errors\">Numeric value expected</span>")

      f.verifyCallListData
    }

    "send redirect if data submit are valid" in new WithApplication{
      val f=fixture
      val data="""{"types":"typ","model":"mod","fabricant":"fab","nbSignaux":"1"}"""

      val r = f.controller.saveTypeInfo.apply(FakeRequest(POST, "/inventary/sensors/type").withJsonBody(Json.parse(data)).withSession("user" -> """{"login":"test"}"""))

      status(r) must equalTo(SEE_OTHER)
      header("Location",r) must equalTo(Some("/inventary/sensors/type/species"))
      val s=session(r)
      s.get("typeInfo") must beSome("""{"model":"mod","types":"typ","nbSignaux":1,"fabricant":"fab"}""")
    }
  }

  "When user is on the resource /inventary/sensors/type/update, TypeSensorManager" should{
    "send 200 on OK with a form" in new WithApplication {
      val typeSensor=Some(TypeSensor(bson,"type","mod","fab",1, List(bson2),false))

      val fix=fixture

      fix.listDataEmptyStream

      val r = fix.controller.updateInfo(FakeRequest(GET, "/inventary/sensors/type/update").withSession("user" -> """{"login":"test"}""").withSession("typeInfo"->"""{"model":"mod","types":"type","fabricant":"fab","nbSignaux":1}"""))

      status(r) must equalTo(OK)
      contentType(r) must beSome.which(_ == "text/html")
      val content = contentAsString(r)
      content must contain("<title>Inventaire des capteurs</title>")
      content must contain("<input id=\"model\" name=\"model\" class=\"form-control\" list=\"list_modele\" type=\"text\" autofocus=\"autofocus\" autocomplete=\"off\" value=\"mod\"/>")
      content must contain("<input id=\"types\" name=\"types\" class=\"form-control\" list=\"list_type\" type=\"text\" autocomplete=\"off\" value=\"type\"/>")
      content must contain("<input id=\"fabricant\" name=\"fabricant\" class=\"form-control\" list=\"list_fabricant\" type=\"text\" autocomplete=\"off\" value=\"fab\"/>")
      content must contain("<input id=\"nbSignaux\" name=\"nbSignaux\" class=\"form-control\" type=\"number\" autocomplete=\"off\" value=\"1\"/>")

      fix.verifyCallListData
    }
  }

  "When user is on the resource /inventary/sensors/:id/update , TypeSensorManager" should {
    "send 200 on OK with a form" in new WithApplication {
      val typeSensor=Some(TypeSensor(bson,"type","mod","fab",1, List(bson2),false))

      val fix=fixture

      fix.typeSensorDaoMock.findById(any[BSONObjectID])(any[ExecutionContext]) returns future{typeSensor}
      fix.typeMesureDaoMock.findAll(any[JsObject],any[JsObject])(any[ExecutionContext]) returns future{List(TypeMesure(bson3,"tension","volt"))}
      fix.especeDaoMock.findAll(any[JsObject],any[JsObject])(any[ExecutionContext]) returns future{List(Espece(bson2,"esp",bson3,2.3f,3.4f))}
      fix.listDataEmptyStream

      val r = fix.controller.typeUpdatePage(bson.stringify).apply(FakeRequest(GET, "/inventary/sensors/"+bson.stringify+"/update").withSession("user" -> """{"login":"test"}"""))


      status(r) must equalTo(OK)
      contentType(r) must beSome.which(_ == "text/html")
      val content = contentAsString(r)
      content must contain("<title>Inventaire des capteurs</title>")
      content must contain("<input id=\"model\" name=\"model\" class=\"form-control\" list=\"list_modele\" type=\"text\" autofocus=\"autofocus\" autocomplete=\"off\" value=\"mod\"/>")
      content must contain("<input id=\"types\" name=\"types\" class=\"form-control\" list=\"list_type\" type=\"text\" autocomplete=\"off\" value=\"type\"/>")
      content must contain("<input id=\"fabricant\" name=\"fabricant\" class=\"form-control\" list=\"list_fabricant\" type=\"text\" autocomplete=\"off\" value=\"fab\"/>")
      content must contain("<input id=\"nbSignaux\" name=\"nbSignaux\" class=\"form-control\" type=\"number\" autocomplete=\"off\" value=\"1\"/>")
      val s=session(r)
      s.get("typeForm") must beSome("update")
      s.get("typeInfo") must beSome("""{"model":"mod","types":"type","nbSignaux":1,"fabricant":"fab"}""")
      s.get("typeEspece") must beSome("""[{"espece":"esp","mesure":"tension","unite":"volt","min":2.299999952316284,"max":3.4000000953674316}]""")
      s.get("typeId") must beSome(bson.stringify)

      there was one(fix.typeSensorDaoMock).findById(any[BSONObjectID])(any[ExecutionContext])
      there was one(fix.typeMesureDaoMock).findAll(any[JsObject],any[JsObject])(any[ExecutionContext])
      there was one(fix.especeDaoMock).findAll(any[JsObject],any[JsObject])(any[ExecutionContext])
      fix.verifyCallListData
    }

    "send Redirect for type sensor not found" in new WithApplication {
      val fix=fixture

      fix.typeSensorDaoMock.findById(any[BSONObjectID])(any[ExecutionContext]) returns future{None}

      val r = fix.controller.typeUpdatePage(bson.stringify).apply(FakeRequest(GET, "/inventary/sensors/"+bson.stringify+"/update").withSession("user" -> """{"login":"test"}"""))

      status(r) must equalTo(SEE_OTHER)
      header("Location",r) must equalTo(Some("/inventary/sensors"))

      there was one(fix.typeSensorDaoMock).findById(any[BSONObjectID])(any[ExecutionContext])
    }
  }

  "When user is on resource /inventary/sensors/type/species, TypeSensorManager" should{
    "send an empty form for insert a new specie" in new WithApplication{
      val fix=fixture

      fix.especeDaoMock.findListEspece() returns future{Stream[BSONDocument]()}
      fix.typeMesureDaoMock.findListMesure("nom") returns future{Stream[BSONDocument]()}
      fix.typeMesureDaoMock.findListUnite("unite") returns future{Stream[BSONDocument]()}

      val r = fix.controller.addEspecePage.apply(FakeRequest(GET, "/inventary/sensors/"+bson.stringify+"/update").withSession("user" -> """{"login":"test"}"""))

      status(r) must equalTo(OK)
      contentType(r) must beSome.which(_ == "text/html")
      val content = contentAsString(r)
      content must contain("<title>Inventaire des capteurs</title>")
      content must contain("<input id=\"espece\" name=\"espece\" class=\"form-control\" list=\"list_espece\" type=\"text\" autocomplete=\"off\"/>")
      content must contain("<input type=\"text\" id=\"min\" name=\"min\" value=\"\" class=\"form-control\"/>")
      content must contain("<input type=\"text\" id=\"max\" name=\"max\" value=\"\" class=\"form-control\"/>")
      content must contain("<input id=\"mesure\" name=\"mesure\" class=\"form-control\" list=\"list_mesure\" type=\"text\" autocomplete=\"off\" value=\"\"/>")
      content must contain("<input id=\"unite\" name=\"unite\" class=\"form-control\" list=\"list_unite\" type=\"text\" autocomplete=\"off\" value=\"\"/>")

      there was one(fix.especeDaoMock).findListEspece()
      there was one(fix.typeMesureDaoMock).findListMesure("nom")
      there was one(fix.typeMesureDaoMock).findListUnite("unite")
    }

    "Send bad request if an empty form is submit" in new WithApplication{
      val fix=fixture

      fix.especeDaoMock.findListEspece() returns future{Stream[BSONDocument]()}
      fix.typeMesureDaoMock.findListMesure("nom") returns future{Stream[BSONDocument]()}
      fix.typeMesureDaoMock.findListUnite("unite") returns future{Stream[BSONDocument]()}

      val r = fix.controller.saveEspece.apply(FakeRequest(GET, "/inventary/sensors/"+bson.stringify+"/update").withSession("user" -> """{"login":"test"}"""))

      status(r) must equalTo(BAD_REQUEST)
      val content=contentAsString(r)
      content must contains("<span class=\"control-label errors\">This field is required</span>",5)

      there was one(fix.especeDaoMock).findListEspece()
      there was one(fix.typeMesureDaoMock).findListMesure("nom")
      there was one(fix.typeMesureDaoMock).findListUnite("unite")
    }

    "Send bad request if min and max signal are not a number" in new WithApplication{
      val fix=fixture
      val data="""{"espece":"esp","mesure":"tension","unite":"volt","min":"a","max":"a"}"""

      fix.especeDaoMock.findListEspece() returns future{Stream[BSONDocument]()}
      fix.typeMesureDaoMock.findListMesure("nom") returns future{Stream[BSONDocument]()}
      fix.typeMesureDaoMock.findListUnite("unite") returns future{Stream[BSONDocument]()}

      val r = fix.controller.saveEspece.apply(FakeRequest(GET, "/inventary/sensors/"+bson.stringify+"/update").withJsonBody(Json.parse(data)).withSession("user" -> """{"login":"test"}"""))

      status(r) must equalTo(BAD_REQUEST)
      val content=contentAsString(r)
      content must contains("<span class=\"control-label errors\">Real number value expected</span>",2)

      there was one(fix.especeDaoMock).findListEspece()
      there was one(fix.typeMesureDaoMock).findListMesure("nom")
      there was one(fix.typeMesureDaoMock).findListUnite("unite")
    }

    "send bad request if min signal is greater or equal than max signal" in new WithApplication{
      val fix=fixture
      val data="""{"espece":"esp","mesure":"tension","unite":"volt","min":"4","max":"0"}"""

      fix.especeDaoMock.findListEspece() returns future{Stream[BSONDocument]()}
      fix.typeMesureDaoMock.findListMesure("nom") returns future{Stream[BSONDocument]()}
      fix.typeMesureDaoMock.findListUnite("unite") returns future{Stream[BSONDocument]()}

      val r = fix.controller.saveEspece.apply(FakeRequest(GET, "/inventary/sensors/"+bson.stringify+"/update").withJsonBody(Json.parse(data)).withSession("user" -> """{"login":"test"}"""))

      status(r) must equalTo(BAD_REQUEST)
      val content=contentAsString(r)
      content must contain("<div class=\"alert alert-danger\" role=\"alert\">Le signal minimum doit être inférieur au signal maximun</div>")

      there was one(fix.especeDaoMock).findListEspece()
      there was one(fix.typeMesureDaoMock).findListMesure("nom")
      there was one(fix.typeMesureDaoMock).findListUnite("unite")
    }

    "send redirect if the form is submit without errors" in new WithApplication{
      val fix=fixture
      val data="""{"espece":"esp","mesure":"tension","unite":"volt","min":"0","max":"4"}"""

      val r = fix.controller.saveEspece.apply(FakeRequest(GET, "/inventary/sensors/"+bson.stringify+"/update").withJsonBody(Json.parse(data)).withSession("user" -> """{"login":"test"}"""))

      status(r) must equalTo(SEE_OTHER)
      header("Location",r) must beSome("/inventary/sensors/type/validate")
      val s=session(r)
      s.get("typeEspece") must beSome("""[{"espece":"esp","mesure":"tension","unite":"volt","min":0.0,"max":4.0}]""")
    }
  }

  "When user is on resource /inventary/sensors/type/validate, TypeSensorManager" should{
    "send the resume of the sensor type" in new WithApplication{
      val fix=fixture
      val espece="""[{"espece":"esp","mesure":"tension","unite":"volt","min":0.0,"max":4.0}]"""
      val info="""{"model":"mod","types":"type","nbSignaux":1,"fabricant":"fab"}"""

      val r = fix.controller.validationPage.apply(FakeRequest(GET, "/inventary/sensors/"+bson.stringify+"/validate").withSession("user" -> """{"login":"test"}""").withSession("typeInfo"->info).withSession("typeEspece"->espece))

      status(r) must equalTo(OK)
      val content=contentAsString(r)
      content must contain("<h4>type / mod</h4>")
      content must contain("<span class=\"bold\">Fabricant</span> : fab")
      content must contain("<span class=\"bold\">Nombre de signaux</span> : 1")
      content must contain("<td>esp</td>")
      content must contain("<td>tension</td>")
      content must contain("<td>0.0 volt</td>")
      content must contain("<td>4.0 volt</td>")
    }

    "send redirect if update a sensor type but not have the sensor type id" in new WithApplication{
      val fix=fixture
      val espece="""[{"espece":"esp","mesure":"tension","unite":"volt","min":0.0,"max":4.0}]"""
      val info="""{"model":"mod","types":"type","nbSignaux":1,"fabricant":"fab"}"""

      val r = fix.controller.validate.apply(FakeRequest(POST, "/inventary/sensors/"+bson.stringify+"/validate").withSession("user" -> """{"login":"test"}""").withSession("typeInfo"->info).withSession("typeEspece"->espece).withSession("typeForm"->"update"))

      status(r) must equalTo(SEE_OTHER)
      header("Location",r) must beSome("/inventary/sensors")
    }

    "execute function for update" in new WithApplication{
      val espece="""[{"espece":"esp","mesure":"tension","unite":"volt","min":0.0,"max":4.0}]"""
      val info="""{"model":"mod","types":"type","nbSignaux":1,"fabricant":"fab"}"""
      val controller=mock[TypeSensorManagerTest]

      org.mockito.Mockito.doCallRealMethod().when(controller).validate
      controller.typeUpdate(org.mockito.Matchers.eq(bson.stringify))(any[Request[AnyContent]]) returns future{Results.Ok("ok")}

      val r = controller.validate.apply(FakeRequest(POST, "/inventary/sensors/"+bson.stringify+"/validate").withSession("user" -> """{"login":"test"}""").withSession("typeInfo"->info).withSession("typeEspece"->espece).withSession("typeForm"->"update").withSession("typeId"->bson.stringify))

      status(r) must equalTo(OK)
      contentAsString(r) must equalTo("ok")

      there was one(controller).typeUpdate(org.mockito.Matchers.eq(bson.stringify))(any[Request[AnyContent]])
    }

    "execute function for update" in new WithApplication{
      val espece="""[{"espece":"esp","mesure":"tension","unite":"volt","min":0.0,"max":4.0}]"""
      val info="""{"model":"mod","types":"type","nbSignaux":1,"fabricant":"fab"}"""
      val controller=mock[TypeSensorManagerTest]

      org.mockito.Mockito.doCallRealMethod().when(controller).validate
      controller.typeInsert(any[Request[AnyContent]]) returns future{Results.Ok("ok")}

      val r = controller.validate.apply(FakeRequest(POST, "/inventary/sensors/"+bson.stringify+"/validate").withSession("user" -> """{"login":"test"}""").withSession("typeInfo"->info).withSession("typeEspece"->espece).withSession("typeForm"->"insert"))

      status(r) must equalTo(OK)
      contentAsString(r) must equalTo("ok")

      there was one(controller).typeInsert(any[Request[AnyContent]])
    }
  }

  "When method typeInsert is called, TypeSensorManager" should{
    "create a query for verify if the sensor type exist and insert the sensor type" in new WithApplication{
      val fix=fixture
      val controller=mock[TypeSensorManagerTest]
      val error="Ce type de capteur existe déjà <input type=\"submit\" class=\"btn btn-danger\" name=\"send\" value=\"Réactiver\"/> <input type=\"submit\" class=\"btn btn-danger\" name=\"send\" value=\"Ignorer\"/>"
      val typeSensor=TypeSensorForm("mod","typ",1,"fab")

      controller.typeSensorDao returns fix.typeSensorDaoMock
      controller.request2lang(any[Request[AnyContent]]) returns Lang("en")
      controller.Redirect(any[Call]) returns Results.Redirect(routes.TypeSensorManager.inventary())
      org.mockito.Mockito.doCallRealMethod().when(controller).typeInsert(any[Request[AnyContent]])
      controller.submitForm(org.mockito.Matchers.eq(error),any[Call])(any[TypeSensorForm=>JsObject])(any[(TypeSensorForm,List[BSONObjectID])=>Future[Result]])(any[Request[AnyContent]]) answers {(param,mock)=> param match{
        case Array(_,_,f1,f2,_) =>{
          f1.asInstanceOf[TypeSensorForm=>JsObject].apply(typeSensor) must equalTo(Json.obj("modele" -> "mod", "fabricant" -> "fab"))
          f2.asInstanceOf[(TypeSensorForm,List[BSONObjectID])=>Future[Result]].apply(typeSensor,List(bson2))
        }
      }}
      fix.typeSensorDaoMock.insert(any[TypeSensor],any[GetLastError])(any[ExecutionContext]) returns future{mock[LastError]}

      val req=FakeRequest(POST, "/inventary/sensors/"+bson.stringify+"/validate").withSession("user" -> """{"login":"test"}""")
      val r=controller.typeInsert(req)

      status(r) must equalTo(SEE_OTHER)
      header("Location",r) must beSome("/inventary/sensors")

      there was one(controller).Redirect(any[Call])
      there was one(controller).submitForm(org.mockito.Matchers.eq(error),any[Call])(any[TypeSensorForm=>JsObject])(any[(TypeSensorForm,List[BSONObjectID])=>Future[Result]])(any[Request[AnyContent]])
      there was one(fix.typeSensorDaoMock).insert(any[TypeSensor],any[GetLastError])(any[ExecutionContext])
    }

    "create a query for verify if the sensor type exist and reactivat the sensor type" in new WithApplication{
      val fix=fixture
      val controller=mock[TypeSensorManagerTest]
      val error="Ce type de capteur existe déjà <input type=\"submit\" class=\"btn btn-danger\" name=\"send\" value=\"Réactiver\"/> <input type=\"submit\" class=\"btn btn-danger\" name=\"send\" value=\"Ignorer\"/>"
      val typeSensor=TypeSensorForm("mod","typ",1,"fab",Some("reactiver"))

      controller.typeSensorDao returns fix.typeSensorDaoMock
      controller.request2lang(any[Request[AnyContent]]) returns Lang("en")
      org.mockito.Mockito.doCallRealMethod().when(controller).typeInsert(any[Request[AnyContent]])
      controller.submitForm(org.mockito.Matchers.eq(error),any[Call])(any[TypeSensorForm=>JsObject])(any[(TypeSensorForm,List[BSONObjectID])=>Future[Result]])(any[Request[AnyContent]]) answers {(param,mock)=> param match{
        case Array(_,_,f1,f2,_) =>{
          f1.asInstanceOf[TypeSensorForm=>JsObject].apply(typeSensor) must equalTo(Json.obj("modele" -> "mod", "fabricant" -> "fab"))
          f2.asInstanceOf[(TypeSensorForm,List[BSONObjectID])=>Future[Result]].apply(typeSensor,List(bson2))
        }
      }}
      controller.updateWithDeleteColumn(org.mockito.Matchers.eq(Json.obj("modele"->"mod","fabricant"->"fab")),org.mockito.Matchers.eq(false)) returns future{Results.Ok("ok")}

      val req=FakeRequest(POST, "/inventary/sensors/"+bson.stringify+"/validate").withSession("user" -> """{"login":"test"}""")
      val r=controller.typeInsert(req)

      status(r) must equalTo(OK)
      contentAsString(r) must equalTo("ok")

      there was one(controller).submitForm(org.mockito.Matchers.eq(error),any[Call])(any[TypeSensorForm=>JsObject])(any[(TypeSensorForm,List[BSONObjectID])=>Future[Result]])(any[Request[AnyContent]])
      there was no(fix.typeSensorDaoMock).insert(any[TypeSensor],any[GetLastError])(any[ExecutionContext])
      there was one(controller).updateWithDeleteColumn(org.mockito.Matchers.eq(Json.obj("modele"->"mod","fabricant"->"fab")),org.mockito.Matchers.eq(false))
    }
  }

  "When method typeUpdate is called, TypeSensorManager" should{
    "create a query for verify if the sensor type exist and update the sensor type" in new WithApplication{
      val fix=fixture
      val controller=mock[TypeSensorManagerTest]
      val error="Ce type de capteur existe déjà <input type=\"submit\" class=\"btn btn-danger\" name=\"send\" value=\"Ignorer\"/>"
      val typeSensor=TypeSensorForm("mod","typ",1,"fab")

      controller.typeSensorDao returns fix.typeSensorDaoMock
      controller.request2lang(any[Request[AnyContent]]) returns Lang("en")
      controller.Redirect(any[Call]) returns Results.Redirect(routes.TypeSensorManager.inventary())
      org.mockito.Mockito.doCallRealMethod().when(controller).typeUpdate(anyString)(any[Request[AnyContent]])
      controller.submitForm(anyString,any[Call])(any[TypeSensorForm=>JsObject])(any[(TypeSensorForm,List[BSONObjectID])=>Future[Result]])(any[Request[AnyContent]]) answers {(param,mock)=> param match{
        case Array(_,_,f1,f2,_) =>{
          f1.asInstanceOf[TypeSensorForm=>JsObject].apply(typeSensor) must equalTo(Json.obj("_id"->Json.obj("$ne"->bson),"modele" -> "mod", "fabricant" -> "fab"))
          f2.asInstanceOf[(TypeSensorForm,List[BSONObjectID])=>Future[Result]].apply(typeSensor,List(bson2))
        }
      }}
      fix.typeSensorDaoMock.updateById(org.mockito.Matchers.eq(bson),any[TypeSensor],any[GetLastError])(any[Writes[TypeSensor]],any[ExecutionContext]) returns future{mock[LastError]}

      val req=FakeRequest(POST, "/inventary/sensors/"+bson.stringify+"/validate").withSession("user" -> """{"login":"test"}""")
      val r=controller.typeUpdate(bson.stringify)(req)

      status(r) must equalTo(SEE_OTHER)
      header("Location",r) must beSome("/inventary/sensors")

      there was one(controller).Redirect(any[Call])
      there was one(controller).submitForm(org.mockito.Matchers.eq(error),any[Call])(any[TypeSensorForm=>JsObject])(any[(TypeSensorForm,List[BSONObjectID])=>Future[Result]])(any[Request[AnyContent]])
      there was one(fix.typeSensorDaoMock).updateById(org.mockito.Matchers.eq(bson),any[TypeSensor],any[GetLastError])(any[Writes[TypeSensor]],any[ExecutionContext])
    }

    "create a query for verify if the sensor type exist and print the resume of the sensor type" in new WithApplication{
      val fix=fixture
      val controller=mock[TypeSensorManagerTest]
      val error="Ce type de capteur existe déjà <input type=\"submit\" class=\"btn btn-danger\" name=\"send\" value=\"Ignorer\"/>"
      val typeSensor=TypeSensorForm("mod","typ",1,"fab",Some("reactiver"))

      controller.typeSensorDao returns fix.typeSensorDaoMock
      controller.request2lang(any[Request[AnyContent]]) returns Lang("en")
      controller.getTypeOnSession(any[Request[AnyContent]]) returns Some(typeSensor)
      controller.getEspeceOnSession(any[Request[AnyContent]]) returns List[EspeceForm](EspeceForm("esp","tension","volt",0f,1f))
      org.mockito.Mockito.doCallRealMethod().when(controller).typeUpdate(org.mockito.Matchers.eq(bson.stringify))(any[Request[AnyContent]])
      controller.submitForm(anyString,any[Call])(any[TypeSensorForm=>JsObject])(any[(TypeSensorForm,List[BSONObjectID])=>Future[Result]])(any[Request[AnyContent]]) answers {(param,mock)=> param match{
        case Array(_,_,f1,f2,_) =>{
          f1.asInstanceOf[TypeSensorForm=>JsObject].apply(typeSensor) must equalTo(Json.obj("_id"->Json.obj("$ne"->bson),"modele" -> "mod", "fabricant" -> "fab"))
          f2.asInstanceOf[(TypeSensorForm,List[BSONObjectID])=>Future[Result]].apply(typeSensor,List(bson2))
        }
      }}

      val req=FakeRequest(POST, "/inventary/sensors/"+bson.stringify+"/validate").withSession("user" -> """{"login":"test"}""")
      val r=controller.typeUpdate(bson.stringify)(req)

      status(r) must equalTo(BAD_REQUEST)
      val content=contentAsString(r)
      content must contain("<h4>typ / mod</h4>")
      content must contain("<span class=\"bold\">Fabricant</span> : fab")
      content must contain("<span class=\"bold\">Nombre de signaux</span> : 1")
      content must contain("<td>esp</td>")
      content must contain("<td>tension</td>")
      content must contain("<td>0.0 volt</td>")
      content must contain("<td>1.0 volt</td>")

      there was one(controller).submitForm(org.mockito.Matchers.eq(error),any[Call])(any[TypeSensorForm=>JsObject])(any[(TypeSensorForm,List[BSONObjectID])=>Future[Result]])(any[Request[AnyContent]])
    }
  }

  "When method getEspeceOnSession is called, TypeSensorManager" should{
    "return an empty list if not have specie" in new WithApplication{
      val fix=fixture

      val req=FakeRequest(POST, "/inventary/sensors/"+bson.stringify+"/validate").withSession("user" -> """{"login":"test"}""")
      val r=fix.controller.getEspeceOnSession(req)

      r must equalTo(List())
    }

    "return a list with specie in session" in new WithApplication{
      val fix=fixture
      val esp=List(EspeceForm("esp","tension","volt",0f,1f))

      val req=FakeRequest(POST, "/inventary/sensors/"+bson.stringify+"/validate").withSession("user" -> """{"login":"test"}""").withSession("typeEspece"->"""[{"espece":"esp","mesure":"tension","unite":"volt","min":0.0,"max":1.0}]""")
      val r=fix.controller.getEspeceOnSession(req)

      r must equalTo(esp)
    }
  }

  "When method getTypeOnSession is called, TypeSensorManager" should{
    "return None if sensor type information not found" in new WithApplication{
      val fix=fixture

      val req=FakeRequest(POST, "/inventary/sensors/"+bson.stringify+"/validate").withSession("user" -> """{"login":"test"}""")
      val r=fix.controller.getTypeOnSession(req)

      r must beNone
    }

    "return sensor type information" in new WithApplication{
      val fix=fixture
      val typ=TypeSensorForm("mod","typ",1,"fab")

      val req=FakeRequest(POST, "/inventary/sensors/"+bson.stringify+"/validate").withSession("user" -> """{"login":"test"}""").withSession("typeInfo"->"""{"model":"mod","types":"typ","nbSignaux":1,"fabricant":"fab"}""")
      val r=fix.controller.getTypeOnSession(req)

      r must beSome(typ)
    }
  }

  "When method especesToJson is called, TypeSensorManager" should{
    "transform a list of specie to Json" in new WithApplication{
      val fix=fixture
      val espece=List(
        EspeceForm("esp1","tension1","volt1",0f,1f),
        EspeceForm("esp2","tension2","volt2",2f,3f)
      )

      val jsEspece=JsArray(Seq(
        Json.obj("espece"->"esp1","mesure"->"tension1","unite"->"volt1","min"->0f,"max"->1f),
        Json.obj("espece"->"esp2","mesure"->"tension2","unite"->"volt2","min"->2f,"max"->3f)
      ))

      val r=fix.controller.especesToJson(espece)

      r must equalTo(jsEspece)
    }
  }

  "When user is on resource /inventary/sensors/type/species/:id/delete, TypeSensorManager" should{
    "delete species in the session" in new WithApplication{
      val fix=fixture

      val req=FakeRequest(POST, "/inventary/sensors/type/species/"+bson.stringify+"/delete").withSession("user" -> """{"login":"test"}""").withSession("typeEspece"->"""[{"espece":"esp","mesure":"t","unite":"v","min":0.0,"max":1.0}]""")
      val r=fix.controller.deleteEspece(0)(req)

      status(r) must equalTo(SEE_OTHER)
      header("Location",r) must beSome("/inventary/sensors/type/validate")
      val s=session(r)
      s.get("typeEspece") must beSome("""[]""")
    }


    "not crash if the index is greater than the number of species" in new WithApplication{
      val fix=fixture

      val req=FakeRequest(POST, "/inventary/sensors/type/species/"+bson.stringify+"/delete").withSession("user" -> """{"login":"test"}""").withSession("typeEspece"->"""[]""")
      val r=fix.controller.deleteEspece(2)(req)

      status(r) must equalTo(SEE_OTHER)
      header("Location",r) must beSome("/inventary/sensors/type/validate")
      val s=session(r)
      s.get("typeEspece") must beSome("""[]""")
    }

    "not crash if the index is less than 0" in new WithApplication{
      val fix=fixture

      val req=FakeRequest(POST, "/inventary/sensors/type/species/"+bson.stringify+"/delete").withSession("user" -> """{"login":"test"}""").withSession("typeEspece"->"""[]""")
      val r=fix.controller.deleteEspece(-2)(req)

      status(r) must equalTo(SEE_OTHER)
      header("Location",r) must beSome("/inventary/sensors/type/validate")
      val s=session(r)
      s.get("typeEspece") must beSome("""[]""")
    }
  }

  "When method getEspeceForm is called, TypeSensorManager" should{
    "return a list with specie" in new WithApplication{
      val fix=fixture
      val id=List(bson,bson2,bson3)

      fix.especeDaoMock.findAll(org.mockito.Matchers.eq(Json.obj("_id"->Json.obj("$in"->id))),any[JsObject])(any[ExecutionContext]) returns future{List(Espece(bson,"esp",bson2,0f,1f))}
      fix.typeMesureDaoMock.findAll(org.mockito.Matchers.eq(Json.obj("_id"->Json.obj("$in"->List(bson2)))),any[JsObject])(any[ExecutionContext]) returns future{List(TypeMesure(bson2,"tension","volt"))}

      val r=fix.controller.getEspeceForm(id)
      val res=Await.result(r,Duration.Inf)

      res must equalTo(List(EspeceForm("esp","tension","volt",0f,1f)))
    }
  }

  "When method insertEspeces is called, TypeSensorManager" should{
    "return an empty list if not have specie" in new WithApplication{
      val fix=fixture

      fix.controller.insertEspeces(List()) must equalTo(List())
    }

    "return a list of BSONObjectID associat to the list of species and insert mesure type if not exists" in new WithApplication{
      val fix=fixture
      val controller=mock[TypeSensorManagerTest]
      val espece=EspeceForm("esp","tension","volt",0f,1f)

      controller.typeMesureDao returns fix.typeMesureDaoMock
      org.mockito.Mockito.doCallRealMethod().when(controller).insertEspeces(any[List[EspeceForm]])
      fix.typeMesureDaoMock.findOne(org.mockito.Matchers.eq(Json.obj("nom"->"tension","unite"->"volt")))(any[ExecutionContext]) returns future{None}
      fix.typeMesureDaoMock.insert(any[TypeMesure],any[GetLastError])(any[ExecutionContext]) returns future{mock[LastError]}
      controller.insertEspece(org.mockito.Matchers.eq(espece),any[BSONObjectID]) returns future{bson}

      val r=controller.insertEspeces(List(espece))
      val res=Await.result(Future.sequence(r),Duration.Inf)

      res must equalTo(List(bson))

      there was one(fix.typeMesureDaoMock).findOne(org.mockito.Matchers.eq(Json.obj("nom"->"tension","unite"->"volt")))(any[ExecutionContext])
      there was one(fix.typeMesureDaoMock).insert(any[TypeMesure],any[GetLastError])(any[ExecutionContext])
      there was one(controller).insertEspece(org.mockito.Matchers.eq(espece),any[BSONObjectID])
    }

    "return a list of BSONObjectID associat to the list of species without insert mesure type if exist" in new WithApplication{
      val fix=fixture
      val controller=mock[TypeSensorManagerTest]
      val espece=EspeceForm("esp","tension","volt",0f,1f)

      controller.typeMesureDao returns fix.typeMesureDaoMock
      org.mockito.Mockito.doCallRealMethod().when(controller).insertEspeces(any[List[EspeceForm]])
      fix.typeMesureDaoMock.findOne(org.mockito.Matchers.eq(Json.obj("nom"->"tension","unite"->"volt")))(any[ExecutionContext]) returns future{Some(TypeMesure(bson2,"tension","volt"))}
      controller.insertEspece(org.mockito.Matchers.eq(espece),org.mockito.Matchers.eq(bson2)) returns future{bson}

      val r=controller.insertEspeces(List(espece))
      val res=Await.result(Future.sequence(r),Duration.Inf)

      res must equalTo(List(bson))

      there was one(fix.typeMesureDaoMock).findOne(org.mockito.Matchers.eq(Json.obj("nom"->"tension","unite"->"volt")))(any[ExecutionContext])
      there was no(fix.typeMesureDaoMock).insert(any[TypeMesure],any[GetLastError])(any[ExecutionContext])
      there was one(controller).insertEspece(org.mockito.Matchers.eq(espece),org.mockito.Matchers.eq(bson2))
    }
  }

  "When method insertEspece is called, TypeSensorManager" should{
    "insert the species if not exist" in new WithApplication{
      val fix=fixture
      val espece=EspeceForm("esp","tension","volt",0f,1f)

      fix.especeDaoMock.findOne(org.mockito.Matchers.eq(Json.obj("espece"->"esp","mesure"->bson,"min"->0f,"max"->1f)))(any[ExecutionContext]) returns future{None}
      fix.especeDaoMock.insert(any[Espece],any[GetLastError])(any[ExecutionContext]) returns future{mock[LastError]}

      val r=Await.result(fix.controller.insertEspece(espece,bson),Duration.Inf)

      val captor = capture[Espece]

      there was one(fix.especeDaoMock).findOne(org.mockito.Matchers.eq(Json.obj("espece"->"esp","mesure"->bson,"min"->0f,"max"->1f)))(any[ExecutionContext])
      there was one(fix.especeDaoMock).insert(captor,any[GetLastError])(any[ExecutionContext])

      r must equalTo(captor.value._id)
    }

    "return species _id if exist" in new WithApplication{
      val fix=fixture
      val espece=EspeceForm("esp","tension","volt",0f,1f)

      fix.especeDaoMock.findOne(org.mockito.Matchers.eq(Json.obj("espece"->"esp","mesure"->bson,"min"->0f,"max"->1f)))(any[ExecutionContext]) returns future{Some(Espece(bson2,"esp",bson,0f,1f))}

      Await.result(fix.controller.insertEspece(espece,bson),Duration.Inf) must equalTo(bson2)

      there was one(fix.especeDaoMock).findOne(org.mockito.Matchers.eq(Json.obj("espece"->"esp","mesure"->bson,"min"->0f,"max"->1f)))(any[ExecutionContext])
    }
  }

  "When user delete a type sensor, TypeSensorManager" should {

    "send 500 Internal Error if mongoDB error when find sensor" in new WithApplication{
      val futureMock=mock[Future[Option[Sensor]]]
      val fix=fixture
      val throwable=mock[Throwable]

      fix.sensorDaoMock.findOne(any[JsObject])(any[ExecutionContext]) returns futureMock
      futureMock.flatMap(any[Option[Sensor]=>Future[Option[Sensor]]])(any[ExecutionContext]) returns futureMock
      futureMock.recover(any[PartialFunction[Throwable,Result]])(any[ExecutionContext]) answers (vals => future{vals.asInstanceOf[PartialFunction[Throwable,Result]](throwable)})

      val r = fix.controller.delete(bson.stringify).apply(FakeRequest(GET, "/inventary/sensors/"+bson.stringify+"/delete").withSession("user" -> """{"login":"test"}"""))

      status(r) must equalTo(INTERNAL_SERVER_ERROR)

      there was one(fix.sensorDaoMock).findOne(any[JsObject])(any[ExecutionContext])
      there was one(futureMock).flatMap(any[Option[Sensor]=>Future[Option[Sensor]]])(any[ExecutionContext])
      there was one(futureMock).recover(any[PartialFunction[Throwable,Results]])(any[ExecutionContext])
    }

    "send BadRequest  if sensor found" in new WithApplication{
      val fix=fixture

      fix.sensorDaoMock.findOne(any[JsObject])(any[ExecutionContext]) returns future{Some(Sensor(bson3,"Id",bson,None,new Date(),None,false,None))}

      val r = fix.controller.delete(bson.stringify).apply(FakeRequest(GET, "/inventary/sensors/"+bson.stringify+"/delete").withSession("user" -> """{"login":"test"}"""))

      status(r) must equalTo(SEE_OTHER)
      header("Location",r) must equalTo(Some("/inventary/sensors"))

      there was one(fix.sensorDaoMock).findOne(any[JsObject])(any[ExecutionContext])
    }

    "send Redirect after update type sensor" in new WithApplication{
      val fix=fixture
      val lastError=mock[LastError]

      fix.typeSensorDaoMock.findOne(any[JsObject])(any[ExecutionContext]) returns future{Some(TypeSensor(bson,"type1","modele1","fab1",1, List()))}
      fix.sensorDaoMock.findOne(any[JsObject])(any[ExecutionContext]) returns future{None}
      fix.typeSensorDaoMock.updateById(any[BSONObjectID],any[TypeSensor],any[GetLastError])(any[Writes[TypeSensor]],any[ExecutionContext]) returns future{lastError}

      val r = fix.controller.delete(bson.stringify).apply(FakeRequest(GET, "/inventary/sensors/"+bson.stringify+"/delete").withSession("user" -> """{"login":"test"}"""))

      status(r) must equalTo(SEE_OTHER)
      header("Location",r) must equalTo(Some("/inventary/sensors"))

      there was one(fix.typeSensorDaoMock).findOne(any[JsObject])(any[ExecutionContext])
      there was one(fix.sensorDaoMock).findOne(any[JsObject])(any[ExecutionContext])
      there was one(fix.typeSensorDaoMock).updateById(any[BSONObjectID],any[TypeSensor],any[GetLastError])(any[Writes[TypeSensor]],any[ExecutionContext])
    }
  }

  "When verify if sensor type found, TypeSensorManager" should {
    "execute particular function if sensor type found" in new WithApplication{
      val fix=fixture
      val func1=mock[TypeSensor=>Future[Result]]
      val func2=mock[Unit=>Future[Result]]

      fix.typeSensorDaoMock.findOne(any[JsObject])(any[ExecutionContext]) returns future{Some(TypeSensor(bson,"type","mod","fab",1, List(),false))}
      func1.apply(any[TypeSensor]) returns future{Results.Ok("func found")}

      val req=FakeRequest(GET, "url")
      val action=Action.async{fix.controller.doIfTypeSensorFound(bson)(func1)(func2)}
      val r=call(action,req)

      status(r) must equalTo(OK)
      contentAsString(r) must equalTo("func found")

      there was one(fix.typeSensorDaoMock).findOne(any[JsObject])(any[ExecutionContext])
      there was one(func1).apply(any[TypeSensor])
      there was no(func2).apply(any[Unit])
    }

    "execute particular function if sensor type not found" in new WithApplication{
      val fix=fixture
      val func1=mock[TypeSensor=>Future[Result]]
      val func2=mock[Unit=>Future[Result]]

      fix.typeSensorDaoMock.findOne(any[JsObject])(any[ExecutionContext]) returns future{None}
      func2.apply(any[Unit]) returns future{Results.Ok("func not found")}

      val req=FakeRequest(GET, "url")
      val action=Action.async{fix.controller.doIfTypeSensorFound(bson)(func1)(func2)}
      val r=call(action,req)

      status(r) must equalTo(OK)
      contentAsString(r) must equalTo("func not found")

      there was one(fix.typeSensorDaoMock).findOne(any[JsObject])(any[ExecutionContext])
      there was one(func2).apply(any[Unit])
      there was no(func1).apply(any[TypeSensor])
    }

    "send Internal server error if error mongoDB" in new WithApplication{
      val fix=fixture
      val func1=mock[TypeSensor=>Future[Result]]
      val func2=mock[Unit=>Future[Result]]
      val futureMock=mock[Future[Option[TypeSensor]]]
      val throwable=mock[Throwable]

      fix.typeSensorDaoMock.findOne(any[JsObject])(any[ExecutionContext]) returns futureMock
      futureMock.flatMap(any[Option[TypeSensor]=>Future[Option[TypeSensor]]])(any[ExecutionContext]) returns futureMock
      futureMock.recover(any[PartialFunction[Throwable,Result]])(any[ExecutionContext]) answers {value => future{value.asInstanceOf[PartialFunction[Throwable,Result]](throwable)}}

      val req=FakeRequest(GET, "url")
      val action=Action.async{fix.controller.doIfTypeSensorFound(bson)(func1)(func2)}
      val r=call(action,req)

      status(r) must equalTo(INTERNAL_SERVER_ERROR)

      there was one(fix.typeSensorDaoMock).findOne(any[JsObject])(any[ExecutionContext])
      there was one(futureMock).flatMap(any[Option[TypeSensor]=>Future[Option[TypeSensor]]])(any[ExecutionContext])
      there was one(futureMock).recover(any[PartialFunction[Throwable,Future[Result]]])(any[ExecutionContext])
    }
  }

  "When method filtreStock is called, TypeSensorManager" should{
    "return true if the value is greater than 0 and filter is equal to yes" in new WithApplication{
      val fix=fixture

      fix.controller.filtreStock("yes")(1) must beTrue
    }

    "return false if the value is less or equal than 0 and filter is equal to yes" in new WithApplication{
      val fix=fixture

      fix.controller.filtreStock("yes")(0) must beFalse
    }

    "return true if the value is equal to 0 and filter is equal to no" in new WithApplication{
      val fix=fixture

      fix.controller.filtreStock("no")(0) must beTrue
    }

    "return false if the value is not equal to 0 and filter is equal to no" in new WithApplication{
      val fix=fixture

      fix.controller.filtreStock("no")(1) must beFalse
    }

    "return true if the value is greater or equal than 0 and filter is not equal to yes or no" in new WithApplication{
      val fix=fixture

      fix.controller.filtreStock("yesNo")(0) must beTrue
    }

    "return false if the value is less than 0 and filter is not equal to yes or no" in new WithApplication{
      val fix=fixture

      fix.controller.filtreStock("yesNo")(-1) must beFalse
    }
  }
}
