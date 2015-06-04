import java.util.Date

import controllers.{TypeSensorManagerLike, TypeSensorForm, TypeSensorManager}
import models._
import org.junit.runner.RunWith
import org.specs2.matcher.{MatchResult, Expectable, Matcher}
import org.specs2.mock.Mockito
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner
import play.api.data.Form
import play.api.libs.json.{Writes, Json, JsObject}
import play.api.mvc._
import play.api.test._
import play.api.test.Helpers._
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
    val controller = new TypeSensorManagerTest {
      override val typeSensorDao: TypeSensorDao = typeSensorDaoMock
      override val typeMesureDao: TypeMesureDao = typeMesureDaoMock
      override val sensorDao: SensorDao = sensorDaoMock
    }

    def listDataEmptyStream:Unit={
      typeSensorDaoMock.findListEspece(any[String]) returns future{Stream[BSONDocument]()}
      typeSensorDaoMock.findListModele(any[String]) returns future{Stream[BSONDocument]()}
      typeSensorDaoMock.findListFabricant(any[String]) returns future{Stream[BSONDocument]()}
      typeSensorDaoMock.findListType(any[String]) returns future{Stream[BSONDocument]()}
      typeMesureDaoMock.findListMesure(any[String]) returns future{Stream[BSONDocument]()}
      typeMesureDaoMock.findListUnite(any[String]) returns future{Stream[BSONDocument]()}
    }

    def verifyCallListData:Unit={
      there was one(typeSensorDaoMock).findListEspece(any[String])
      there was one(typeSensorDaoMock).findListModele(any[String])
      there was one(typeSensorDaoMock).findListFabricant(any[String])
      there was one(typeSensorDaoMock).findListType(any[String])
      there was one(typeMesureDaoMock).findListMesure(any[String])
      there was one(typeMesureDaoMock).findListUnite(any[String])
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
      f.typeSensorDaoMock.findAllType(any[BSONDocument]) returns future {
        Stream[BSONDocument]()
      }

      val r = f.controller.inventary().apply(FakeRequest(GET, "/inventary/sensors").withSession("user" -> """{"login":"test"}"""))
      status(r) must equalTo(OK)
      contentType(r) must beSome.which(_ == "text/html")
      val content = contentAsString(r)
      content must contain("<title>Inventaire des capteurs</title>")
      content must contain("<h3 style=\"text-align:center\">Aucun résultat trouvé</h3>")

      there was one(f.sensorDaoMock).countByType()
      there was one(f.typeSensorDaoMock).findAll(any[JsObject], any[JsObject])(any[ExecutionContext])
      there was one(f.typeMesureDaoMock).findAll(any[JsObject], any[JsObject])(any[ExecutionContext])
      there was one(f.typeSensorDaoMock).findAllType(any[BSONDocument])
    }

    "send 200 on OK with 1 result" in new WithApplication {
      val f = fixture

      f.sensorDaoMock.countByType() returns future {
        Stream[BSONDocument](BSONDocument("_id" -> bson, "count" -> 5))
      }

      f.typeSensorDaoMock.findAll(any[JsObject], any[JsObject])(any[ExecutionContext]) returns future {
        List[TypeSensor](TypeSensor(bson, "type1", "modele1", bson2, "fab1", 1, List[String]("esp1", "esp2")))
      }
      f.typeMesureDaoMock.findAll(any[JsObject], any[JsObject])(any[ExecutionContext]) returns future {
        List[TypeMesure](TypeMesure(bson2, "mesure1", "unite1"))
      }

      f.typeSensorDaoMock.findAllType(any[BSONDocument]) returns future {
        Stream[BSONDocument]()
      }

      val r = f.controller.inventary().apply(FakeRequest(GET, "/inventary/sensors").withSession("user" -> """{"login":"test"}"""))
      status(r) must equalTo(OK)
      contentType(r) must beSome.which(_ == "text/html")
      val content = contentAsString(r)
      content must contain("<title>Inventaire des capteurs</title>")
      content must not contain ("<h3 style=\"text-align:center\">Aucun résultat trouvé</h3>")
      content must contain("type1")
      content must contain("modele1")
      content must matchRegex("<span class=\"bold\">\\s*Fabricant\\s*</span>\\s*:\\s*fab1")
      content must matchRegex("<span class=\"bold\">\\s*Nombre de signaux\\s*</span>\\s*:\\s*1\\s*\\(\\s*mesure1\\s*\\)")
      content must matchRegex("<span class=\"bold\">\\s*Espèces\\s*</span>\\s*:\\s*esp1, esp2")
      content must matchRegex("<span class=\"bold\">\\s*Stock\\s*</span>\\s*:\\s*5")

      there was one(f.sensorDaoMock).countByType()
      there was one(f.typeSensorDaoMock).findAll(any[JsObject], any[JsObject])(any[ExecutionContext])
      there was one(f.typeMesureDaoMock).findAll(any[JsObject], any[JsObject])(any[ExecutionContext])
      there was one(f.typeSensorDaoMock).findAllType(any[BSONDocument])
    }

    "send 200 on OK with 2 results" in new WithApplication {
      val f = fixture

      f.sensorDaoMock.countByType() returns future {
        Stream[BSONDocument](BSONDocument("_id" -> bson, "count" -> 5), BSONDocument("_id" -> bson3, "count" -> 0))
      }

      f.typeSensorDaoMock.findAll(any[JsObject], any[JsObject])(any[ExecutionContext]) returns future {
        List[TypeSensor](
          TypeSensor(bson, "type1", "modele1", bson2, "fab1", 1, List[String]("esp1", "esp2")),
          TypeSensor(bson3, "type2", "modele2", bson4, "fab2", 2, List[String]("esp3", "esp4"))
        )
      }
      f.typeMesureDaoMock.findAll(any[JsObject], any[JsObject])(any[ExecutionContext]) returns future {
        List[TypeMesure](TypeMesure(bson2, "mesure1", "unite1"), TypeMesure(bson4, "mesure2", "unite2"))
      }

      f.typeSensorDaoMock.findAllType(any[BSONDocument]) returns future {
        Stream[BSONDocument]()
      }

      val r = f.controller.inventary().apply(FakeRequest(GET, "/inventary/sensors").withSession("user" -> """{"login":"test"}"""))

      status(r) must equalTo(OK)
      contentType(r) must beSome.which(_ == "text/html")
      val content = contentAsString(r)
      content must contain("<title>Inventaire des capteurs</title>")
      content must not contain ("<h3 style=\"text-align:center\">Aucun résultat trouvé</h3>")
      content must contain("type1")
      content must contain("modele1")
      content must matchRegex("<span class=\"bold\">\\s*Fabricant\\s*</span>\\s*:\\s*fab1")
      content must matchRegex("<span class=\"bold\">\\s*Nombre de signaux\\s*</span>\\s*:\\s*1\\s*\\(\\s*mesure1\\s*\\)")
      content must matchRegex("<span class=\"bold\">\\s*Espèces\\s*</span>\\s*:\\s*esp1, esp2")
      content must matchRegex("<span class=\"bold\">\\s*Stock\\s*</span>\\s*:\\s*5")

      content must contain("type2")
      content must contain("modele2")
      content must matchRegex("<span class=\"bold\">\\s*Fabricant\\s*</span>\\s*:\\s*fab2")
      content must matchRegex("<span class=\"bold\">\\s*Nombre de signaux\\s*</span>\\s*:\\s*2\\s*\\(\\s*mesure2\\s*\\)")
      content must matchRegex("<span class=\"bold\">\\s*Espèces\\s*</span>\\s*:\\s*esp3, esp4")
      content must matchRegex("<span class=\"bold\">\\s*Stock\\s*</span>\\s*:\\s*0")

      there was one(f.sensorDaoMock).countByType()
      there was one(f.typeSensorDaoMock).findAll(any[JsObject], any[JsObject])(any[ExecutionContext])
      there was one(f.typeMesureDaoMock).findAll(any[JsObject], any[JsObject])(any[ExecutionContext])
      there was one(f.typeSensorDaoMock).findAllType(any[BSONDocument])
    }

  }

  "When method getInventaryTypeSensor is called, TypeSensorManager" should{

    "Call function for print result" in new WithApplication {
      val f = fixture
      val func=mock[(List[TypeSensor],List[TypeMesure],List[BSONDocument],List[BSONDocument])=>Result]

      f.sensorDaoMock.countByType() returns future {Stream[BSONDocument]()}
      f.typeSensorDaoMock.findAll(any[JsObject], any[JsObject])(any[ExecutionContext]) returns future {List[TypeSensor]()}
      f.typeMesureDaoMock.findAll(any[JsObject], any[JsObject])(any[ExecutionContext]) returns future {List[TypeMesure]()}
      f.typeSensorDaoMock.findAllType(any[BSONDocument]) returns future {Stream[BSONDocument]()}
      func.apply(any[List[TypeSensor]],any[List[TypeMesure]],any[List[BSONDocument]],any[List[BSONDocument]]) returns Results.Ok("call function")

      val req=FakeRequest(GET, "/inventary/sensors").withSession("user" -> """{"login":"test"}""")
      val action = Action.async{f.controller.getInventaryTypeSensor(Json.obj(),"")(func)}
      val r=call(action,req)
      status(r) must equalTo(OK)
      contentAsString(r) must equalTo("call function")

      there was one(f.sensorDaoMock).countByType()
      there was one(f.typeSensorDaoMock).findAll(any[JsObject], any[JsObject])(any[ExecutionContext])
      there was one(f.typeMesureDaoMock).findAll(any[JsObject], any[JsObject])(any[ExecutionContext])
      there was one(f.typeSensorDaoMock).findAllType(any[BSONDocument])
      there was one(func).apply(any[List[TypeSensor]],any[List[TypeMesure]],any[List[BSONDocument]],any[List[BSONDocument]])
    }

    "send 500 internal error if mongoDB error when find type sensor" in new WithApplication{
      val f=fixture
      val futureMock=mock[Future[List[TypeSensor]]]
      val throwable=mock[Throwable]
      val func=mock[(List[TypeSensor],List[TypeMesure],List[BSONDocument],List[BSONDocument])=>Result]

      f.sensorDaoMock.countByType() returns future{Stream[BSONDocument]()}
      f.typeSensorDaoMock.findAll(any[JsObject], any[JsObject])(any[ExecutionContext]) returns futureMock
      futureMock.flatMap(any[List[TypeSensor]=>Future[List[TypeSensor]]])(any[ExecutionContext]) returns futureMock
      futureMock.recover(any[PartialFunction[Throwable,Result]])(any[ExecutionContext]) answers (value => future{value.asInstanceOf[PartialFunction[Throwable,Result]](throwable)})
      f.typeMesureDaoMock.findAll(any[JsObject], any[JsObject])(any[ExecutionContext]) returns future{List[TypeMesure]()}
      f.typeSensorDaoMock.findAllType(any[BSONDocument]) returns future{Stream[BSONDocument]()}

      val req=FakeRequest(GET, "/inventary/sensors").withSession("user" -> """{"login":"test"}""")
      val action = Action.async{f.controller.getInventaryTypeSensor(Json.obj(),"")(func)}
      val r=call(action,req)

      status(r) must equalTo(INTERNAL_SERVER_ERROR)

      there was one(f.sensorDaoMock).countByType()
      there was one(f.typeSensorDaoMock).findAll(any[JsObject], any[JsObject])(any[ExecutionContext])
      there was one(f.typeMesureDaoMock).findAll(any[JsObject], any[JsObject])(any[ExecutionContext])
      there was one(f.typeSensorDaoMock).findAllType(any[BSONDocument])
      there was one(futureMock).flatMap(any[List[TypeSensor]=>Future[List[TypeSensor]]])(any[ExecutionContext])
      there was one(futureMock).recover(any[PartialFunction[Throwable,Result]])(any[ExecutionContext])
      there was no(func).apply(any[List[TypeSensor]],any[List[TypeMesure]],any[List[BSONDocument]],any[List[BSONDocument]])
    }

    "send 500 internal error if mongoDB error when find type mesure" in new WithApplication{
      val f=fixture
      val futureMock=mock[Future[List[TypeMesure]]]
      val throwable=mock[Throwable]
      val func=mock[(List[TypeSensor],List[TypeMesure],List[BSONDocument],List[BSONDocument])=>Result]

      f.sensorDaoMock.countByType() returns future{Stream[BSONDocument]()}
      f.typeSensorDaoMock.findAll(any[JsObject], any[JsObject])(any[ExecutionContext]) returns future{List[TypeSensor]()}
      f.typeMesureDaoMock.findAll(any[JsObject], any[JsObject])(any[ExecutionContext]) returns futureMock
      futureMock.flatMap(any[List[TypeMesure]=>Future[List[TypeMesure]]])(any[ExecutionContext]) returns futureMock
      futureMock.recover(any[PartialFunction[Throwable,Result]])(any[ExecutionContext]) answers (value => future{value.asInstanceOf[PartialFunction[Throwable,Result]](throwable)})
      f.typeSensorDaoMock.findAllType(any[BSONDocument]) returns future{Stream[BSONDocument]()}

      val req=FakeRequest(GET, "/inventary/sensors").withSession("user" -> """{"login":"test"}""")
      val action = Action.async{f.controller.getInventaryTypeSensor(Json.obj(),"")(func)}
      val r=call(action,req)

      status(r) must equalTo(INTERNAL_SERVER_ERROR)

      there was one(f.sensorDaoMock).countByType()
      there was one(f.typeSensorDaoMock).findAll(any[JsObject], any[JsObject])(any[ExecutionContext])
      there was one(f.typeMesureDaoMock).findAll(any[JsObject], any[JsObject])(any[ExecutionContext])
      there was one(f.typeSensorDaoMock).findAllType(any[BSONDocument])
      there was one(futureMock).flatMap(any[List[TypeMesure]=>Future[List[TypeMesure]]])(any[ExecutionContext])
      there was one(futureMock).recover(any[PartialFunction[Throwable,Result]])(any[ExecutionContext])
      there was no(func).apply(any[List[TypeSensor]],any[List[TypeMesure]],any[List[BSONDocument]],any[List[BSONDocument]])
    }

    "send 500 internal error if mongoDB error when find stock" in new WithApplication{
      val f=fixture
      val futureMock=mock[Future[Stream[BSONDocument]]]
      val throwable=mock[Throwable]
      val func=mock[(List[TypeSensor],List[TypeMesure],List[BSONDocument],List[BSONDocument])=>Result]

      f.sensorDaoMock.countByType() returns futureMock
      futureMock.flatMap(any[Stream[BSONDocument]=>Future[Stream[BSONDocument]]])(any[ExecutionContext]) returns futureMock
      futureMock.recover(any[PartialFunction[Throwable,Result]])(any[ExecutionContext]) answers (value => future{value.asInstanceOf[PartialFunction[Throwable,Result]](throwable)})
      f.typeSensorDaoMock.findAll(any[JsObject], any[JsObject])(any[ExecutionContext]) returns future{List[TypeSensor]()}
      f.typeMesureDaoMock.findAll(any[JsObject], any[JsObject])(any[ExecutionContext]) returns future{List[TypeMesure]()}
      f.typeSensorDaoMock.findAllType(any[BSONDocument]) returns future{Stream[BSONDocument]()}

      val req=FakeRequest(GET, "/inventary/sensors").withSession("user" -> """{"login":"test"}""")
      val action = Action.async{f.controller.getInventaryTypeSensor(Json.obj(),"")(func)}
      val r=call(action,req)

      status(r) must equalTo(INTERNAL_SERVER_ERROR)

      there was one(f.sensorDaoMock).countByType()
      there was one(f.typeSensorDaoMock).findAll(any[JsObject], any[JsObject])(any[ExecutionContext])
      there was one(f.typeMesureDaoMock).findAll(any[JsObject], any[JsObject])(any[ExecutionContext])
      there was one(f.typeSensorDaoMock).findAllType(any[BSONDocument])
      there was one(futureMock).flatMap(any[Stream[BSONDocument]=>Future[Stream[BSONDocument]]])(any[ExecutionContext])
      there was one(futureMock).recover(any[PartialFunction[Throwable,Result]])(any[ExecutionContext])
      there was no(func).apply(any[List[TypeSensor]],any[List[TypeMesure]],any[List[BSONDocument]],any[List[BSONDocument]])
    }

    "send 500 internal error if mongoDB error when find type name" in new WithApplication{
      val f=fixture
      val futureMock=mock[Future[Stream[BSONDocument]]]
      val throwable=mock[Throwable]
      val func=mock[(List[TypeSensor],List[TypeMesure],List[BSONDocument],List[BSONDocument])=>Result]

      f.sensorDaoMock.countByType() returns future{Stream[BSONDocument]()}
      f.typeSensorDaoMock.findAll(any[JsObject], any[JsObject])(any[ExecutionContext]) returns future{List[TypeSensor]()}
      f.typeMesureDaoMock.findAll(any[JsObject], any[JsObject])(any[ExecutionContext]) returns future{List[TypeMesure]()}
      f.typeSensorDaoMock.findAllType(any[BSONDocument]) returns futureMock
      futureMock.map(any[Stream[BSONDocument]=>Stream[BSONDocument]])(any[ExecutionContext]) returns futureMock
      futureMock.recover(any[PartialFunction[Throwable,Result]])(any[ExecutionContext]) answers (value => future{value.asInstanceOf[PartialFunction[Throwable,Result]](throwable)})

      val req=FakeRequest(GET, "/inventary/sensors").withSession("user" -> """{"login":"test"}""")
      val action = Action.async{f.controller.getInventaryTypeSensor(Json.obj(),"")(func)}
      val r=call(action,req)

      status(r) must equalTo(INTERNAL_SERVER_ERROR)

      there was one(f.sensorDaoMock).countByType()
      there was one(f.typeSensorDaoMock).findAll(any[JsObject], any[JsObject])(any[ExecutionContext])
      there was one(f.typeMesureDaoMock).findAll(any[JsObject], any[JsObject])(any[ExecutionContext])
      there was one(f.typeSensorDaoMock).findAllType(any[BSONDocument])
      there was one(futureMock).map(any[Stream[BSONDocument]=>Stream[BSONDocument]])(any[ExecutionContext])
      there was one(futureMock).recover(any[PartialFunction[Throwable,Result]])(any[ExecutionContext])
      there was no(func).apply(any[List[TypeSensor]],any[List[TypeMesure]],any[List[BSONDocument]],any[List[BSONDocument]])
    }
  }

  "When method printForm is called, TypeSensorManager" should{
    "print an empty form" in new WithApplication{
      val f=fixture

      f.listDataEmptyStream

      val req=FakeRequest(GET, "/inventary/sensors/type").withSession("user" -> """{"login":"test"}""")
      val action = Action.async{implicit request=>f.controller.printForm(Results.Ok,TypeSensorManager.form,mock[Call])}
      val r=call(action,req)


      status(r) must equalTo(OK)
      contentType(r) must beSome.which(_ == "text/html")
      val content = contentAsString(r)
      content must contain("<title>Inventaire des capteurs</title>")
      content must contain("<input id=\"model\" name=\"model\" class=\"form-control\" list=\"list_modele\" type=\"text\" autocomplete=\"off\" value=\"\"/>")
      content must contain("<input id=\"types\" name=\"types\" class=\"form-control\" list=\"list_type\" type=\"text\" autocomplete=\"off\" value=\"\"/>")
      content must contain("<input id=\"espece\" name=\"espece[]\" class=\"form-control\" list=\"list_espece\" type=\"text\" autocomplete=\"off\" value=\"\"/>")
      content must contain("<input id=\"fabricant\" name=\"fabricant\" class=\"form-control\" list=\"list_fabricant\" type=\"text\" autocomplete=\"off\" value=\"\"/>")
      content must contain("<input id=\"nbSignaux\" name=\"nbSignaux\" class=\"form-control\" type=\"number\" autocomplete=\"off\" value=\"\"/>")
      content must contain("<input id=\"mesure\" name=\"mesure\" class=\"form-control\" list=\"list_mesure\" type=\"text\" autocomplete=\"off\" value=\"\"/>")
      content must contain("<input id=\"unite\" name=\"unite\" class=\"form-control\" list=\"list_unite\" type=\"text\" autocomplete=\"off\" value=\"\"/>")

      f.verifyCallListData
    }
  }

  "When method applyFunctionWithInsertSensor is called, TypeSensorManager" should {
    "insert type mesure and apply parameter function" in new WithApplication{
      val f=fixture
      val func=mock[(TypeSensorForm,List[String],TypeMesure)=>Future[Result]]
      val especes=mock[List[String]]
      val typeData=mock[TypeSensorForm]
      val lastError=mock[LastError]

      f.typeMesureDaoMock.insert(any[TypeMesure],any[GetLastError])(any[ExecutionContext]) returns future{lastError}
      func.apply(org.mockito.Matchers.eq(typeData),org.mockito.Matchers.eq(especes),any[TypeMesure]) returns future{Results.Ok("func applied")}

      val req=FakeRequest(POST, "/inventary/sensors/"+bson.stringify+"/update")
      val action=Action.async(f.controller.applyFunctionWithInsertSensor(typeData,especes,func))
      val r=call(action,req)
      Await.result(r,Duration.Inf)

      status(r) must equalTo(OK)
      contentAsString(r) must equalTo("func applied")

      there was one(f.typeMesureDaoMock).insert(any[TypeMesure],any[GetLastError])(any[ExecutionContext])
      there was one(func).apply(org.mockito.Matchers.eq(typeData),org.mockito.Matchers.eq(especes),any[TypeMesure])
    }

    "send 500 internal error after insert type mesure" in new WithApplication{
      val f=fixture
      val func=mock[(TypeSensorForm,List[String],TypeMesure)=>Future[Result]]
      val especes=mock[List[String]]
      val typeData=mock[TypeSensorForm]
      val futureMock=mock[Future[LastError]]
      val throwable=mock[Throwable]

      f.typeMesureDaoMock.insert(any[TypeMesure],any[GetLastError])(any[ExecutionContext]) returns futureMock
      futureMock.flatMap(any[LastError=>Future[LastError]])(any[ExecutionContext]) returns futureMock
      futureMock.recover(any[PartialFunction[Throwable,Result]])(any[ExecutionContext]) answers {value => future{value.asInstanceOf[PartialFunction[Throwable,Result]](throwable)}}

      val req=FakeRequest(POST, "/inventary/sensors/"+bson.stringify+"/update")
      val action=Action.async(f.controller.applyFunctionWithInsertSensor(typeData,especes,func))
      val r=call(action,req)
      Await.result(r,Duration.Inf)

      status(r) must equalTo(INTERNAL_SERVER_ERROR)

      there was one(f.typeMesureDaoMock).insert(any[TypeMesure],any[GetLastError])(any[ExecutionContext])
      there was no(func).apply(org.mockito.Matchers.eq(typeData),org.mockito.Matchers.eq(especes),any[TypeMesure])
      there was one(futureMock).flatMap(any[LastError=>Future[LastError]])(any[ExecutionContext])
      there was one(futureMock).recover(any[PartialFunction[Throwable,Result]])(any[ExecutionContext])
    }
  }

  "When user submit a form, TypeSensorManager" should {
    "send bad_request with form and empty input" in new WithApplication {
      val route = mock[Call]
      val function = mock[(TypeSensorForm, List[String], TypeMesure) => Future[Result]]
      val function2 = mock[TypeSensorForm => JsObject]
      val f = fixture

      f.listDataEmptyStream

      val r = f.controller.submitForm("error", route)(function2)(function)(FakeRequest(POST, "url").withSession("user" -> """{"login":"test"}"""))

      status(r) must equalTo(BAD_REQUEST)
      contentType(r) must beSome.which(_ == "text/html")
      val content = contentAsString(r)
      content must contain("<title>Inventaire des capteurs</title>")
      content must contains("<span class=\"control-label errors\">This field is required</span>", 7)

      f.verifyCallListData
    }

    "send bad_request with form and signal not an integer" in new WithApplication {
      val formData = Json.parse( """{"nbSignaux":"a"}""")
      val route = mock[Call]
      val function = mock[(TypeSensorForm, List[String], TypeMesure) => Future[Result]]
      val function2 = mock[TypeSensorForm => JsObject]
      val f = fixture

      f.listDataEmptyStream

      val r = f.controller.submitForm("error", route)(function2)(function)(FakeRequest(POST, "url").withJsonBody(formData).withSession("user" -> """{"login":"test"}"""))

      status(r) must equalTo(BAD_REQUEST)
      contentType(r) must beSome.which(_ == "text/html")
      val content = contentAsString(r)
      content must contain("<title>Inventaire des capteurs</title>")
      content must contains("<span class=\"control-label errors\">This field is required</span>", 6)
      content must contains("<span class=\"control-label errors\">Numeric value expected</span>", 1)

      f.verifyCallListData
    }

    "send bad_request with form and signal not valid integer" in new WithApplication {
      val formData = Json.parse( """{"nbSignaux":"0"}""")
      val route = mock[Call]
      val function = mock[(TypeSensorForm, List[String], TypeMesure) => Future[Result]]
      val function2 = mock[TypeSensorForm => JsObject]
      val f = fixture

      f.listDataEmptyStream

      val r = f.controller.submitForm("error", route)(function2)(function)(FakeRequest(POST, "url").withJsonBody(formData).withSession("user" -> """{"login":"test"}"""))

      status(r) must equalTo(BAD_REQUEST)
      contentType(r) must beSome.which(_ == "text/html")
      val content = contentAsString(r)
      content must contain("<title>Inventaire des capteurs</title>")
      content must contains("<span class=\"control-label errors\">This field is required</span>", 6)
      content must contains("<span class=\"control-label errors\">Must be greater or equal to 1</span>", 1)

      f.verifyCallListData
    }

    "send Bad_Request for empty list especes" in new WithApplication {
      val formData = Json.parse( """{"model":"mod","types":"typ","espece":[],"fabricant":"fab","nbSignaux":"1","mesure":"mes","unite":"u"}""")
      val route = mock[Call]
      val function = mock[(TypeSensorForm, List[String], TypeMesure) => Future[Result]]
      val function2 = mock[TypeSensorForm => JsObject]
      val f = fixture

      f.listDataEmptyStream

      val r = f.controller.submitForm("error", route)(function2)(function)(FakeRequest(POST, "url").withJsonBody(formData).withSession("user" -> """{"login":"test"}"""))

      status(r) must equalTo(BAD_REQUEST)
      contentType(r) must beSome.which(_ == "text/html")
      val content = contentAsString(r)
      content must contain("<span class=\"control-label errors\">This field is required</span>")

      f.verifyCallListData
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

      fix.typeSensorDaoMock.findOne(any[JsObject])(any[ExecutionContext]) returns future{Some(TypeSensor(bson,"type1","modele1",bson2,"fab1",1,List[String]("esp1","esp2")))}
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

      fix.typeSensorDaoMock.findOne(any[JsObject])(any[ExecutionContext]) returns future{Some(TypeSensor(bson,"type1","modele1",bson2,"fab1",1,List[String]("esp1","esp2")))}
      fix.typeSensorDaoMock.updateById(any[BSONObjectID],any[TypeSensor],any[GetLastError])(any[Writes[TypeSensor]],any[ExecutionContext]) returns future{lastError}

      val r = fix.controller.updateWithDeleteColumn(Json.obj(),true)

      status(r) must equalTo(SEE_OTHER)
      header("Location",r) must equalTo(Some("/inventary/sensors"))

      there was one(fix.typeSensorDaoMock).findOne(any[JsObject])(any[ExecutionContext])
      there was one(fix.typeSensorDaoMock).updateById(any[BSONObjectID],any[TypeSensor],any[GetLastError])(any[Writes[TypeSensor]],any[ExecutionContext])
    }
  }

  "When method actionWhenFormValid is called, TypeSensorManager" should{
    "send Bad_Request for existing type" in new WithApplication {
      val espece=List("esp1","esp2")
      val formData = TypeSensorForm("mod","typ",espece,"fab",1,"mes","u")
      val route=mock[Call]
      val function=mock[(TypeSensorForm,List[String],TypeMesure)=>Future[Result]]
      val function2=mock[TypeSensorForm=>JsObject]
      val f=fixture
      val typeSensor=mock[TypeSensor]

      function2.apply(org.mockito.Matchers.eq(formData)) returns Json.obj()
      f.typeSensorDaoMock.findAll(any[JsObject],any[JsObject])(any[ExecutionContext]) returns future{List(typeSensor)}
      f.listDataEmptyStream

      val r = f.controller.actionWhenFormValid("error message",route,formData,espece,function2,function)(FakeRequest(POST,"url").withSession("user" -> """{"login":"test"}"""))

      status(r) must equalTo(BAD_REQUEST)
      contentType(r) must beSome.which(_ == "text/html")
      val content = contentAsString(r)
      content must contain("<div class=\"alert alert-danger\" role=\"alert\">Ce type de capteur existe déjà</div>")

      there was one(f.typeSensorDaoMock).findAll(any[JsObject],any[JsObject])(any[ExecutionContext])
      there was one(function2).apply(org.mockito.Matchers.eq(formData))
      f.verifyCallListData
    }

    "send Bad_Request for existing type but delete" in new WithApplication {
      val espece=List("esp1","esp2")
      val formData = TypeSensorForm("mod","typ",espece,"fab",1,"mes","u")
      val route=mock[Call]
      val function=mock[(TypeSensorForm,List[String],TypeMesure)=>Future[Result]]
      val function2=mock[TypeSensorForm=>JsObject]
      val f=fixture
      val typeSensor=mock[TypeSensor]

      typeSensor.delete returns true
      function2.apply(org.mockito.Matchers.eq(formData)) returns Json.obj()
      f.typeSensorDaoMock.findAll(any[JsObject],any[JsObject])(any[ExecutionContext]) returns future{List(typeSensor)}
      f.listDataEmptyStream

      val r = f.controller.actionWhenFormValid("error message",route,formData,espece,function2,function)(FakeRequest(POST,"url").withSession("user" -> """{"login":"test"}"""))

      status(r) must equalTo(BAD_REQUEST)
      contentType(r) must beSome.which(_ == "text/html")
      val content = contentAsString(r)
      content must contain("<div class=\"alert alert-danger\" role=\"alert\">error message</div>")

      there was one(f.typeSensorDaoMock).findAll(any[JsObject],any[JsObject])(any[ExecutionContext])
      there was one(function2).apply(org.mockito.Matchers.eq(formData))
      f.verifyCallListData
    }

    "send 500 Internal error if mongoDB error when find all sensor type" in new WithApplication {
      val espece=List("esp1","esp2")
      val formData = TypeSensorForm("mod","typ",espece,"fab",1,"mes","u")
      val route=mock[Call]
      val function=mock[(TypeSensorForm,List[String],TypeMesure)=>Future[Result]]
      val function2=mock[TypeSensorForm=>JsObject]
      val futureMock=mock[Future[List[TypeSensor]]]
      val fix=fixture
      val throwable=mock[Throwable]

      function2.apply(org.mockito.Matchers.eq(formData)) returns Json.obj()
      fix.typeSensorDaoMock.findAll(any[JsObject],any[JsObject])(any[ExecutionContext]) returns futureMock
      futureMock.flatMap(any[List[TypeSensor]=>Future[List[TypeSensor]]])(any[ExecutionContext]) returns futureMock
      futureMock.recover(any[PartialFunction[Throwable,Result]])(any[ExecutionContext]) answers (vals => future{vals.asInstanceOf[PartialFunction[Throwable,Result]](throwable)})

      val r = fix.controller.actionWhenFormValid("error",route,formData,espece,function2,function)(FakeRequest(POST,"url").withSession("user" -> """{"login":"test"}"""))

      status(r) must equalTo(INTERNAL_SERVER_ERROR)

      there was one(function2).apply(org.mockito.Matchers.eq(formData))
      there was one(fix.typeSensorDaoMock).findAll(any[JsObject],any[JsObject])(any[ExecutionContext])
      there was one(futureMock).flatMap(any[List[TypeSensor]=>Future[List[TypeSensor]]])(any[ExecutionContext])
      there was one(futureMock).recover(any[PartialFunction[Throwable,Result]])(any[ExecutionContext])
    }

    "send 500 Internal error if mongoDB error when find mesure" in new WithApplication {
      val espece=List("esp1","esp2")
      val formData = TypeSensorForm("mod","typ",espece,"fab",1,"mes","u")
      val route=mock[Call]
      val function=mock[(TypeSensorForm,List[String],TypeMesure)=>Future[Result]]
      val function2=mock[TypeSensorForm=>JsObject]
      val futureMock=mock[Future[Option[TypeMesure]]]
      val fix=fixture
      val throwable=mock[Throwable]

      function2.apply(org.mockito.Matchers.eq(formData)) returns Json.obj()
      fix.typeSensorDaoMock.findAll(any[JsObject],any[JsObject])(any[ExecutionContext]) returns future{List()}
      fix.typeMesureDaoMock.findOne(any[JsObject])(any[ExecutionContext]) returns futureMock
      futureMock.flatMap(any[Option[TypeMesure]=>Future[Option[TypeMesure]]])(any[ExecutionContext]) returns futureMock
      futureMock.recover(any[PartialFunction[Throwable,Result]])(any[ExecutionContext]) answers (vals => future{vals.asInstanceOf[PartialFunction[Throwable,Result]](throwable)})

      val r = fix.controller.actionWhenFormValid("error",route,formData,espece,function2,function)(FakeRequest(POST,"url").withSession("user" -> """{"login":"test"}"""))

      status(r) must equalTo(INTERNAL_SERVER_ERROR)

      there was one(function2).apply(org.mockito.Matchers.eq(formData))
      there was one(fix.typeSensorDaoMock).findAll(any[JsObject],any[JsObject])(any[ExecutionContext])
      there was one(futureMock).flatMap(any[Option[TypeMesure]=>Future[Option[TypeMesure]]])(any[ExecutionContext])
      there was one(futureMock).recover(any[PartialFunction[Throwable,Result]])(any[ExecutionContext])
      there was one(fix.typeMesureDaoMock).findOne(any[JsObject])(any[ExecutionContext])
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
      content must contain("<input id=\"model\" name=\"model\" class=\"form-control\" list=\"list_modele\" type=\"text\" autocomplete=\"off\" value=\"\"/>")
      content must contain("<input id=\"types\" name=\"types\" class=\"form-control\" list=\"list_type\" type=\"text\" autocomplete=\"off\" value=\"\"/>")
      content must contain("<input id=\"espece\" name=\"espece[]\" class=\"form-control\" list=\"list_espece\" type=\"text\" autocomplete=\"off\" value=\"\"/>")
      content must contain("<input id=\"fabricant\" name=\"fabricant\" class=\"form-control\" list=\"list_fabricant\" type=\"text\" autocomplete=\"off\" value=\"\"/>")
      content must contain("<input id=\"nbSignaux\" name=\"nbSignaux\" class=\"form-control\" type=\"number\" autocomplete=\"off\" value=\"\"/>")
      content must contain("<input id=\"mesure\" name=\"mesure\" class=\"form-control\" list=\"list_mesure\" type=\"text\" autocomplete=\"off\" value=\"\"/>")
      content must contain("<input id=\"unite\" name=\"unite\" class=\"form-control\" list=\"list_unite\" type=\"text\" autocomplete=\"off\" value=\"\"/>")

      f.verifyCallListData
    }

    "send redirect after insert type mesure and type sensor" in new WithApplication {
      val formData = Json.parse("""{"model":"mod","types":"typ","espece":["esp"],"fabricant":"fab","nbSignaux":"1","mesure":"mes","unite":"u"}""")
      val lastError=mock[LastError]
      val f=fixture

      f.typeSensorDaoMock.findAll(any[JsObject],any[JsObject])(any[ExecutionContext]) returns future{List()}
      f.typeMesureDaoMock.findOne(any[JsObject])(any[ExecutionContext]) returns future{None}
      f.typeMesureDaoMock.insert(any[TypeMesure],any[GetLastError])(any[ExecutionContext]) returns future{lastError}
      f.typeSensorDaoMock.insert(any[TypeSensor],any[GetLastError])(any[ExecutionContext]) returns future{lastError}

      val r = f.controller.typeInsert.apply(FakeRequest(POST, "/inventary/sensors/type").withJsonBody(formData).withSession("user" -> """{"login":"test"}"""))

      status(r) must equalTo(SEE_OTHER)

      there was one(f.typeSensorDaoMock).findAll(any[JsObject],any[JsObject])(any[ExecutionContext])
      there was one(f.typeMesureDaoMock).findOne(any[JsObject])(any[ExecutionContext])
      there was one(f.typeMesureDaoMock).insert(any[TypeMesure],any[GetLastError])(any[ExecutionContext])
      there was one(f.typeSensorDaoMock).insert(any[TypeSensor],any[GetLastError])(any[ExecutionContext])
    }

    "send redirect after insert type sensor" in new WithApplication {
      val formData = Json.parse("""{"model":"mod","types":"typ","espece":["esp"],"fabricant":"fab","nbSignaux":"1","mesure":"mes","unite":"u"}""")
      val lastError=mock[LastError]
      val f=fixture

      f.typeSensorDaoMock.findAll(any[JsObject],any[JsObject])(any[ExecutionContext]) returns future{List()}
      f.typeMesureDaoMock.findOne(any[JsObject])(any[ExecutionContext]) returns future{Some(TypeMesure(bson,"non","u"))}
      f.typeSensorDaoMock.insert(any[TypeSensor],any[GetLastError])(any[ExecutionContext]) returns future{lastError}

      val r = f.controller.typeInsert.apply(FakeRequest(POST, "/inventary/sensors/type").withJsonBody(formData).withSession("user" -> """{"login":"test"}"""))

      status(r) must equalTo(SEE_OTHER)

      there was one(f.typeSensorDaoMock).findAll(any[JsObject],any[JsObject])(any[ExecutionContext])
      there was one(f.typeMesureDaoMock).findOne(any[JsObject])(any[ExecutionContext])
      there was one(f.typeSensorDaoMock).insert(any[TypeSensor],any[GetLastError])(any[ExecutionContext])
    }

    "send redirect after reactivat type sensor" in new WithApplication {
      val formData = Json.parse("""{"model":"mod","types":"typ","espece":["esp"],"fabricant":"fab","nbSignaux":"1","mesure":"mes","unite":"u","send":"Réactiver"}""")
      val lastError=mock[LastError]
      val f=fixture
      val typeSensor=TypeSensor(bson,"type1","modele1",bson2,"fab1",1,List[String]("esp1","esp2"),true)

      f.typeSensorDaoMock.findAll(any[JsObject],any[JsObject])(any[ExecutionContext]) returns future{List(typeSensor)}
      f.typeMesureDaoMock.findOne(any[JsObject])(any[ExecutionContext]) returns future{Some(TypeMesure(bson,"non","u"))}
      f.typeSensorDaoMock.findOne(any[JsObject])(any[ExecutionContext]) returns future{Some(typeSensor)}
      f.typeSensorDaoMock.updateById(any[BSONObjectID],any[TypeSensor],any[GetLastError])(any[Writes[TypeSensor]],any[ExecutionContext]) returns future{lastError}

      val r = f.controller.typeInsert.apply(FakeRequest(POST, "/inventary/sensors/type").withJsonBody(formData).withSession("user" -> """{"login":"test"}"""))

      status(r) must equalTo(SEE_OTHER)

      there was one(f.typeSensorDaoMock).findAll(any[JsObject],any[JsObject])(any[ExecutionContext])
      there was one(f.typeMesureDaoMock).findOne(any[JsObject])(any[ExecutionContext])
      there was one(f.typeSensorDaoMock).findOne(any[JsObject])(any[ExecutionContext])
      there was one(f.typeSensorDaoMock).updateById(any[BSONObjectID],any[TypeSensor],any[GetLastError])(any[Writes[TypeSensor]],any[ExecutionContext])
    }

    "send 500 internal error if mongoDB error when insert sensor type" in new WithApplication {
      val formData = Json.parse("""{"model":"mod","types":"typ","espece":["esp"],"fabricant":"fab","nbSignaux":"1","mesure":"mes","unite":"u"}""")
      val futureMock=mock[Future[LastError]]
      val throwable=mock[Throwable]
      val f=fixture

      f.typeSensorDaoMock.findAll(any[JsObject],any[JsObject])(any[ExecutionContext]) returns future{List()}
      f.typeMesureDaoMock.findOne(any[JsObject])(any[ExecutionContext]) returns future{Some(TypeMesure(bson,"non","u"))}
      f.typeSensorDaoMock.insert(any[TypeSensor],any[GetLastError])(any[ExecutionContext]) returns futureMock
      futureMock.map(any[LastError=>LastError])(any[ExecutionContext]) returns futureMock
      futureMock.recover(any[PartialFunction[Throwable,Result]])(any[ExecutionContext]) answers {value => future{value.asInstanceOf[PartialFunction[Throwable,Result]](throwable)}}

      val r = f.controller.typeInsert.apply(FakeRequest(POST, "/inventary/sensors/type").withJsonBody(formData).withSession("user" -> """{"login":"test"}"""))

      status(r) must equalTo(INTERNAL_SERVER_ERROR)

      there was one(f.typeSensorDaoMock).findAll(any[JsObject],any[JsObject])(any[ExecutionContext])
      there was one(f.typeMesureDaoMock).findOne(any[JsObject])(any[ExecutionContext])
      there was one(f.typeSensorDaoMock).insert(any[TypeSensor],any[GetLastError])(any[ExecutionContext])
      there was one(futureMock).map(any[LastError=>LastError])(any[ExecutionContext])
      there was one(futureMock).recover(any[PartialFunction[Throwable,Result]])(any[ExecutionContext])
    }
  }

  "When user is on the resource /inventary/sensors/:id/update , TypeSensorManager" should {
    "send 200 on OK with a form" in new WithApplication {
      val typeSensor=Some(TypeSensor(bson,"type","mod",bson2,"fab",1,List[String]("esp1"),false))
      val typeMesure=Some(TypeMesure(bson2,"Tension","Volt"))

      val fix=fixture

      fix.typeSensorDaoMock.findById(any[BSONObjectID])(any[ExecutionContext]) returns future{typeSensor}
      fix.typeMesureDaoMock.findById(any[BSONObjectID])(any[ExecutionContext]) returns future{typeMesure}
      fix.listDataEmptyStream

      val r = fix.controller.typeUpdatePage(bson.stringify).apply(FakeRequest(GET, "/inventary/sensors/"+bson.stringify+"/update").withSession("user" -> """{"login":"test"}"""))


      status(r) must equalTo(OK)
      contentType(r) must beSome.which(_ == "text/html")
      val content = contentAsString(r)
      content must contain("<title>Inventaire des capteurs</title>")
      content must contain("<input id=\"model\" name=\"model\" class=\"form-control\" list=\"list_modele\" type=\"text\" autocomplete=\"off\" value=\"mod\"/>")
      content must contain("<input id=\"types\" name=\"types\" class=\"form-control\" list=\"list_type\" type=\"text\" autocomplete=\"off\" value=\"type\"/>")
      content must contain("<input id=\"espece\" name=\"espece[]\" class=\"form-control\" list=\"list_espece\" type=\"text\" autocomplete=\"off\" value=\"esp1\"/>")
      content must contain("<input id=\"fabricant\" name=\"fabricant\" class=\"form-control\" list=\"list_fabricant\" type=\"text\" autocomplete=\"off\" value=\"fab\"/>")
      content must contain("<input id=\"nbSignaux\" name=\"nbSignaux\" class=\"form-control\" type=\"number\" autocomplete=\"off\" value=\"1\"/>")
      content must contain("<input id=\"mesure\" name=\"mesure\" class=\"form-control\" list=\"list_mesure\" type=\"text\" autocomplete=\"off\" value=\"Tension\"/>")
      content must contain("<input id=\"unite\" name=\"unite\" class=\"form-control\" list=\"list_unite\" type=\"text\" autocomplete=\"off\" value=\"Volt\"/>")

      there was one(fix.typeSensorDaoMock).findById(any[BSONObjectID])(any[ExecutionContext])
      there was one(fix.typeMesureDaoMock).findById(any[BSONObjectID])(any[ExecutionContext])
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

    "send Redirect for type mesure not found" in new WithApplication {
      val typeSensor=Some(TypeSensor(bson,"type","mod",bson2,"fab",1,List[String]("esp1"),false))

      val fix=fixture

      fix.typeSensorDaoMock.findById(any[BSONObjectID])(any[ExecutionContext]) returns future{typeSensor}
      fix.typeMesureDaoMock.findById(any[BSONObjectID])(any[ExecutionContext]) returns future{None}

      val r = fix.controller.typeUpdatePage(bson.stringify).apply(FakeRequest(GET, "/inventary/sensors/"+bson.stringify+"/update").withSession("user" -> """{"login":"test"}"""))


      status(r) must equalTo(SEE_OTHER)
      header("Location",r) must equalTo(Some("/inventary/sensors"))

      there was one(fix.typeSensorDaoMock).findById(any[BSONObjectID])(any[ExecutionContext])
      there was one(fix.typeMesureDaoMock).findById(any[BSONObjectID])(any[ExecutionContext])
    }

    "send 500 Internal error if mongoDB error when find sensor type" in new WithApplication {
      val future_Mock=mock[Future[Option[TypeSensor]]]
      val throwable=mock[Throwable]

      val fix=fixture

      fix.typeSensorDaoMock.findById(any[BSONObjectID])(any[ExecutionContext]) returns future_Mock
      future_Mock.flatMap(any[(Option[TypeSensor])=>Future[Option[TypeSensor]]])(any[ExecutionContext]) returns future_Mock
      future_Mock.recover(any[PartialFunction[Throwable,Result]])(any[ExecutionContext]) answers (vals => future{vals.asInstanceOf[PartialFunction[Throwable,Result]](throwable)})

      val r = fix.controller.typeUpdatePage(bson.stringify).apply(FakeRequest(GET, "/inventary/sensors/"+bson.stringify+"/update").withSession("user" -> """{"login":"test"}"""))


      status(r) must equalTo(INTERNAL_SERVER_ERROR)

      there was one(fix.typeSensorDaoMock).findById(any[BSONObjectID])(any[ExecutionContext])
      there was one(future_Mock).flatMap(any[(Option[TypeSensor])=>Future[Option[TypeSensor]]])(any[ExecutionContext])
      there was one(future_Mock).recover(any[PartialFunction[Throwable,Result]])(any[ExecutionContext])
    }

    "send 500 Internal error if mongoDB error when find mesure type" in new WithApplication {
      val future_Mock=mock[Future[Option[TypeMesure]]]
      val typeSensor=Some(TypeSensor(bson,"type","mod",bson2,"fab",1,List[String]("esp1"),false))
      val throwable=mock[Throwable]

      val fix = fixture

      fix.typeSensorDaoMock.findById(any[BSONObjectID])(any[ExecutionContext]) returns future{typeSensor}
      fix.typeMesureDaoMock.findById(any[BSONObjectID])(any[ExecutionContext]) returns future_Mock
      future_Mock.flatMap(any[(Option[TypeMesure])=>Future[Option[TypeMesure]]])(any[ExecutionContext]) returns future_Mock
      future_Mock.recover(any[PartialFunction[Throwable,Result]])(any[ExecutionContext]) answers (vals => future{vals.asInstanceOf[PartialFunction[Throwable,Result]](throwable)})

      val r = fix.controller.typeUpdatePage(bson.stringify).apply(FakeRequest(GET, "/inventary/sensors/"+bson.stringify+"/update").withSession("user" -> """{"login":"test"}"""))

      status(r) must equalTo(INTERNAL_SERVER_ERROR)

      there was one(fix.typeSensorDaoMock).findById(any[BSONObjectID])(any[ExecutionContext])
      there was one(fix.typeMesureDaoMock).findById(any[BSONObjectID])(any[ExecutionContext])
      there was one(future_Mock).flatMap(any[(Option[TypeMesure])=>Future[Option[TypeMesure]]])(any[ExecutionContext])
      there was one(future_Mock).recover(any[PartialFunction[Throwable,Result]])(any[ExecutionContext])
    }

    "send redirect after insert type mesure and update type sensor" in new WithApplication {
      val formData = Json.parse("""{"model":"mod","types":"typ","espece":["esp"],"fabricant":"fab","nbSignaux":"1","mesure":"mes","unite":"u"}""")
      val fix=fixture
      val lastError=mock[LastError]

      fix.typeSensorDaoMock.findAll(any[JsObject],any[JsObject])(any[ExecutionContext]) returns future{List()}
      fix.typeMesureDaoMock.findOne(any[JsObject])(any[ExecutionContext]) returns future{None}
      fix.typeMesureDaoMock.insert(any[TypeMesure],any[GetLastError])(any[ExecutionContext]) returns future{lastError}
      fix.typeSensorDaoMock.updateById(any[BSONObjectID],any[TypeSensor],any[GetLastError])(any[Writes[TypeSensor]],any[ExecutionContext]) returns future{lastError}

      val r = fix.controller.typeUpdate(bson.stringify).apply(FakeRequest(POST, "/inventary/sensors/"+bson.stringify+"/update").withJsonBody(formData).withSession("user" -> """{"login":"test"}"""))

      there was one(fix.typeSensorDaoMock).findAll(any[JsObject],any[JsObject])(any[ExecutionContext])
      there was one(fix.typeMesureDaoMock).findOne(any[JsObject])(any[ExecutionContext])
      there was one(fix.typeMesureDaoMock).insert(any[TypeMesure],any[GetLastError])(any[ExecutionContext])
      there was one(fix.typeSensorDaoMock).updateById(any[BSONObjectID],any[TypeSensor],any[GetLastError])(any[Writes[TypeSensor]],any[ExecutionContext])

      status(r) must equalTo(SEE_OTHER)
      header("Location",r) must equalTo(Some("/inventary/sensors"))

      there was one(fix.typeSensorDaoMock).findAll(any[JsObject],any[JsObject])(any[ExecutionContext])
      there was one(fix.typeMesureDaoMock).findOne(any[JsObject])(any[ExecutionContext])
      there was one(fix.typeMesureDaoMock).insert(any[TypeMesure],any[GetLastError])(any[ExecutionContext])
      there was one(fix.typeSensorDaoMock).updateById(any[BSONObjectID],any[TypeSensor],any[GetLastError])(any[Writes[TypeSensor]],any[ExecutionContext])
    }

    "send redirect after update type sensor" in new WithApplication {
      val formData = Json.parse("""{"model":"mod","types":"typ","espece":["esp"],"fabricant":"fab","nbSignaux":"1","mesure":"mes","unite":"u","send":"Ignorer"}""")
      val lastError=mock[LastError]
      val fix=fixture

      fix.typeSensorDaoMock.findAll(any[JsObject],any[JsObject])(any[ExecutionContext]) returns future{List(TypeSensor(bson,"type","mod",bson2,"fab",1,List[String]("esp1"),true))}
      fix.typeMesureDaoMock.findOne(any[JsObject])(any[ExecutionContext]) returns future{Some(TypeMesure(bson,"non","u"))}
      fix.typeSensorDaoMock.updateById(any[BSONObjectID],any[TypeSensor],any[GetLastError])(any[Writes[TypeSensor]],any[ExecutionContext]) returns future{lastError}

      val r = fix.controller.typeUpdate(bson.stringify).apply(FakeRequest(POST, "/inventary/sensors/"+bson.stringify+"/update").withJsonBody(formData).withSession("user" -> """{"login":"test"}"""))

      status(r) must equalTo(SEE_OTHER)
      header("Location",r) must equalTo(Some("/inventary/sensors"))

      there was one(fix.typeSensorDaoMock).findAll(any[JsObject],any[JsObject])(any[ExecutionContext])
      there was one(fix.typeMesureDaoMock).findOne(any[JsObject])(any[ExecutionContext])
      there was one(fix.typeSensorDaoMock).updateById(any[BSONObjectID],any[TypeSensor],any[GetLastError])(any[Writes[TypeSensor]],any[ExecutionContext])
    }

    "send 500 internal error if mongoDB error wehn update sensor type" in new WithApplication {
      val formData = Json.parse("""{"model":"mod","types":"typ","espece":["esp"],"fabricant":"fab","nbSignaux":"1","mesure":"mes","unite":"u"}""")
      val futureMock=mock[Future[LastError]]
      val fix=fixture
      val throwable=mock[Throwable]

      fix.typeSensorDaoMock.findAll(any[JsObject],any[JsObject])(any[ExecutionContext]) returns future{List()}
      fix.typeMesureDaoMock.findOne(any[JsObject])(any[ExecutionContext]) returns future{Some(TypeMesure(bson,"non","u"))}
      fix.typeSensorDaoMock.updateById(any[BSONObjectID],any[TypeSensor],any[GetLastError])(any[Writes[TypeSensor]],any[ExecutionContext]) returns futureMock
      futureMock.map(any[LastError=>LastError])(any[ExecutionContext]) returns futureMock
      futureMock.recover(any[PartialFunction[Throwable,Result]])(any[ExecutionContext]) answers {value=>future{value.asInstanceOf[PartialFunction[Throwable,Result]](throwable)}}

      val r = fix.controller.typeUpdate(bson.stringify).apply(FakeRequest(POST, "/inventary/sensors/"+bson.stringify+"/update").withJsonBody(formData).withSession("user" -> """{"login":"test"}"""))

      status(r) must equalTo(INTERNAL_SERVER_ERROR)

      there was one(fix.typeSensorDaoMock).findAll(any[JsObject],any[JsObject])(any[ExecutionContext])
      there was one(fix.typeMesureDaoMock).findOne(any[JsObject])(any[ExecutionContext])
      there was one(fix.typeSensorDaoMock).updateById(any[BSONObjectID],any[TypeSensor],any[GetLastError])(any[Writes[TypeSensor]],any[ExecutionContext])
      there was one(futureMock).map(any[LastError=>LastError])(any[ExecutionContext])
      there was one(futureMock).recover(any[PartialFunction[Throwable,Result]])(any[ExecutionContext])
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

      fix.typeSensorDaoMock.findOne(any[JsObject])(any[ExecutionContext]) returns future{Some(TypeSensor(bson,"type1","modele1",bson2,"fab1",1,List[String]("esp1","esp2")))}
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
      val func1=mock[Unit=>Future[Result]]
      val func2=mock[Unit=>Future[Result]]

      fix.typeSensorDaoMock.findOne(any[JsObject])(any[ExecutionContext]) returns future{Some(TypeSensor(bson,"type","mod",bson2,"fab",1,List[String]("esp1"),false))}
      func1.apply(any[Unit]) returns future{Results.Ok("func found")}

      val req=FakeRequest(GET, "url")
      val action=Action.async{fix.controller.doIfTypeSensorFound(bson)(func1)(func2)}
      val r=call(action,req)

      status(r) must equalTo(OK)
      contentAsString(r) must equalTo("func found")

      there was one(fix.typeSensorDaoMock).findOne(any[JsObject])(any[ExecutionContext])
      there was one(func1).apply(any[Unit])
      there was no(func2).apply(any[Unit])
    }

    "execute particular function if sensor type not found" in new WithApplication{
      val fix=fixture
      val func1=mock[Unit=>Future[Result]]
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
      there was no(func1).apply(any[Unit])
    }

    "send Internal server error if error mongoDB" in new WithApplication{
      val fix=fixture
      val func1=mock[Unit=>Future[Result]]
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
}
