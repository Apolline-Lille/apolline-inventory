import java.util.Date

import controllers.{TypeSensorManagerLike, SensorManagerLike, SensorForm, SensorManager}
import models._
import org.junit.runner.RunWith
import org.mockito.ArgumentCaptor
import org.specs2.matcher.{MatchResult, Expectable, Matcher}
import org.specs2.mock.Mockito
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner
import play.api.data.Form
import play.api.mvc._
import play.api.test.Helpers._
import play.api.test.{FakeRequest, WithApplication}
import play.api.libs.json.{Writes, Json, JsObject}
import reactivemongo.bson.BSONObjectID
import reactivemongo.core.commands.{LastError, GetLastError}

import scala.concurrent._
import scala.concurrent.duration.Duration

@RunWith(classOf[JUnitRunner])
class SensorManagerSpec extends Specification with Mockito {

  class SensorManagerTest extends SensorManagerLike
  class TypeSensorManagerTest extends TypeSensorManagerLike

  case class matchRegex(a: String) extends Matcher[String]() {
    override def apply[S <: String](t: Expectable[S]): MatchResult[S] = result(a.r.findFirstIn(t.value).nonEmpty, "okMessage",t.value +"not found " + a, t)
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

  def fixture = new {
    val typeSensorDaoMock = mock[TypeSensorDao]
    val typeMesureDaoMock = mock[TypeMesureDao]
    val sensorDaoMock = mock[SensorDao]
    val typeSensorController=mock[TypeSensorManagerLike]
    val controller = new SensorManagerTest {
      override val typeSensorDao: TypeSensorDao = typeSensorDaoMock
      override val typeMesureDao: TypeMesureDao = typeMesureDaoMock
      override val sensorDao: SensorDao = sensorDaoMock
      override val typeSensorManager:TypeSensorManagerLike = typeSensorController
    }
    val typeSensor=TypeSensor(bson,"type1","modele1",bson2,"fab1",1,List[String]("esp1","esp2"))

    def applyFoundFunction() {
      typeSensorController.doIfTypeSensorFound(org.mockito.Matchers.eq(bson))(any[Unit => Future[Result]])(any[Unit => Future[Result]]) answers { (params, _) => params match {
        case Array(_, p: (Unit => Future[Result]), _) => p.apply()
      }}
    }

    def applyNotFoundFunction() {
      typeSensorController.doIfTypeSensorFound(org.mockito.Matchers.eq(bson))(any[Unit => Future[Result]])(any[Unit => Future[Result]]) answers { (params, _) => params match {
        case Array(_,_, p: (Unit => Future[Result])) => p.apply()
      }}
    }

  }

  "When user is not connected, SensorManager" should {
    "redirect to login for resource /inventary/sensors/:id" in new WithApplication{
      route(FakeRequest(GET, "/inventary/sensors/"+bson.stringify)).map(
       r=> {
         status(r) must equalTo(SEE_OTHER)
         header("Location", r) must equalTo(Some("/login"))
       }
      ).getOrElse(
        failure("Pas de retour de la fonction")
      )
    }

    "redirect to login for resource /inventary/sensors/:id/sensor" in new WithApplication{
      route(FakeRequest(GET, "/inventary/sensors/"+bson.stringify+"/sensor")).map(
        r=> {

          status(r) must equalTo(SEE_OTHER)
          header("Location", r) must equalTo(Some("/login"))
        }
      ).getOrElse(
        failure("Pas de retour de la fonction")
      )
    }

    "redirect to login for resource /inventary/sensors/:id/sensor" in new WithApplication{
      route(FakeRequest(POST, "/inventary/sensors/"+bson.stringify+"/sensor")).map(
        r=> {
          status(r) must equalTo(SEE_OTHER)
          header("Location", r) must equalTo(Some("/login"))
        }
      ).getOrElse(
        failure("Pas de retour de la fonction")
      )
    }

    "redirect to login for resource /inventary/sensors/:id/:id2" in new WithApplication{
      route(FakeRequest(GET, "/inventary/sensors/"+bson.stringify+"/"+bson2.stringify)).map(
        r=>{

          status(r) must equalTo(SEE_OTHER)
          header("Location",r) must equalTo(Some("/login"))
        }
      ).getOrElse(
        failure("Pas de retour de la fonction")
      )
    }

    "redirect to login for resource /inventary/sensors/:id/:id2" in new WithApplication{
      route(FakeRequest(POST, "/inventary/sensors/"+bson.stringify+"/"+bson2.stringify)).map(
        r=>{

          status(r) must equalTo(SEE_OTHER)
          header("Location",r) must equalTo(Some("/login"))
        }
      ).getOrElse(
        failure("Pas de retour de la fonction")
      )
    }

    "redirect to login for resource /inventary/sensors/:id/:id2/delete" in new WithApplication{
      route(FakeRequest(GET, "/inventary/sensors/"+bson.stringify+"/"+bson2.stringify+"/delete")).map(
        r=>{

          status(r) must equalTo(SEE_OTHER)
          header("Location",r) must equalTo(Some("/login"))
        }
      ).getOrElse(
        failure("Pas de retour de la fonction")
      )
    }

    "redirect to login for resource /inventary/sensors/:id/:id2/clone" in new WithApplication{
      route(FakeRequest(GET, "/inventary/sensors/"+bson.stringify+"/"+bson2.stringify+"/clone")).map(
        r=>{

          status(r) must equalTo(SEE_OTHER)
          header("Location",r) must equalTo(Some("/login"))
        }
      ).getOrElse(
        failure("Pas de retour de la fonction")
      )
    }
  }

  "When user is on resource /inventary/sensors/:id, SensorManager" should{

    "send 200 OK page with result" in new WithApplication{
      val f=fixture
      val typeMesure=TypeMesure(bson2,"mesure1","unite1")
      val list_sensor=List[Sensor](
        Sensor(bson3,"Id",bson,None,date,None,false,None)
      )
      f.sensorDaoMock.findAll(any[JsObject],org.mockito.Matchers.eq(Json.obj("acquisition" -> -1)))(any[ExecutionContext]) returns future{list_sensor}
      f.typeMesureDaoMock.findById(org.mockito.Matchers.eq(bson2))(any[ExecutionContext]) returns future{Some(typeMesure)}
      f.typeSensorDaoMock.findById(org.mockito.Matchers.eq(bson))(any[ExecutionContext]) returns  future{Some(f.typeSensor)}

      val r=f.controller.inventary(bson.stringify,"acquisition",-1).apply(FakeRequest(GET,"/inventary/sensors/"+bson.stringify).withSession("user" -> ""))

      status(r) must equalTo(OK)
      contentType(r) must beSome.which(_ == "text/html")
      val content = contentAsString(r)
      content must contain("<title>Inventaire des capteurs</title>")
      content must contain("<td>Id</td>")
      content must contain("<td>22/04/2015</td>")
      content must contain("<td></td>")
    }

     "method printListSensor return 200 OK page with the message 'Aucun résultat trouvé', if not have sensors" in new WithApplication{
       val f=fixture
       val typeMesure=TypeMesure(bson2,"mesure1","unite1")
       val list_sensor=List[Sensor]()

       val action=Action{f.controller.printListSensor(f.typeSensor,typeMesure,list_sensor,"id",1)}
       val req=FakeRequest(GET,"/inventary/sensors/"+bson.stringify)
       val r=call(action,req)


       status(r) must equalTo(OK)
       contentType(r) must beSome.which(_ == "text/html")
       val content = contentAsString(r)
       content must contain("<title>Inventaire des capteurs</title>")
       content must contain("<h3 style=\"text-align:center\">Aucun résultat trouvé</h3>")
       content must matchRegex("type1\\s*/\\s*modele1")
       content must matchRegex("<span class=\"bold\">\\s*Fabricant\\s*</span>\\s*:\\s*fab1")
       content must matchRegex("<span class=\"bold\">\\s*Nombre de signaux\\s*</span>\\s*:\\s*1\\s*\\(\\s*mesure1\\s*\\)")
       content must matchRegex("<span class=\"bold\">\\s*Espèces\\s*</span>\\s*:\\s*esp1, esp2")
       content must matchRegex("<span class=\"bold\">\\s*Stocks\\s*</span>\\s*:\\s*0")
     }

    "method printListSensor return 200 OK page with 2 sensors" in new WithApplication{
      val f=fixture
      val typeMesure=TypeMesure(bson2,"mesure1","unite1")
      val list_sensor=List[Sensor](
        Sensor(bson3,"Id",bson,None,date,None,false,None),
        Sensor(bson4,"Id2",bson,Some(date),date,Some(date),true,Some("un com"))
      )

      val action=Action{f.controller.printListSensor(f.typeSensor,typeMesure,list_sensor,"acquisition",1)}
      val req=FakeRequest(GET,"/inventary/sensors/"+bson.stringify)
      val r=call(action,req)

      status(r) must equalTo(OK)
      contentType(r) must beSome.which(_ == "text/html")
      val content = contentAsString(r)
      content must contain("<title>Inventaire des capteurs</title>")
      content must contain("<td>Id</td>")
      content must contains("<td>22/04/2015</td>",4)
      content must contain("<td></td>")
      content must contain("<td>Id2</td>")
      content must contain("<td>Hors service</td>")
    }

    "method findSensorsForPrint return 500 internal error if mongoDB error" in new WithApplication{
      val f=fixture
      val typeMesure=TypeMesure(bson2,"mesure1","unite1")
      val futureMock=mock[Future[List[Sensor]]]
      val throwable=mock[Throwable]

      futureMock.map(any[List[Sensor]=>List[Sensor]])(any[ExecutionContext]) returns futureMock
      futureMock.recover(any[PartialFunction[Throwable,Result]])(any[ExecutionContext]) answers (value=>future{value.asInstanceOf[PartialFunction[Throwable,Result]](throwable)})

      val action=Action.async{f.controller.findSensorsForPrint(futureMock,"id",1)(f.typeSensor)(typeMesure)}
      val req=FakeRequest(GET,"/inventary/sensors/"+bson.stringify)
      val r=call(action,req)


      status(r) must equalTo(INTERNAL_SERVER_ERROR)

      there was one(futureMock).map(any[List[Sensor]=>List[Sensor]])(any[ExecutionContext])
      there was one(futureMock).recover(any[PartialFunction[Throwable,Result]])(any[ExecutionContext])
    }

    "method findSensorsForPrint return 200 Ok page with 1 result" in new WithApplication{
      val f=fixture
      val typeMesure=TypeMesure(bson2,"mesure1","unite1")
      val list_sensor=List[Sensor](Sensor(bson3,"Id",bson,None,date,None,false,None))
      val futureMock=mock[Future[List[Sensor]]]

      futureMock.map(any[List[Sensor]=>Result])(any[ExecutionContext]) answers (value=>future{value.asInstanceOf[List[Sensor]=>Result](list_sensor)})

      val action=Action.async{f.controller.findSensorsForPrint(futureMock,"id",1)(f.typeSensor)(typeMesure)}
      val req=FakeRequest(GET,"/inventary/sensors/"+bson.stringify)
      val r=call(action,req)


      status(r) must equalTo(OK)
      contentType(r) must beSome.which(_ == "text/html")
      val content = contentAsString(r)
      content must contain("<title>Inventaire des capteurs</title>")
      content must contain("<td>Id</td>")
      content must contain("<td>22/04/2015</td>")
      content must contain("<td></td>")

      there was one(futureMock).map(any[List[Sensor]=>List[Sensor]])(any[ExecutionContext])
    }

    "method findTypeMesureForPrint return 500 internal error if mongoDB error" in new WithApplication{
      val f=fixture
      val typeMesure=TypeMesure(bson2,"mesure1","unite1")
      val futureMock=mock[Future[List[Sensor]]]
      val futureMockMesure=mock[Future[Option[TypeMesure]]]
      val func=mock[TypeMesure=>Future[Result]]
      val throwable=mock[Throwable]

      f.typeMesureDaoMock.findById(org.mockito.Matchers.eq(bson2))(any[ExecutionContext]) returns futureMockMesure
      futureMockMesure.flatMap(any[Option[TypeMesure]=>Future[Option[TypeMesure]]])(any[ExecutionContext]) returns futureMockMesure
      futureMockMesure.recover(any[PartialFunction[Throwable,Result]])(any[ExecutionContext]) answers (value=>future{value.asInstanceOf[PartialFunction[Throwable,Result]](throwable)})

      val action=Action.async{f.controller.findTypeMesureForPrint(futureMock)(f.typeSensor,func)}
      val req=FakeRequest(GET,"/inventary/sensors/"+bson.stringify)
      val r=call(action,req)


      status(r) must equalTo(INTERNAL_SERVER_ERROR)

      there was one(f.typeMesureDaoMock).findById(org.mockito.Matchers.eq(bson2))(any[ExecutionContext])
      there was one(futureMockMesure).flatMap(any[Option[TypeMesure]=>Future[Option[TypeMesure]]])(any[ExecutionContext])
      there was one(futureMockMesure).recover(any[PartialFunction[Throwable,Result]])(any[ExecutionContext])
    }

    "method findTypeMesureForPrint return Redirect if type mesure not found" in new WithApplication{
      val f=fixture
      val typeMesure=TypeMesure(bson2,"mesure1","unite1")
      val futureMockMesure=mock[Future[Option[TypeMesure]]]
      val futureMock=mock[Future[List[Sensor]]]
      val func=mock[TypeMesure=>Future[Result]]

      f.typeMesureDaoMock.findById(org.mockito.Matchers.eq(bson2))(any[ExecutionContext]) returns futureMockMesure
      futureMockMesure.flatMap(any[Option[TypeMesure]=>Future[Result]])(any[ExecutionContext]) answers (value=>value.asInstanceOf[Option[TypeMesure]=>Future[Result]](None))

      val action=Action.async{f.controller.findTypeMesureForPrint(futureMock)(f.typeSensor,func)}
      val req=FakeRequest(GET,"/inventary/sensors/"+bson.stringify)
      val r=call(action,req)


      status(r) must equalTo(SEE_OTHER)
      header("Location",r) must equalTo(Some("/inventary/sensors"))

      there was one(f.typeMesureDaoMock).findById(org.mockito.Matchers.eq(bson2))(any[ExecutionContext])
      there was one(futureMockMesure).flatMap(any[Option[TypeMesure]=>Future[Result]])(any[ExecutionContext])
    }

    "method findTypeMesureForPrint return 200 Ok page" in new WithApplication{
      val f=fixture
      val typeMesure=TypeMesure(bson2,"mesure1","unite1")
      val futureMockMesure=mock[Future[Option[TypeMesure]]]
      val futureMock=mock[Future[List[Sensor]]]
      val func=mock[TypeMesure=>Future[Result]]

      f.typeMesureDaoMock.findById(org.mockito.Matchers.eq(bson2))(any[ExecutionContext]) returns futureMockMesure
      futureMockMesure.flatMap(any[Option[TypeMesure]=>Future[Result]])(any[ExecutionContext]) answers (value=>value.asInstanceOf[Option[TypeMesure]=>Future[Result]](Some(typeMesure)))
      func.apply(org.mockito.Matchers.eq(typeMesure)) returns future{Results.Ok("ok")}

      val action=Action.async{f.controller.findTypeMesureForPrint(futureMock)(f.typeSensor,func)}
      val req=FakeRequest(GET,"/inventary/sensors/"+bson.stringify)
      val r=call(action,req)


      status(r) must equalTo(OK)
      contentAsString(r) must equalTo("ok")

      there was one(f.typeMesureDaoMock).findById(org.mockito.Matchers.eq(bson2))(any[ExecutionContext])
      there was one(futureMockMesure).flatMap(any[Option[TypeMesure]=>Future[Result]])(any[ExecutionContext])
      there was one(func).apply(org.mockito.Matchers.eq(typeMesure))
    }

    "method findTypeSensorForPrint return 500 internal error if mongoDB error" in new WithApplication{
      val f=fixture
      val futureMock=mock[Future[List[Sensor]]]
      val futureMockSensor=mock[Future[Option[TypeSensor]]]
      val func=mock[(TypeSensor,(TypeMesure=>Future[Result]))=>Future[Result]]
      val func2=mock[TypeSensor=>TypeMesure=>Future[Result]]
      val throwable=mock[Throwable]

      f.typeSensorDaoMock.findById(org.mockito.Matchers.eq(bson))(any[ExecutionContext]) returns futureMockSensor
      futureMockSensor.flatMap(any[Option[TypeSensor]=>Future[Option[TypeSensor]]])(any[ExecutionContext]) returns futureMockSensor
      futureMockSensor.recover(any[PartialFunction[Throwable,Result]])(any[ExecutionContext]) answers (value=>future{value.asInstanceOf[PartialFunction[Throwable,Result]](throwable)})

      val action=Action.async{f.controller.findTypeSensorForPrint(bson,futureMock)(func,func2)}
      val req=FakeRequest(GET,"/inventary/sensors/"+bson.stringify)
      val r=call(action,req)


      status(r) must equalTo(INTERNAL_SERVER_ERROR)

      there was one(f.typeSensorDaoMock).findById(org.mockito.Matchers.eq(bson))(any[ExecutionContext])
      there was one(futureMockSensor).flatMap(any[Option[TypeSensor]=>Future[Option[TypeSensor]]])(any[ExecutionContext])
      there was one(futureMockSensor).recover(any[PartialFunction[Throwable,Result]])(any[ExecutionContext])
    }

    "method findSensorsForPrint return Redirect if type sensor not found" in new WithApplication{
      val f=fixture
      val futureMockSensor=mock[Future[Option[TypeSensor]]]
      val futureMock=mock[Future[List[Sensor]]]
      val func=mock[(TypeSensor,(TypeMesure=>Future[Result]))=>Future[Result]]
      val func2=mock[TypeSensor=>TypeMesure=>Future[Result]]

      f.typeSensorDaoMock.findById(org.mockito.Matchers.eq(bson))(any[ExecutionContext]) returns futureMockSensor
      futureMockSensor.flatMap(any[Option[TypeSensor]=>Future[Result]])(any[ExecutionContext]) answers (value=>value.asInstanceOf[Option[TypeSensor]=>Future[Result]](None))

      val action=Action.async{f.controller.findTypeSensorForPrint(bson,futureMock)(func,func2)}
      val req=FakeRequest(GET,"/inventary/sensors/"+bson.stringify)
      val r=call(action,req)


      status(r) must equalTo(SEE_OTHER)
      header("Location",r) must equalTo(Some("/inventary/sensors"))

      there was one(f.typeSensorDaoMock).findById(org.mockito.Matchers.eq(bson))(any[ExecutionContext])
      there was one(futureMockSensor).flatMap(any[Option[TypeSensor]=>Future[Result]])(any[ExecutionContext])
    }

    "method findSensorsForPrint return 200 Ok page" in new WithApplication{
      val f=fixture
      val futureMockSensor=mock[Future[Option[TypeSensor]]]
      val futureMock=mock[Future[List[Sensor]]]
      val func=mock[(TypeSensor,(TypeMesure=>Future[Result]))=>Future[Result]]
      val func2=mock[TypeSensor=>TypeMesure=>Future[Result]]
      val func3=mock[TypeMesure=>Future[Result]]

      f.typeSensorDaoMock.findById(org.mockito.Matchers.eq(bson))(any[ExecutionContext]) returns futureMockSensor
      futureMockSensor.flatMap(any[Option[TypeSensor]=>Future[Result]])(any[ExecutionContext]) answers (value=>value.asInstanceOf[Option[TypeSensor]=>Future[Result]](Some(f.typeSensor)))
      func.apply(org.mockito.Matchers.eq(f.typeSensor),org.mockito.Matchers.eq(func3)) returns future{Results.Ok("ok")}
      func2.apply(org.mockito.Matchers.eq(f.typeSensor)) returns func3

      val action=Action.async{f.controller.findTypeSensorForPrint(bson,futureMock)(func,func2)}
      val req=FakeRequest(GET,"/inventary/sensors/"+bson.stringify)
      val r=call(action,req)


      status(r) must equalTo(OK)
      contentAsString(r) must equalTo("ok")

      there was one(f.typeSensorDaoMock).findById(org.mockito.Matchers.eq(bson))(any[ExecutionContext])
      there was one(futureMockSensor).flatMap(any[Option[TypeSensor]=>Future[Result]])(any[ExecutionContext])
      there was one(func).apply(org.mockito.Matchers.eq(f.typeSensor),org.mockito.Matchers.eq(func3))
      there was one(func2).apply(org.mockito.Matchers.eq(f.typeSensor))
    }
  }

  "When user is on the page /inventary/sensors/:id/sensor, SensorManager" should{
    "send 200 Ok page with an empty form" in new WithApplication{
      val f=fixture

      f.applyFoundFunction()

      val req=FakeRequest(GET,"/inventary/sensors/"+bson.stringify+"/sensor").withSession("user" -> "")
      val r=f.controller.sensorPage(bson.stringify).apply(req)

      status(r) must equalTo(OK)
      contentType(r) must beSome.which(_ == "text/html")
      val content = contentAsString(r)
      content must contain("<title>Inventaire des capteurs</title>")
      content must contain("<input type=\"text\" id=\"id\" name=\"id\" value=\"\" class=\"form-control\"/>")
      content must contain("<input type=\"date\" id=\"acquisition\" name=\"acquisition\" value=\"\" class=\"form-control\"/>")
      content must contain("<input type=\"date\" id=\"expiration\" name=\"expiration\" value=\"\" class=\"form-control\"/>")
      content must contain("<input type=\"date\" id=\"firstUse\" name=\"firstUse\" value=\"\" class=\"form-control\"/>")
      content must contain("<input type=\"checkbox\" id=\"hs\" name=\"hs\" value=\"true\"  />")
      content must contain("<textarea id=\"commentaire\" name=\"commentaire\" class=\"form-control\"></textarea>")

      there was one(f.typeSensorController).doIfTypeSensorFound(org.mockito.Matchers.eq(bson))(any[Unit=>Future[Result]])(any[Unit=>Future[Result]])
    }

    "send bad_request with the form if sensor type not exist" in new WithApplication{
      val f=fixture
      val func=mock[Sensor=>SensorForm]
      val routeCall=mock[Call]

      f.applyNotFoundFunction()

      val req=FakeRequest(GET,"/inventary/sensors/"+bson.stringify+"/sensor").withSession("user" -> "")
      val r=f.controller.sensorPage(bson.stringify).apply(req)

      status(r) must equalTo(BAD_REQUEST)
      contentType(r) must beSome.which(_ == "text/html")
      val content = contentAsString(r)
      content must contain("<title>Inventaire des capteurs</title>")
      content must contain("Ce type de capteur n&#x27;existe pas")

      there was one(f.typeSensorController).doIfTypeSensorFound(org.mockito.Matchers.eq(bson))(any[Unit=>Future[Result]])(any[Unit=>Future[Result]])
    }

    "send 500 internal error if have mongoDB error when insert sensor" in new WithApplication{
      val f=fixture
      val data=Json.parse("""{"id":"Id","acquisition":"2015-04-22","send":"submit"}""")
      val futureMock=mock[Future[LastError]]
      val throwable=mock[Throwable]

      f.applyFoundFunction()
      f.sensorDaoMock.findOne(any[JsObject])(any[ExecutionContext]) returns future{None}
      f.sensorDaoMock.insert(any[Sensor],any[GetLastError])(any[ExecutionContext]) returns futureMock
      futureMock.map(any[LastError=>LastError])(any[ExecutionContext]) returns futureMock
      futureMock.recover(any[PartialFunction[Throwable,Result]])(any[ExecutionContext]) answers {value=>future{value.asInstanceOf[PartialFunction[Throwable,Result]](throwable)}}

      val req=FakeRequest(POST,"/inventary/sensors/"+bson.stringify+"/sensor").withJsonBody(data).withSession("user" -> "")
      val r=f.controller.sensorInsert(bson.stringify).apply(req)

      status(r) must equalTo(INTERNAL_SERVER_ERROR)

      there was one(f.typeSensorController).doIfTypeSensorFound(org.mockito.Matchers.eq(bson))(any[Unit=>Future[Result]])(any[Unit=>Future[Result]])
      there was one(f.sensorDaoMock).findOne(any[JsObject])(any[ExecutionContext])
      there was one(f.sensorDaoMock).insert(any[Sensor],any[GetLastError])(any[ExecutionContext])
      there was one(futureMock).map(any[LastError=>LastError])(any[ExecutionContext])
      there was one(futureMock).recover(any[PartialFunction[Throwable,Result]])(any[ExecutionContext])
    }

    "send redirect if user not would be continue to insert sensor" in new WithApplication{
      val f=fixture
      val data=Json.parse("""{"id":"Id","acquisition":"2015-04-22","send":"Envoyer"}""")
      val lastError=mock[LastError]

      f.applyFoundFunction()
      f.sensorDaoMock.findOne(any[JsObject])(any[ExecutionContext]) returns future{None}
      f.sensorDaoMock.insert(any[Sensor],any[GetLastError])(any[ExecutionContext]) returns future{lastError}

      val req=FakeRequest(POST,"/inventary/sensors/"+bson.stringify+"/sensor").withJsonBody(data).withSession("user" -> "")
      val r=f.controller.sensorInsert(bson.stringify).apply(req)

      status(r) must equalTo(SEE_OTHER)
      header("Location",r) must equalTo(Some("/inventary/sensors/"+bson.stringify))

      there was one(f.typeSensorController).doIfTypeSensorFound(org.mockito.Matchers.eq(bson))(any[Unit=>Future[Result]])(any[Unit=>Future[Result]])
      there was one(f.sensorDaoMock).findOne(any[JsObject])(any[ExecutionContext])
      there was one(f.sensorDaoMock).insert(any[Sensor],any[GetLastError])(any[ExecutionContext])
    }

    "send 200 Ok page after insert sensor" in new WithApplication{
      val f=fixture
      val data=Json.parse("""{"id":"Id","acquisition":"2015-04-22","expiration":"2015-04-22","firstUse":"2015-04-22","commentaire":"un com","send":"Envoyer et continuer"}""")
      val lastError=mock[LastError]

      f.applyFoundFunction()
      f.sensorDaoMock.findOne(any[JsObject])(any[ExecutionContext]) returns future{None}
      f.sensorDaoMock.insert(any[Sensor],any[GetLastError])(any[ExecutionContext]) returns future{lastError}

      val req=FakeRequest(POST,"/inventary/sensors/"+bson.stringify+"/sensor").withJsonBody(data).withSession("user" -> "")
      val r=f.controller.sensorInsert(bson.stringify).apply(req)

      status(r) must equalTo(OK)
      contentType(r) must beSome.which(_ == "text/html")
      val content=contentAsString(r)
      content must contain("<title>Inventaire des capteurs</title>")
      content must contain("<form action=\"/inventary/sensors/"+bson.stringify+"/sensor\" method=\"POST\" >")
      content must contain("<input type=\"text\" id=\"id\" name=\"id\" value=\"\" class=\"form-control\"/>")
      content must contain("<input type=\"date\" id=\"acquisition\" name=\"acquisition\" value=\"2015-04-22\" class=\"form-control\"/>")
      content must contain("<input type=\"date\" id=\"expiration\" name=\"expiration\" value=\"2015-04-22\" class=\"form-control\"/>")
      content must contain("<input type=\"date\" id=\"firstUse\" name=\"firstUse\" value=\"2015-04-22\" class=\"form-control\"/>")
      content must contain("<textarea id=\"commentaire\" name=\"commentaire\" class=\"form-control\">un com</textarea>")

      there was one(f.typeSensorController).doIfTypeSensorFound(org.mockito.Matchers.eq(bson))(any[Unit=>Future[Result]])(any[Unit=>Future[Result]])
      there was one(f.sensorDaoMock).findOne(any[JsObject])(any[ExecutionContext])
      there was one(f.sensorDaoMock).insert(any[Sensor],any[GetLastError])(any[ExecutionContext])
    }
  }

  "When method printFormWIthData is called, SensorManager" should{

    "send bad_request with the form if sensor type not exist" in new WithApplication{
      val f=fixture
      val func=mock[Sensor=>SensorForm]
      val routeCall=mock[Call]

      f.applyNotFoundFunction()

      val req=FakeRequest(GET,"/inventary/sensors/"+bson.stringify+"/"+bson2.stringify).withSession("user" -> "")
      val action=Action.async(implicit request => f.controller.printFormWithData(bson.stringify,bson2.stringify,routeCall)(func))
      val r=call(action,req)

      status(r) must equalTo(BAD_REQUEST)
      contentType(r) must beSome.which(_ == "text/html")
      val content = contentAsString(r)
      content must contain("<title>Inventaire des capteurs</title>")
      content must contain("Ce type de capteur n&#x27;existe pas")

      there was one(f.typeSensorController).doIfTypeSensorFound(org.mockito.Matchers.eq(bson))(any[Unit=>Future[Result]])(any[Unit=>Future[Result]])
    }

    "send 200 Ok page with a prefilled form" in new WithApplication{
      val f=fixture
      val func=mock[Sensor=>SensorForm]
      val sensor=Sensor(bson2,"Id",bson,Some(date),date,Some(date),false,None)
      val routeCall=mock[Call]

      f.applyFoundFunction()
      f.sensorDaoMock.findById(org.mockito.Matchers.eq(bson2))(any[ExecutionContext]) returns future{Some(sensor)}
      func.apply(org.mockito.Matchers.eq(sensor)) returns SensorForm("Id",date,Some(date),Some(date),false,Some("un com"),"")

      val req=FakeRequest(GET,"/inventary/sensors/"+bson.stringify+"/"+bson2.stringify).withSession("user" -> "")
      val action=Action.async(implicit request => f.controller.printFormWithData(bson.stringify,bson2.stringify,routeCall)(func))
      val r=call(action,req)

      status(r) must equalTo(OK)
      contentType(r) must beSome.which(_ == "text/html")
      val content = contentAsString(r)
      content must contain("<title>Inventaire des capteurs</title>")
      content must contain("<input type=\"text\" id=\"id\" name=\"id\" value=\"Id\" class=\"form-control\"/>")
      content must contain("<input type=\"date\" id=\"acquisition\" name=\"acquisition\" value=\"2015-04-22\" class=\"form-control\"/>")
      content must contain("<input type=\"date\" id=\"expiration\" name=\"expiration\" value=\"2015-04-22\" class=\"form-control\"/>")
      content must contain("<input type=\"date\" id=\"firstUse\" name=\"firstUse\" value=\"2015-04-22\" class=\"form-control\"/>")
      content must contain("<textarea id=\"commentaire\" name=\"commentaire\" class=\"form-control\">un com</textarea>")

      there was one(f.typeSensorController).doIfTypeSensorFound(org.mockito.Matchers.eq(bson))(any[Unit=>Future[Result]])(any[Unit=>Future[Result]])
      there was one(f.sensorDaoMock).findById(org.mockito.Matchers.eq(bson2))(any[ExecutionContext])
      there was one(func).apply(org.mockito.Matchers.eq(sensor))
    }

    "send redirect if sensor not found" in new WithApplication{
      val f=fixture
      val func=mock[Sensor=>SensorForm]
      val sensor=Sensor(bson2,"Id",bson,Some(date),date,Some(date),false,None)
      val routeCall=mock[Call]

      f.applyFoundFunction()
      f.sensorDaoMock.findById(org.mockito.Matchers.eq(bson2))(any[ExecutionContext]) returns future{None}

      val req=FakeRequest(GET,"/inventary/sensors/"+bson.stringify+"/sensor").withSession("user" -> "")
      val action=Action.async(implicit request => f.controller.printFormWithData(bson.stringify,bson2.stringify,routeCall)(func))
      val r=call(action,req)

      status(r) must equalTo(SEE_OTHER)
      header("Location",r) must equalTo(Some("/inventary/sensors/"+bson.stringify))

      there was one(f.typeSensorController).doIfTypeSensorFound(org.mockito.Matchers.eq(bson))(any[Unit=>Future[Result]])(any[Unit=>Future[Result]])
      there was one(f.sensorDaoMock).findById(org.mockito.Matchers.eq(bson2))(any[ExecutionContext])
    }

    "send 500 internal server error if have mongoDB error" in new WithApplication{
      val f=fixture
      val func=mock[Sensor=>SensorForm]
      val futureMock=mock[Future[Option[Sensor]]]
      val routeCall=mock[Call]
      val throwable=mock[Throwable]

      f.applyFoundFunction()
      f.sensorDaoMock.findById(org.mockito.Matchers.eq(bson2))(any[ExecutionContext]) returns futureMock
      futureMock.map(any[Option[Sensor]=>Option[Sensor]])(any[ExecutionContext]) returns futureMock
      futureMock.recover(any[PartialFunction[Throwable,Result]])(any[ExecutionContext]) answers {value=>future{value.asInstanceOf[PartialFunction[Throwable,Result]](throwable)}}

      val req=FakeRequest(GET,"/inventary/sensors/"+bson.stringify+"/sensor").withSession("user" -> "")
      val action=Action.async(implicit request => f.controller.printFormWithData(bson.stringify,bson2.stringify,routeCall)(func))
      val r=call(action,req)

      status(r) must equalTo(INTERNAL_SERVER_ERROR)

      there was one(f.typeSensorController).doIfTypeSensorFound(org.mockito.Matchers.eq(bson))(any[Unit=>Future[Result]])(any[Unit=>Future[Result]])
      there was one(f.sensorDaoMock).findById(org.mockito.Matchers.eq(bson2))(any[ExecutionContext])
      there was one(futureMock).map(any[Option[Sensor]=>Option[Sensor]])(any[ExecutionContext])
      there was one(futureMock).recover(any[PartialFunction[Throwable,Result]])(any[ExecutionContext])
    }
  }

  "When method update is called, SensorManager" should{
    "send 500 internal error if mongoDB error when update sensor" in new WithApplication {
      val f = fixture
      val futureMock=mock[Future[LastError]]
      val sensorInfo=mock[SensorInfo]
      val throwable=mock[Throwable]

      f.sensorDaoMock.updateById(org.mockito.Matchers.eq(bson2), any[Sensor],any[GetLastError])(any[Writes[Sensor]],any[ExecutionContext]) returns futureMock
      futureMock.map(any[LastError=>LastError])(any[ExecutionContext]) returns futureMock
      futureMock.recover(any[PartialFunction[Throwable,Result]])(any[ExecutionContext]) answers {value=>future{value.asInstanceOf[PartialFunction[Throwable,Result]](throwable)}}

      val req=FakeRequest(POST,"/inventary/sensors/"+bson.stringify+"/"+bson2.stringify).withSession("user" -> "")
      val action=Action.async{implicit request=>f.controller.update(bson.stringify,bson2.stringify,sensorInfo,false)}
      val r=call(action,req)


      status(r) must equalTo(INTERNAL_SERVER_ERROR)

      there was one(f.sensorDaoMock).updateById(org.mockito.Matchers.eq(bson2), any[Sensor],any[GetLastError])(any[Writes[Sensor]],any[ExecutionContext])
      there was one(futureMock).map(any[LastError=>LastError])(any[ExecutionContext])
      there was one(futureMock).recover(any[PartialFunction[Throwable,Result]])(any[ExecutionContext])
    }

    "send redirect after update sensor" in new WithApplication {
      val f = fixture
      val sensorInfo=Sensor(bson2,"Id",bson,Some(date),date,Some(date),false,None)
      val lastError=mock[LastError]

      f.sensorDaoMock.updateById(org.mockito.Matchers.eq(bson2), any[Sensor],any[GetLastError])(any[Writes[Sensor]],any[ExecutionContext]) returns future{lastError}

      val req=FakeRequest(POST,"/inventary/sensors/"+bson.stringify+"/"+bson2.stringify).withSession("user" -> "")
      val action=Action.async{implicit request=>f.controller.update(bson.stringify,bson2.stringify,sensorInfo,false)}
      val r=call(action,req)


      status(r) must equalTo(SEE_OTHER)
      header("Location", r) must equalTo(Some("/inventary/sensors/"+bson.stringify))

      there was one(f.sensorDaoMock).updateById(org.mockito.Matchers.eq(bson2), any[Sensor],any[GetLastError])(any[Writes[Sensor]],any[ExecutionContext])
    }
  }

  "When user submit a form, SensorManager" should{
    "send bad_request with the form if sensor type not exist" in new WithApplication{
      val f=fixture
      val func=mock[SensorForm=>Future[Result]]
      val funcVerif=mock[SensorForm=>JsObject]
      val routeCall=mock[Call]

      f.applyNotFoundFunction()

      val req=FakeRequest(POST,"/inventary/sensors/"+bson.stringify+"/sensor").withSession("user" -> "")
      val action=Action.async(implicit request => f.controller.submitForm(bson.stringify,routeCall)(funcVerif)(func))
      val r=call(action,req)

      status(r) must equalTo(BAD_REQUEST)
      contentType(r) must beSome.which(_ == "text/html")
      val content = contentAsString(r)
      content must contain("<title>Inventaire des capteurs</title>")
      content must contain("Ce type de capteur n&#x27;existe pas")

      there was one(f.typeSensorController).doIfTypeSensorFound(org.mockito.Matchers.eq(bson))(any[Unit=>Future[Result]])(any[Unit=>Future[Result]])
    }

    "send bad request when the form was submit with empty fields" in new WithApplication{
      val f=fixture
      val routeCall=mock[Call]
      val funcVerif=mock[SensorForm=>JsObject]
      val func=mock[SensorForm=>Future[Result]]

      f.applyFoundFunction()

      val req=FakeRequest(POST,"/inventary/sensors/"+bson.stringify+"/sensor").withSession("user" -> "")
      val action=Action.async(implicit request => f.controller.submitForm(bson.stringify,routeCall)(funcVerif)(func))
      val r=call(action,req)

      status(r) must equalTo(BAD_REQUEST)
      contentType(r) must beSome.which(_ == "text/html")
      val content = contentAsString(r)
      content must contain("<title>Inventaire des capteurs</title>")
      content must contains("<span class=\"errors\">This field is required</span>",2)
      content must not contain("<span class=\"errors\">Valid date required</span>")

      there was one(f.typeSensorController).doIfTypeSensorFound(org.mockito.Matchers.eq(bson))(any[Unit=>Future[Result]])(any[Unit=>Future[Result]])
    }

    "send bad request when the form was submit with not valid date" in new WithApplication{
      val f=fixture
      val routeCall=mock[Call]
      val funcVerif=mock[SensorForm=>JsObject]
      val func=mock[SensorForm=>Future[Result]]
      val data=Json.parse("""{"id":"Id","acquisition":"a","expiration":"a","firstUse":"a","send":"submit"}""")

      f.applyFoundFunction()

      val req=FakeRequest(POST,"/inventary/sensors/"+bson.stringify+"/sensor").withJsonBody(data).withSession("user" -> "")
      val action=Action.async(implicit request => f.controller.submitForm(bson.stringify,routeCall)(funcVerif)(func))
      val r=call(action,req)

      status(r) must equalTo(BAD_REQUEST)
      contentType(r) must beSome.which(_ == "text/html")
      val content = contentAsString(r)
      content must contain("<title>Inventaire des capteurs</title>")
      content must not contain("<span class=\"errors\">This field is required</span>")
      content must contains("<span class=\"errors\">Valid date required</span>",3)

      there was one(f.typeSensorController).doIfTypeSensorFound(org.mockito.Matchers.eq(bson))(any[Unit=>Future[Result]])(any[Unit=>Future[Result]])
    }

    "Verify if the acquisition date is before expiration date" in new WithApplication{
      val f=fixture
      val formMock=mock[Form[SensorForm]]
      val formMock2=mock[Form[SensorForm]]
      val sensorForm=SensorForm("Id",date2,Some(date),None,false,Some("un com"),"")

      formMock.withError(org.mockito.Matchers.eq("expiration"),org.mockito.Matchers.eq("La date d'expiration doit être supèrieur à la date d'acquisition"),any[Array[Any]]) returns formMock2

      val ret=f.controller.verifyErrorAcquisitionAfterExpiration(sensorForm,formMock)

      ret must equalTo(formMock2)

      there was one(formMock).withError(org.mockito.Matchers.eq("expiration"),org.mockito.Matchers.eq("La date d'expiration doit être supèrieur à la date d'acquisition"),any[Array[Any]])
    }

    "Verify if the acquisition date is before first use date" in new WithApplication{
      val f=fixture
      val formMock=mock[Form[SensorForm]]
      val formMock2=mock[Form[SensorForm]]
      val sensorForm=SensorForm("Id",date2,None,Some(date),false,Some("un com"),"")

      formMock.withError(org.mockito.Matchers.eq("firstUse"),org.mockito.Matchers.eq("La date de première utilisation doit être supèrieur à la date d'acquisition"),any[Array[Any]]) returns formMock2

      val ret=f.controller.verifyErrorAcquisitionAfterFirstUse(sensorForm,formMock)

      ret must equalTo(formMock2)

      there was one(formMock).withError(org.mockito.Matchers.eq("firstUse"),org.mockito.Matchers.eq("La date de première utilisation doit être supèrieur à la date d'acquisition"),any[Array[Any]])
    }

    "send bad request when the form was submit with date error" in new WithApplication{
      val f=fixture
      val routeCall=mock[Call]
      val funcVerif=mock[SensorForm=>JsObject]
      val func=mock[SensorForm=>Future[Result]]
      val data=Json.parse("""{"id":"Id","acquisition":"2015-04-23","expiration":"2015-04-22","firstUse":"2015-04-22","send":"submit"}""")

      f.applyFoundFunction()

      val req=FakeRequest(POST,"/inventary/sensors/"+bson.stringify+"/sensor").withJsonBody(data).withSession("user" -> "")
      val action=Action.async(implicit request => f.controller.submitForm(bson.stringify,routeCall)(funcVerif)(func))
      val r=call(action,req)

      status(r) must equalTo(BAD_REQUEST)
      contentType(r) must beSome.which(_ == "text/html")
      val content = contentAsString(r)
      content must contain("<title>Inventaire des capteurs</title>")
      content must not contain("<span class=\"errors\">This field is required</span>")
      content must contain("<span class=\"errors\">La date de première utilisation doit être supèrieur à la date d&#x27;acquisition</span>")
      content must contain("<span class=\"errors\">La date d&#x27;expiration doit être supèrieur à la date d&#x27;acquisition</span>")

      there was one(f.typeSensorController).doIfTypeSensorFound(org.mockito.Matchers.eq(bson))(any[Unit=>Future[Result]])(any[Unit=>Future[Result]])
    }

    "send 500 internal error if mongoDB error when find sensor" in new WithApplication{
      val f=fixture
      val routeCall=mock[Call]
      val funcVerif=mock[SensorForm=>JsObject]
      val func=mock[SensorForm=>Future[Result]]
      val data=Json.parse("""{"id":"Id","acquisition":"2015-04-22","send":"submit"}""")
      val futureMock=mock[Future[Option[Sensor]]]
      val throwable=mock[Throwable]

      f.applyFoundFunction()
      f.sensorDaoMock.findOne(any[JsObject])(any[ExecutionContext]) returns futureMock
      funcVerif.apply(any[SensorForm]) returns Json.obj()
      futureMock.flatMap(any[Option[Sensor]=>Future[Option[Sensor]]])(any[ExecutionContext]) returns futureMock
      futureMock.recover(any[PartialFunction[Throwable,Result]])(any[ExecutionContext]) answers (value=>future{value.asInstanceOf[PartialFunction[Throwable,Result]](throwable)})

      val req=FakeRequest(POST,"/inventary/sensors/"+bson.stringify+"/sensor").withJsonBody(data).withSession("user" -> "")
      val action=Action.async(implicit request => f.controller.submitForm(bson.stringify,routeCall)(funcVerif)(func))
      val r=call(action,req)

      status(r) must equalTo(INTERNAL_SERVER_ERROR)

      there was one(f.typeSensorController).doIfTypeSensorFound(org.mockito.Matchers.eq(bson))(any[Unit=>Future[Result]])(any[Unit=>Future[Result]])
      there was one(f.sensorDaoMock).findOne(any[JsObject])(any[ExecutionContext])
      there was one(funcVerif).apply(any[SensorForm])
      there was one(futureMock).flatMap(any[Option[Sensor]=>Future[Option[Sensor]]])(any[ExecutionContext])
      there was one(futureMock).recover(any[PartialFunction[Throwable,Result]])(any[ExecutionContext])
    }

    "send bad_request if sensor exist" in new WithApplication{
      val f=fixture
      val routeCall=mock[Call]
      val funcVerif=mock[SensorForm=>JsObject]
      val func=mock[SensorForm=>Future[Result]]
      val data=Json.parse("""{"id":"Id","acquisition":"2015-04-22","send":"submit"}""")
      val futureMock=mock[Future[Option[Sensor]]]
      val sensor=mock[Sensor]

      f.applyFoundFunction()
      f.sensorDaoMock.findOne(any[JsObject])(any[ExecutionContext]) returns future{Some(sensor)}
      funcVerif.apply(any[SensorForm]) returns Json.obj()

      val req=FakeRequest(POST,"/inventary/sensors/"+bson.stringify+"/sensor").withJsonBody(data).withSession("user" -> "")
      val action=Action.async(implicit request => f.controller.submitForm(bson.stringify,routeCall)(funcVerif)(func))
      val r=call(action,req)

      status(r) must equalTo(BAD_REQUEST)
      val content=contentAsString(r)
      content must contain("<div class=\"alert alert-danger\" role=\"alert\">Ce capteur existe déjà</div>")

      there was one(f.typeSensorController).doIfTypeSensorFound(org.mockito.Matchers.eq(bson))(any[Unit=>Future[Result]])(any[Unit=>Future[Result]])
      there was one(f.sensorDaoMock).findOne(any[JsObject])(any[ExecutionContext])
      there was one(funcVerif).apply(any[SensorForm])
    }

    "execute dedicated function if sensor not exist" in new WithApplication{
      val f=fixture
      val routeCall=mock[Call]
      val funcVerif=mock[SensorForm=>JsObject]
      val func=mock[SensorForm=>Future[Result]]
      val data=Json.parse("""{"id":"Id","acquisition":"2015-04-22","send":"submit"}""")
      val futureMock=mock[Future[Option[Sensor]]]

      f.applyFoundFunction()
      f.sensorDaoMock.findOne(any[JsObject])(any[ExecutionContext]) returns future{None}
      funcVerif.apply(any[SensorForm]) returns Json.obj()
      func.apply(any[SensorForm]) returns future{Results.Ok("function executed")}

      val req=FakeRequest(POST,"/inventary/sensors/"+bson.stringify+"/sensor").withJsonBody(data).withSession("user" -> "")
      val action=Action.async(implicit request => f.controller.submitForm(bson.stringify,routeCall)(funcVerif)(func))
      val r=call(action,req)


      status(r) must equalTo(OK)
      val content=contentAsString(r)
      content must equalTo("function executed")

      there was one(f.typeSensorController).doIfTypeSensorFound(org.mockito.Matchers.eq(bson))(any[Unit=>Future[Result]])(any[Unit=>Future[Result]])
      there was one(f.sensorDaoMock).findOne(any[JsObject])(any[ExecutionContext])
      there was one(funcVerif).apply(any[SensorForm])
      there was one(func).apply(any[SensorForm])
    }
  }

  "When user is on the page /inventary/sensors/:id/:id2, SensorManager" should{
    "send 200 Ok page with a prefilled form" in new WithApplication{
      val f=fixture
      val sensor=Sensor(bson2,"Id",bson,Some(date),date,Some(date),false,Some("un com"))

      f.applyFoundFunction()
      f.sensorDaoMock.findById(org.mockito.Matchers.eq(bson2))(any[ExecutionContext]) returns future{Some(sensor)}

      val req=FakeRequest(GET,"/inventary/sensors/"+bson.stringify+"/"+bson2.stringify).withSession("user" -> "")
      val r=f.controller.sensorUpdatePage(bson.stringify,bson2.stringify).apply(req)

      status(r) must equalTo(OK)
      contentType(r) must beSome.which(_ == "text/html")
      val content = contentAsString(r)
      content must contain("<title>Inventaire des capteurs</title>")
      content must contain("<form action=\"/inventary/sensors/"+bson.stringify+"/"+bson2.stringify+"\" method=\"POST\" >")
      content must contain("<input type=\"text\" id=\"id\" name=\"id\" value=\"Id\" class=\"form-control\"/>")
      content must contain("<input type=\"date\" id=\"acquisition\" name=\"acquisition\" value=\"2015-04-22\" class=\"form-control\"/>")
      content must contain("<input type=\"date\" id=\"expiration\" name=\"expiration\" value=\"2015-04-22\" class=\"form-control\"/>")
      content must contain("<input type=\"date\" id=\"firstUse\" name=\"firstUse\" value=\"2015-04-22\" class=\"form-control\"/>")
      content must contain("<textarea id=\"commentaire\" name=\"commentaire\" class=\"form-control\">un com</textarea>")

      there was one(f.typeSensorController).doIfTypeSensorFound(org.mockito.Matchers.eq(bson))(any[Unit=>Future[Result]])(any[Unit=>Future[Result]])
      there was one(f.sensorDaoMock).findById(org.mockito.Matchers.eq(bson2))(any[ExecutionContext])
    }

    "send redirect after update sensor" in new WithApplication{
      val f=fixture
      val data=Json.parse("""{"id":"Id","acquisition":"2015-04-22","expiration":"2015-04-22","firstUse":"2015-04-22","commentaire":"un com","send":"Envoyer"}""")
      val sensor=Sensor(bson2,"Id",bson,Some(date),date,Some(date),false,Some("un com"))
      val lastError=mock[LastError]

      f.applyFoundFunction()
      f.sensorDaoMock.findOne(any[JsObject])(any[ExecutionContext]) returns future{None}
      f.sensorDaoMock.updateById(org.mockito.Matchers.eq(bson2), org.mockito.Matchers.eq(sensor),any[GetLastError])(any[Writes[Sensor]],any[ExecutionContext]) returns future{lastError}

      val req=FakeRequest(POST,"/inventary/sensors/"+bson.stringify+"/sensor").withJsonBody(data).withSession("user" -> "")
      val r=f.controller.sensorUpdate(bson.stringify,bson2.stringify).apply(req)


      status(r) must equalTo(SEE_OTHER)
      header("Location",r) must equalTo(Some("/inventary/sensors/"+bson.stringify))

      there was one(f.typeSensorController).doIfTypeSensorFound(org.mockito.Matchers.eq(bson))(any[Unit=>Future[Result]])(any[Unit=>Future[Result]])
      there was one(f.sensorDaoMock).findOne(any[JsObject])(any[ExecutionContext])
      there was one(f.sensorDaoMock).updateById(org.mockito.Matchers.eq(bson2), org.mockito.Matchers.eq(sensor),any[GetLastError])(any[Writes[Sensor]],any[ExecutionContext])
    }
  }

  "When user is on the page /inventary/sensors/:id/:id2/clone, SensorManager" should{
    "send 200 Ok page with a prefilled form" in new WithApplication{
      val f=fixture
      val sensor=Sensor(bson2,"Id",bson,Some(date),date,Some(date),false,Some("un com"))

      f.applyFoundFunction()
      f.sensorDaoMock.findById(org.mockito.Matchers.eq(bson2))(any[ExecutionContext]) returns future{Some(sensor)}

      val req=FakeRequest(GET,"/inventary/sensors/"+bson.stringify+"/"+bson2.stringify+"/clone").withSession("user" -> "")
      val r=f.controller.sensorClonePage(bson.stringify,bson2.stringify).apply(req)


      status(r) must equalTo(OK)
      contentType(r) must beSome.which(_ == "text/html")
      val content = contentAsString(r)
      content must contain("<title>Inventaire des capteurs</title>")
      content must contain("<form action=\"/inventary/sensors/"+bson.stringify+"/sensor\" method=\"POST\" >")
      content must contain("<input type=\"text\" id=\"id\" name=\"id\" value=\"\" class=\"form-control\"/>")
      content must contain("<input type=\"date\" id=\"acquisition\" name=\"acquisition\" value=\"2015-04-22\" class=\"form-control\"/>")
      content must contain("<input type=\"date\" id=\"expiration\" name=\"expiration\" value=\"2015-04-22\" class=\"form-control\"/>")
      content must contain("<input type=\"date\" id=\"firstUse\" name=\"firstUse\" value=\"2015-04-22\" class=\"form-control\"/>")
      content must contain("<textarea id=\"commentaire\" name=\"commentaire\" class=\"form-control\">un com</textarea>")

      there was one(f.typeSensorController).doIfTypeSensorFound(org.mockito.Matchers.eq(bson))(any[Unit=>Future[Result]])(any[Unit=>Future[Result]])
      there was one(f.sensorDaoMock).findById(org.mockito.Matchers.eq(bson2))(any[ExecutionContext])
    }
  }

  "When user is on the page /inventary/sensors/:id/:id2/delete, SensorManager" should{
    "send bad_request with the form if sensor type not exist" in new WithApplication{
      val f=fixture

      f.applyNotFoundFunction()

      val req=FakeRequest(GET,"/inventary/sensors/"+bson.stringify+"/"+bson2.stringify+"/delete").withSession("user" -> "")
      val r=f.controller.delete(bson.stringify,bson2.stringify).apply(req)

      status(r) must equalTo(SEE_OTHER)
      header("Location",r) must equalTo(Some("/inventary/sensors"))

      there was one(f.typeSensorController).doIfTypeSensorFound(org.mockito.Matchers.eq(bson))(any[Unit=>Future[Result]])(any[Unit=>Future[Result]])
    }

    "send 500 internal error if mongoDB error" in new WithApplication{
      val f=fixture
      val futureMock=mock[Future[Option[Sensor]]]
      val throwable=mock[Throwable]

      f.applyFoundFunction()
      f.sensorDaoMock.findOne(any[JsObject])(any[ExecutionContext]) returns futureMock
      futureMock.flatMap(any[Option[Sensor]=>Future[Option[Sensor]]])(any[ExecutionContext]) returns futureMock
      futureMock.recover(any[PartialFunction[Throwable,Result]])(any[ExecutionContext]) answers {value=>future{value.asInstanceOf[PartialFunction[Throwable,Result]](throwable)}}

      val req=FakeRequest(GET,"/inventary/sensors/"+bson.stringify+"/"+bson2.stringify+"/delete").withSession("user" -> "")
      val r=f.controller.delete(bson.stringify,bson2.stringify).apply(req)

      status(r) must equalTo(INTERNAL_SERVER_ERROR)

      there was one(f.typeSensorController).doIfTypeSensorFound(org.mockito.Matchers.eq(bson))(any[Unit=>Future[Result]])(any[Unit=>Future[Result]])
      there was one(f.sensorDaoMock).findOne(any[JsObject])(any[ExecutionContext])
      there was one(futureMock).flatMap(any[Option[Sensor]=>Future[Option[Sensor]]])(any[ExecutionContext])
      there was one(futureMock).recover(any[PartialFunction[Throwable,Result]])(any[ExecutionContext])
    }

    "send redirect if sensor not found" in new WithApplication{
      val f=fixture

      f.applyFoundFunction()
      f.sensorDaoMock.findOne(any[JsObject])(any[ExecutionContext]) returns future{None}

      val req=FakeRequest(GET,"/inventary/sensors/"+bson.stringify+"/"+bson2.stringify+"/delete").withSession("user" -> "")
      val r=f.controller.delete(bson.stringify,bson2.stringify).apply(req)

      status(r) must equalTo(SEE_OTHER)
      header("Location",r) must equalTo(Some("/inventary/sensors/"+bson.stringify))

      there was one(f.typeSensorController).doIfTypeSensorFound(org.mockito.Matchers.eq(bson))(any[Unit=>Future[Result]])(any[Unit=>Future[Result]])
      there was one(f.sensorDaoMock).findOne(any[JsObject])(any[ExecutionContext])
    }

    "send redirect after delete sensor" in new WithApplication{
      val f=fixture
      val sensorIn=Sensor(bson2,"Id",bson,Some(date),date,Some(date),false,Some("un com"),false)
      val sensorOut=Sensor(bson2,"Id",bson,Some(date),date,Some(date),false,Some("un com"),true)
      val lastError=mock[LastError]

      f.applyFoundFunction()
      f.sensorDaoMock.findOne(any[JsObject])(any[ExecutionContext]) returns future{Some(sensorIn)}
      f.sensorDaoMock.updateById(org.mockito.Matchers.eq(bson2), org.mockito.Matchers.eq(sensorOut),any[GetLastError])(any[Writes[Sensor]],any[ExecutionContext]) returns future{lastError}

      val req=FakeRequest(GET,"/inventary/sensors/"+bson.stringify+"/"+bson2.stringify+"/delete").withSession("user" -> "")
      val r=f.controller.delete(bson.stringify,bson2.stringify).apply(req)

      status(r) must equalTo(SEE_OTHER)
      header("Location",r) must equalTo(Some("/inventary/sensors/"+bson.stringify))

      there was one(f.typeSensorController).doIfTypeSensorFound(org.mockito.Matchers.eq(bson))(any[Unit=>Future[Result]])(any[Unit=>Future[Result]])
      there was one(f.sensorDaoMock).findOne(any[JsObject])(any[ExecutionContext])
      there was one(f.sensorDaoMock).updateById(org.mockito.Matchers.eq(bson2), org.mockito.Matchers.eq(sensorOut),any[GetLastError])(any[Writes[Sensor]],any[ExecutionContext])
    }
  }
}