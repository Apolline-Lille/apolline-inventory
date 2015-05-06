
import controllers.{TypeModuleForm, TypeModuleManager, TypeModuleManagerLike}
import models.{TypeModule, TypeModuleDao}
import org.junit.runner.RunWith
import org.specs2.matcher.{MatchResult, Expectable, Matcher}
import org.specs2.mock.Mockito
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner
import play.api.libs.json.{Json, JsObject}
import play.api.mvc.{Result, Action, Results, Call}
import play.api.test.Helpers._
import play.api.test.{FakeRequest, WithApplication}
import reactivemongo.bson.{BSONObjectID, BSONDocument}
import reactivemongo.core.commands.{GetLastError, LastError}
import scala.concurrent._

@RunWith(classOf[JUnitRunner])
class TypeModuleManagerSpec extends Specification with Mockito {

  class TypeModuleManagerTest extends TypeModuleManagerLike

  case class matchRegex(a: String) extends Matcher[String]() {
    override def apply[S <: String](t: Expectable[S]): MatchResult[S] = result(a.r.findFirstIn(t.value).nonEmpty, "okMessage", "not found " + a, t)
  }

  case class contains(a: String, b: Int) extends Matcher[String]() {
    override def apply[S <: String](t: Expectable[S]): MatchResult[S] = result(a.r.findAllMatchIn(t.value).size == b, "okMessage", "not found " + b + " times but " + a.r.findAllMatchIn(t.value).size + " times " + a, t)
  }

  val bson=BSONObjectID.generate

  def fixture = new {
    val typeModuleDaoMock = mock[TypeModuleDao]
    val controller = new TypeModuleManagerTest {
      override val typeModuleDao: TypeModuleDao = typeModuleDaoMock
    }
  }

  "When user is not connected, TypeModuleManager" should {
    "redirect to login for resource /inventary/modules" in new WithApplication {
      route(FakeRequest(GET, "/inventary/modules")).map(
        r=>{
          status(r) must equalTo(SEE_OTHER)
          header("Location",r) must equalTo(Some("/login"))
        }
      ).getOrElse(
        failure("Pas de retour de la fonction")
      )
    }

    "redirect to login for GET resource /inventary/modules/type" in new WithApplication{
      route(FakeRequest(GET, "/inventary/modules/type")).map(
        r=>{
          status(r) must equalTo(SEE_OTHER)
          header("Location",r) must equalTo(Some("/login"))
        }
      ).getOrElse(
          failure("Pas de retour de la fonction")
        )
    }

    "redirect to login for POST resource /inventary/modules/type" in new WithApplication{
      route(FakeRequest(POST, "/inventary/modules/type")).map(
        r=>{
          status(r) must equalTo(SEE_OTHER)
          header("Location",r) must equalTo(Some("/login"))
        }
      ).getOrElse(
          failure("Pas de retour de la fonction")
        )
    }

    "redirect to login for GET resource /inventary/modules/:id/update" in new WithApplication{
      route(FakeRequest(GET, "/inventary/modules/"+bson.stringify+"/update")).map(
        r=>{
          status(r) must equalTo(SEE_OTHER)
          header("Location",r) must equalTo(Some("/login"))
        }
      ).getOrElse(
          failure("Pas de retour de la fonction")
        )
    }
  }

  "When user is on the resource /inventary/modules , TypeModuleManager" should {
    "send 200 on OK with the message 'Aucun résultat trouvé'" in new WithApplication {
      val f=fixture

      f.typeModuleDaoMock.findAll(any[JsObject], any[JsObject])(any[ExecutionContext]) returns future{List[TypeModule]()}
      f.typeModuleDaoMock.findListType() returns future{Stream[BSONDocument]()}

      val r = f.controller.inventary().apply(FakeRequest(GET, "/inventary/modules").withSession("user" -> """{"login":"test"}"""))
      status(r) must equalTo(OK)
      contentType(r) must beSome.which(_ == "text/html")
      val content = contentAsString(r)
      content must contain("<title>Inventaire des modules</title>")
      content must contain("<h3 style=\"text-align:center\">Aucun résultat trouvé</h3>")

      there was one(f.typeModuleDaoMock).findAll(any[JsObject], any[JsObject])(any[ExecutionContext])
      there was one(f.typeModuleDaoMock).findListType()
    }

    "send 200 on OK with 1 result" in new WithApplication {
      val f=fixture

      f.typeModuleDaoMock.findAll(any[JsObject], any[JsObject])(any[ExecutionContext]) returns future{List[TypeModule](TypeModule(bson,"mod","type"))}
      f.typeModuleDaoMock.findListType() returns future{Stream[BSONDocument]()}

      val r = f.controller.inventary().apply(FakeRequest(GET, "/inventary/modules").withSession("user" -> """{"login":"test"}"""))
      status(r) must equalTo(OK)
      contentType(r) must beSome.which(_ == "text/html")
      val content = contentAsString(r)
      content must contain("<title>Inventaire des modules</title>")
      content must not contain("<h3 style=\"text-align:center\">Aucun résultat trouvé</h3>")
      content must matchRegex("type\\s*/\\s*mod")
      content must matchRegex("<span class=\"bold\">\\s*Stocks\\s*</span>\\s*:\\s*0")

      there was one(f.typeModuleDaoMock).findAll(any[JsObject], any[JsObject])(any[ExecutionContext])
      there was one(f.typeModuleDaoMock).findListType()
    }

    "send 200 on OK with 2 results" in new WithApplication {
      val f=fixture

      f.typeModuleDaoMock.findAll(any[JsObject], any[JsObject])(any[ExecutionContext]) returns future{List[TypeModule](
        TypeModule(bson,"mod","type"),
        TypeModule(bson,"mod2","type2")
      )}
      f.typeModuleDaoMock.findListType() returns future{Stream[BSONDocument]()}

      val r = f.controller.inventary().apply(FakeRequest(GET, "/inventary/modules").withSession("user" -> """{"login":"test"}"""))

      status(r) must equalTo(OK)
      contentType(r) must beSome.which(_ == "text/html")
      val content = contentAsString(r)
      content must contain("<title>Inventaire des modules</title>")
      content must not contain("<h3 style=\"text-align:center\">Aucun résultat trouvé</h3>")
      content must matchRegex("type\\s*/\\s*mod")
      content must matchRegex("<span class=\"bold\">\\s*Stocks\\s*</span>\\s*:\\s*0")

      content must matchRegex("type2\\s*/\\s*mod2")

      there was one(f.typeModuleDaoMock).findAll(any[JsObject], any[JsObject])(any[ExecutionContext])
      there was one(f.typeModuleDaoMock).findListType()
    }

    "send 500 internal error if mongoDB error when find all module type" in new WithApplication{
      val f=fixture
      val futureMock=mock[Future[List[TypeModule]]]
      val throwable=mock[Throwable]

      f.typeModuleDaoMock.findAll(any[JsObject], any[JsObject])(any[ExecutionContext]) returns futureMock
      futureMock.flatMap(any[List[TypeModule]=>Future[List[TypeModule]]])(any[ExecutionContext]) returns futureMock
      futureMock.recover(any[PartialFunction[Throwable,Result]])(any[ExecutionContext]) answers {value=>future{value.asInstanceOf[PartialFunction[Throwable,Result]](throwable)}}
      f.typeModuleDaoMock.findListType() returns future{Stream[BSONDocument]()}

      val r = f.controller.inventary().apply(FakeRequest(GET, "/inventary/modules").withSession("user" -> """{"login":"test"}"""))

      status(r) must equalTo(INTERNAL_SERVER_ERROR)

      there was one(f.typeModuleDaoMock).findAll(any[JsObject], any[JsObject])(any[ExecutionContext])
      there was one(futureMock).flatMap(any[List[TypeModule]=>Future[List[TypeModule]]])(any[ExecutionContext])
      there was one(futureMock).recover(any[PartialFunction[Throwable,Result]])(any[ExecutionContext])
      there was one(f.typeModuleDaoMock).findListType()
    }

    "send 500 internal error if mongoDB error when find all module type name" in new WithApplication{
      val f=fixture
      val futureMock=mock[Future[Stream[BSONDocument]]]
      val throwable=mock[Throwable]

      f.typeModuleDaoMock.findAll(any[JsObject], any[JsObject])(any[ExecutionContext]) returns future{List[TypeModule]()}
      f.typeModuleDaoMock.findListType() returns futureMock
      futureMock.map(any[Stream[BSONDocument]=>Stream[BSONDocument]])(any[ExecutionContext]) returns futureMock
      futureMock.recover(any[PartialFunction[Throwable,Result]])(any[ExecutionContext]) answers {value=>future{value.asInstanceOf[PartialFunction[Throwable,Result]](throwable)}}

      val r = f.controller.inventary().apply(FakeRequest(GET, "/inventary/modules").withSession("user" -> """{"login":"test"}"""))

      status(r) must equalTo(INTERNAL_SERVER_ERROR)

      there was one(f.typeModuleDaoMock).findAll(any[JsObject], any[JsObject])(any[ExecutionContext])
      there was one(futureMock).map(any[Stream[BSONDocument]=>Stream[BSONDocument]])(any[ExecutionContext])
      there was one(futureMock).recover(any[PartialFunction[Throwable,Result]])(any[ExecutionContext])
      there was one(f.typeModuleDaoMock).findListType()
    }
  }

  "When method printForm is called, TypeModuleManager" should{
    "print an empty form" in new WithApplication{
      val f=fixture

      f.typeModuleDaoMock.findListModele() returns future{Stream[BSONDocument]()}
      f.typeModuleDaoMock.findListType() returns future{Stream[BSONDocument]()}

      val req=FakeRequest(GET, "/inventary/modules/type").withSession("user" -> """{"login":"test"}""")
      val action = Action.async{implicit request=>f.controller.printForm(Results.Ok,TypeModuleManager.form,mock[Call])}
      val r=call(action,req)


      status(r) must equalTo(OK)
      contentType(r) must beSome.which(_ == "text/html")
      val content = contentAsString(r)
      content must contain("<title>Inventaire des modules</title>")
      content must contain("<input id=\"modele\" name=\"modele\" class=\"form-control\" list=\"list_modele\" type=\"text\" autocomplete=\"off\" value=\"\"/>")
      content must contain("<input id=\"types\" name=\"types\" class=\"form-control\" list=\"list_type\" type=\"text\" autocomplete=\"off\" value=\"\"/>")

      there was one(f.typeModuleDaoMock).findListModele()
      there was one(f.typeModuleDaoMock).findListType()
    }

    "send Internal Error if mongoDB error when find list of modele" in new WithApplication{
      val f=fixture
      val futureMock=mock[Future[Stream[BSONDocument]]]
      val throwable=mock[Throwable]

      f.typeModuleDaoMock.findListModele() returns futureMock
      futureMock.flatMap(any[Stream[BSONDocument]=>Future[Stream[BSONDocument]]])(any[ExecutionContext]) returns futureMock
      futureMock.recover(any[PartialFunction[Throwable,Result]])(any[ExecutionContext]) answers {value=>future{value.asInstanceOf[PartialFunction[Throwable,Result]](throwable)}}

      val req=FakeRequest(GET, "/inventary/modules/type").withSession("user" -> """{"login":"test"}""")
      val action = Action.async{implicit request=>f.controller.printForm(Results.Ok,TypeModuleManager.form,mock[Call])}
      val r=call(action,req)

      status(r) must equalTo(INTERNAL_SERVER_ERROR)

      there was one(f.typeModuleDaoMock).findListModele()
      there was one(futureMock).flatMap(any[Stream[BSONDocument]=>Future[Stream[BSONDocument]]])(any[ExecutionContext])
      there was one(futureMock).recover(any[PartialFunction[Throwable,Result]])(any[ExecutionContext])
    }

    "send Internal Error if mongoDB error when find list of type" in new WithApplication{
      val f=fixture
      val futureMock=mock[Future[Stream[BSONDocument]]]
      val throwable=mock[Throwable]

      f.typeModuleDaoMock.findListModele() returns future{Stream[BSONDocument]()}
      f.typeModuleDaoMock.findListType() returns futureMock
      futureMock.map(any[Stream[BSONDocument]=>Stream[BSONDocument]])(any[ExecutionContext]) returns futureMock
      futureMock.recover(any[PartialFunction[Throwable,Result]])(any[ExecutionContext]) answers {value=>future{value.asInstanceOf[PartialFunction[Throwable,Result]](throwable)}}
      val req=FakeRequest(GET, "/inventary/modules/type").withSession("user" -> """{"login":"test"}""")
      val action = Action.async{implicit request=>f.controller.printForm(Results.Ok,TypeModuleManager.form,mock[Call])}
      val r=call(action,req)

      status(r) must equalTo(INTERNAL_SERVER_ERROR)

      there was one(f.typeModuleDaoMock).findListModele()
      there was one(f.typeModuleDaoMock).findListType()
      there was one(futureMock).map(any[Stream[BSONDocument]=>Stream[BSONDocument]])(any[ExecutionContext])
      there was one(futureMock).recover(any[PartialFunction[Throwable,Result]])(any[ExecutionContext])
    }
  }

  "When user submit a form, TypeModuleManager" should {
    "send bad_request with form and empty input" in new WithApplication {
      val route=mock[Call]
      val function=mock[TypeModuleForm=>Future[Result]]
      val function2=mock[TypeModuleForm=>JsObject]
      val f=fixture

      f.typeModuleDaoMock.findListModele() returns future{Stream[BSONDocument]()}
      f.typeModuleDaoMock.findListType() returns future{Stream[BSONDocument]()}

      val r = f.controller.submitForm(route)(function2)(function)(FakeRequest(POST, "url").withSession("user" -> """{"login":"test"}"""))

      status(r) must equalTo(BAD_REQUEST)
      contentType(r) must beSome.which(_ == "text/html")
      val content = contentAsString(r)
      content must contain("<title>Inventaire des modules</title>")
      content must contains("<span class=\"control-label errors\">This field is required</span>", 2)

      there was one(f.typeModuleDaoMock).findListModele()
      there was one(f.typeModuleDaoMock).findListType()
    }

    "send Bad_Request for existing type" in new WithApplication {
      val formData = Json.parse("""{"modele":"mod","types":"typ"}""")
      val route=mock[Call]
      val function=mock[TypeModuleForm=>Future[Result]]
      val function2=mock[TypeModuleForm=>JsObject]
      val f=fixture
      val typeModule=mock[TypeModule]

      function2.apply(any[TypeModuleForm]) returns Json.obj()
      f.typeModuleDaoMock.findOne(any[JsObject])(any[ExecutionContext]) returns future{Some(typeModule)}
      f.typeModuleDaoMock.findListModele() returns future{Stream[BSONDocument]()}
      f.typeModuleDaoMock.findListType() returns future{Stream[BSONDocument]()}

      val r = f.controller.submitForm(route)(function2)(function)(FakeRequest(POST, "url").withJsonBody(formData).withSession("user" -> """{"login":"test"}"""))

      status(r) must equalTo(BAD_REQUEST)
      contentType(r) must beSome.which(_ == "text/html")
      val content = contentAsString(r)
      content must contain("<div class=\"alert alert-danger\" role=\"alert\">Ce type de module existe déjà</div>")

      there was one(f.typeModuleDaoMock).findOne(any[JsObject])(any[ExecutionContext])
      there was one(function2).apply(any[TypeModuleForm])
      there was one(f.typeModuleDaoMock).findListModele()
      there was one(f.typeModuleDaoMock).findListType()
    }

    "send 500 Internal error for mongoDB error" in new WithApplication {
      val formData = Json.parse("""{"modele":"mod","types":"typ"}""")
      val route=mock[Call]
      val function=mock[TypeModuleForm=>Future[Result]]
      val function2=mock[TypeModuleForm=>JsObject]
      val futureMock=mock[Future[Option[TypeModule]]]
      val fix=fixture
      val throwable=mock[Throwable]

      function2.apply(any[TypeModuleForm]) returns Json.obj()
      fix.typeModuleDaoMock.findOne(any[JsObject])(any[ExecutionContext]) returns futureMock
      futureMock.flatMap(any[Option[TypeModule]=>Future[Option[TypeModule]]])(any[ExecutionContext]) returns futureMock
      futureMock.recover(any[PartialFunction[Throwable,Result]])(any[ExecutionContext]) answers (vals => future{vals.asInstanceOf[PartialFunction[Throwable,Result]](throwable)})


      val r = fix.controller.submitForm(route)(function2)(function)(FakeRequest(POST, "url").withJsonBody(formData).withSession("user" -> """{"login":"test"}"""))


      status(r) must equalTo(INTERNAL_SERVER_ERROR)

      there was one(function2).apply(any[TypeModuleForm])
      there was one(fix.typeModuleDaoMock).findOne(any[JsObject])(any[ExecutionContext])
      there was one(futureMock).flatMap(any[Option[TypeModule]=>Future[Option[TypeModule]]])(any[ExecutionContext])
      there was one(futureMock).recover(any[PartialFunction[Throwable,Result]])(any[ExecutionContext])
    }
  }

  "When user is on the resource /inventary/modules/type , TypeModuleManager" should {
    "send 200 on OK with an empty form" in new WithApplication {
      val f=fixture

      f.typeModuleDaoMock.findListModele() returns future{Stream[BSONDocument]()}
      f.typeModuleDaoMock.findListType() returns future{Stream[BSONDocument]()}

      val r = f.controller.typePage.apply(FakeRequest(GET, "/inventary/modules/type").withSession("user" -> """{"login":"test"}"""))

      status(r) must equalTo(OK)
      contentType(r) must beSome.which(_ == "text/html")
      val content = contentAsString(r)
      content must contain("<title>Inventaire des modules</title>")
      content must contain("<input id=\"modele\" name=\"modele\" class=\"form-control\" list=\"list_modele\" type=\"text\" autocomplete=\"off\" value=\"\"/>")
      content must contain("<input id=\"types\" name=\"types\" class=\"form-control\" list=\"list_type\" type=\"text\" autocomplete=\"off\" value=\"\"/>")

      there was one(f.typeModuleDaoMock).findListModele()
      there was one(f.typeModuleDaoMock).findListType()
    }

    "send redirect after type module" in new WithApplication {
      val formData = Json.parse("""{"modele":"mod","types":"typ"}""")
      val lastError=mock[LastError]
      val f=fixture

      f.typeModuleDaoMock.findOne(any[JsObject])(any[ExecutionContext]) returns future{None}
      f.typeModuleDaoMock.insert(any[TypeModule],any[GetLastError])(any[ExecutionContext]) returns future{lastError}

      val r = f.controller.typeInsert.apply(FakeRequest(POST, "/inventary/modules/type").withJsonBody(formData).withSession("user" -> """{"login":"test"}"""))

      status(r) must equalTo(SEE_OTHER)
      header("Location",r) must beSome("/inventary/modules")

      there was one(f.typeModuleDaoMock).findOne(any[JsObject])(any[ExecutionContext])
      there was one(f.typeModuleDaoMock).insert(any[TypeModule],any[GetLastError])(any[ExecutionContext])
    }

    "send internal server error if mongoDB error when insert module type" in new WithApplication {
      val formData = Json.parse("""{"modele":"mod","types":"typ"}""")
      val lastError=mock[Future[LastError]]
      val f=fixture
      val throwable=mock[Throwable]

      f.typeModuleDaoMock.findOne(any[JsObject])(any[ExecutionContext]) returns future{None}
      f.typeModuleDaoMock.insert(any[TypeModule],any[GetLastError])(any[ExecutionContext]) returns lastError
      lastError.map(any[LastError=>LastError])(any[ExecutionContext]) returns lastError
      lastError.recover(any[PartialFunction[Throwable,Result]])(any[ExecutionContext]) answers {value => future{value.asInstanceOf[PartialFunction[Throwable,Result]](throwable)}}

      val r = f.controller.typeInsert.apply(FakeRequest(POST, "/inventary/modules/type").withJsonBody(formData).withSession("user" -> """{"login":"test"}"""))

      status(r) must equalTo(INTERNAL_SERVER_ERROR)

      there was one(f.typeModuleDaoMock).findOne(any[JsObject])(any[ExecutionContext])
      there was one(f.typeModuleDaoMock).insert(any[TypeModule],any[GetLastError])(any[ExecutionContext])
      there was one(lastError).map(any[LastError=>LastError])(any[ExecutionContext])
      there was one(lastError).recover(any[PartialFunction[Throwable,Result]])(any[ExecutionContext])
    }
  }

  "When user is on the resource /inventary/modules/:id/update , TypeModuleManager" should {
    "send 200 on OK with a form" in new WithApplication {
      val typeModule = Some(TypeModule(bson, "mod", "typ"))

      val fix = fixture

      fix.typeModuleDaoMock.findById(any[BSONObjectID])(any[ExecutionContext]) returns future{typeModule}
      fix.typeModuleDaoMock.findListModele() returns future{Stream[BSONDocument]()}
      fix.typeModuleDaoMock.findListType() returns future{Stream[BSONDocument]()}

      val r = fix.controller.typeUpdatePage(bson.stringify).apply(FakeRequest(GET, "/inventary/modules/" + bson.stringify + "/update").withSession("user" -> """{"login":"test"}"""))

      status(r) must equalTo(OK)
      contentType(r) must beSome.which(_ == "text/html")
      val content = contentAsString(r)
      content must contain("<title>Inventaire des modules</title>")
      content must contain("<input id=\"modele\" name=\"modele\" class=\"form-control\" list=\"list_modele\" type=\"text\" autocomplete=\"off\" value=\"mod\"/>")
      content must contain("<input id=\"types\" name=\"types\" class=\"form-control\" list=\"list_type\" type=\"text\" autocomplete=\"off\" value=\"typ\"/>")

      there was one(fix.typeModuleDaoMock).findById(any[BSONObjectID])(any[ExecutionContext])
      there was one(fix.typeModuleDaoMock).findListModele()
      there was one(fix.typeModuleDaoMock).findListType()
    }
  }
}