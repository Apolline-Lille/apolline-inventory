import java.io.File
import java.util.UUID

import controllers._
import models._
import org.junit.runner.RunWith
import org.mockito.ArgumentCaptor
import org.specs2.matcher.{MatchResult, Expectable, Matcher}
import org.specs2.mock.Mockito
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner
import play.api.libs.Files.TemporaryFile
import play.api.libs.json.{JsObject, Writes, Json}
import play.api.mvc.BodyParsers.parse
import play.api.mvc.{MultipartFormData, Results, Action, Result}
import play.api.test.Helpers._
import play.api.test.{FakeHeaders, FakeRequest, WithApplication}
import reactivemongo.bson.BSONObjectID
import reactivemongo.extensions.BSONFormats.BSONObjectIDFormat
import reactivemongo.core.commands.{LastError, GetLastError}

import scala.concurrent._
import scala.concurrent.duration.Duration
import scala.tools.nsc.dependencies.Files

@RunWith(classOf[JUnitRunner])
class LocalisationManagerSpec extends Specification with Mockito {

  class LocalisationManagerTest extends LocalisationManagerLike

  case class matchRegex(a: String) extends Matcher[String]() {
    override def apply[S <: String](t: Expectable[S]): MatchResult[S] = result(a.r.findFirstIn(t.value).nonEmpty, "okMessage", t.value + "not found " + a, t)
  }

  case class contains(a: String, b: Int) extends Matcher[String]() {
    override def apply[S <: String](t: Expectable[S]): MatchResult[S] = result(a.r.findAllMatchIn(t.value).size == b, "okMessage", "not found " + b + " times but " + a.r.findAllMatchIn(t.value).size + " times " + a, t)
  }

  val bson = BSONObjectID.generate
  val bson2 = BSONObjectID.generate

  def fixture = new {
    val localisationDaoMock=mock[LocalisationDao]
    val appMock=mock[play.api.Application]
    val controller=new LocalisationManagerTest{
      override val localisationDao:LocalisationDao=localisationDaoMock
      override val app=appMock
    }
  }

  "When user is not connected, LocalisationManager" should {
    "redirect to login for resource /campaigns/localisations" in new WithApplication {
      route(FakeRequest(GET, "/campaigns/localisations")).map(
        r => {
          status(r) must equalTo(SEE_OTHER)
          header("Location", r) must equalTo(Some("/login"))
        }
      ).getOrElse(
          failure("Pas de retour de la fonction")
        )
    }

    "redirect to login for resource /campaigns/localisations/localisation" in new WithApplication {
      route(FakeRequest(GET, "/campaigns/localisations/localisation")).map(
        r => {
          status(r) must equalTo(SEE_OTHER)
          header("Location", r) must equalTo(Some("/login"))
        }
      ).getOrElse(
          failure("Pas de retour de la fonction")
        )
    }

    "redirect to login for resource /campaigns/localisations/localisation" in new WithApplication {
      val f=fixture
      val formData=MultipartFormData[TemporaryFile](dataParts = Map(), files = Seq(), badParts = Seq(), missingFileParts = Seq())
      f.controller.addLocalisation()(FakeRequest(POST, "/campaigns/localisations/localisation").withMultipartFormDataBody(formData)).map(
        data => {
          val r=future{data}
          status(r) must equalTo(SEE_OTHER)
          header("Location", r) must equalTo(Some("/login"))
        }
      )
    }
  }

  "When user is on resource /campaigns/localisations, LocalisationManager" should{
    "send 200 Ok page with the message 'Aucun résultat trouvé' if not localisation found" in new WithApplication{
      val f=fixture

      f.localisationDaoMock.findAll(any[JsObject],any[JsObject])(any[ExecutionContext]) returns future{List()}

      val r=f.controller.listLocalisation().apply(FakeRequest(GET,"/campaigns/localisations").withSession("user" -> """{"login":"test"}"""))

      status(r) must equalTo(OK)
      contentType(r) must beSome.which(_ == "text/html")
      contentAsString(r) must contain("<h3 style=\"text-align:center\">Aucun résultat trouvé</h3>")

      there was one(f.localisationDaoMock).findAll(any[JsObject],any[JsObject])(any[ExecutionContext])
    }

    "send 200 Ok page with 1 result" in new WithApplication{
      val f=fixture
      val localisation=Localisation(nom="unNom",lat=Some(1.2f),lon=Some(3.4f),commentaire=Some("un com"),photo=List())

      f.localisationDaoMock.findAll(any[JsObject],any[JsObject])(any[ExecutionContext]) returns future{List(localisation)}

      val r=f.controller.listLocalisation().apply(FakeRequest(GET,"/campaigns/localisations").withSession("user" -> """{"login":"test"}"""))

      status(r) must equalTo(OK)
      contentType(r) must beSome.which(_ == "text/html")
      val content=contentAsString(r)
      content must contain("unNom")
      content must contain("<div class=\"row\"><span class=\"bold\">Commentaire</span> : un com</div>")
      content must contain("<div class=\"row\"><span class=\"bold\">Latitude</span> : 1.2</div>")
      content must contain("<div class=\"row\"><span class=\"bold\">Longitude</span> : 3.4</div>")
      content must contain("<div class=\"row\"><span class=\"bold\">Photo</span> : 0</div>")

      there was one(f.localisationDaoMock).findAll(any[JsObject],any[JsObject])(any[ExecutionContext])
    }

    "send 200 Ok page with 2 result" in new WithApplication{
      val f=fixture
      val localisations=
      List(
        Localisation(nom="unNom",lat=Some(1.2f),lon=Some(3.4f),commentaire=Some("un com"),photo=List()),
        Localisation(nom="unNom2",lat=Some(5.6f),lon=Some(7.8f),commentaire=Some("un com2"),photo=List("une image","deux image"))
      )

      f.localisationDaoMock.findAll(any[JsObject],any[JsObject])(any[ExecutionContext]) returns future{localisations}

      val r=f.controller.listLocalisation().apply(FakeRequest(GET,"/campaigns/localisations").withSession("user" -> """{"login":"test"}"""))

      status(r) must equalTo(OK)
      contentType(r) must beSome.which(_ == "text/html")
      val content=contentAsString(r)
      content must contain("unNom")
      content must contain("<div class=\"row\"><span class=\"bold\">Commentaire</span> : un com</div>")
      content must contain("<div class=\"row\"><span class=\"bold\">Latitude</span> : 1.2</div>")
      content must contain("<div class=\"row\"><span class=\"bold\">Longitude</span> : 3.4</div>")
      content must contain("<div class=\"row\"><span class=\"bold\">Photo</span> : 0</div>")
      content must contain("unNom2")
      content must contain("<div class=\"row\"><span class=\"bold\">Commentaire</span> : un com2</div>")
      content must contain("<div class=\"row\"><span class=\"bold\">Latitude</span> : 5.6</div>")
      content must contain("<div class=\"row\"><span class=\"bold\">Longitude</span> : 7.8</div>")
      content must contain("<div class=\"row\"><span class=\"bold\">Photo</span> : 2</div>")

      there was one(f.localisationDaoMock).findAll(any[JsObject],any[JsObject])(any[ExecutionContext])
    }
  }

  "When user is on resource /campaigns/localisations/localisation, LocalisationManager" should{
    "send 200 Ok page with an empty form" in new WithApplication{
      val f=fixture

      val r=f.controller.addLocalisationPage().apply(FakeRequest(GET,"/campaigns/localisations/localisation").withSession("user" -> """{"login":"test"}"""))

      status(r) must equalTo(OK)
      val content=contentAsString(r)
      content must contain("<input type=\"text\" id=\"nom\" name=\"nom\" value=\"\" class=\"form-control\"/>")
      content must contain("<input type=\"text\" id=\"lat\" name=\"lat\" value=\"\" class=\"form-control\"/>")
      content must contain("<input type=\"text\" id=\"lon\" name=\"lon\" value=\"\" class=\"form-control\"/>")
      content must contain("<input name=\"photo[]\" id=\"photo\" type=\"file\" accept=\"image/*\"/>")
      content must contain("<textarea id=\"commentaire\" name=\"commentaire\" class=\"form-control\"></textarea>")
    }

    "send redirect to localisation list after insert data received" in new WithApplication{
      val f=fixture
      val file=mock[MultipartFormData.FilePart[TemporaryFile]]
      val tempFile=mock[TemporaryFile]
      val captor = ArgumentCaptor.forClass(classOf[File])
      val param=Map("nom"->Seq("unNom"),"lat"->Seq("2.3"),"lon"->Seq("3.5"),"commentaire"->Seq("un com"))
      val formData=MultipartFormData[TemporaryFile](dataParts = param, files = Seq(file), badParts = Seq(), missingFileParts = Seq())
      val lastError=mock[LastError]

      f.localisationDaoMock.findOne(org.mockito.Matchers.eq(Json.obj("nom"->"unNom")))(any[ExecutionContext]) returns future{None}
      file.filename returns "file.jpg"
      f.appMock.path returns (new File("/route"))
      file.ref returns tempFile
      org.mockito.Mockito.doNothing.when(tempFile).moveTo(any[File],any[Boolean])
      f.localisationDaoMock.insert(any[Localisation],any[GetLastError])(any[ExecutionContext]) returns future{lastError}

      val req=FakeRequest(POST,"/campaigns/localisations/localisation",FakeHeaders(),formData).withSession("user" -> """{"login":"test"}""")
      val r=f.controller.addLocalisation()(req)

      Await.result(r,Duration.Inf)

      there was one(tempFile).moveTo(captor.capture,any[Boolean])
      val arg=captor.getValue
      arg.getAbsolutePath must matchRegex("/route/public/images/campaign/[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}\\.jpg")

      status(r) must equalTo(SEE_OTHER)
      header("Location",r) must beSome("/campaigns/localisations")

      there was one(f.localisationDaoMock).findOne(org.mockito.Matchers.eq(Json.obj("nom"->"unNom")))(any[ExecutionContext])
      there was one(file).filename
      there was one(f.appMock).path
      there was 2.times(file).ref
      there was one(f.localisationDaoMock).insert(any[Localisation],any[GetLastError])(any[ExecutionContext])
    }
  }

  "When method insertImage is called, LocalisationManager" should{
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

      arg.getAbsolutePath must matchRegex("/route/public/images/campaign/[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}\\.jpg")
      r must equalTo(List(arg.getAbsolutePath.split("/").last))

      there was one(file).filename
      there was one(f.appMock).path
      there was 2.times(file).ref
    }
  }

  "When method submitForm is called, LocalisationManager" should{
    "send bad request with an empty form if it was submit with empty field" in new WithApplication{
      val f=fixture
      val route=routes.LocalisationManager.listLocalisation()
      val verif=mock[LocalisationForm=>JsObject]
      val func=mock[(LocalisationForm,List[String])=>Future[Result]]
      val formData=MultipartFormData[TemporaryFile](dataParts = Map(), files = Seq(), badParts = Seq(), missingFileParts = Seq())

      val req=FakeRequest(POST,"/campaigns/localisations/localisation",FakeHeaders(),formData).withSession("user" -> """{"login":"test"}""")
      val r=f.controller.submitForm(route)(verif)(func)(req)

      status(r) must equalTo(BAD_REQUEST)
      contentAsString(r) must contain("<span class=\"control-label errors\">This field is required</span>")

      there was no(verif).apply(any[LocalisationForm])
      there was no(func).apply(any[LocalisationForm],any[List[String]])
    }

    "send bad request if localisation found" in new WithApplication{
      val f=fixture
      val route=routes.LocalisationManager.listLocalisation()
      val verif=mock[LocalisationForm=>JsObject]
      val func=mock[(LocalisationForm,List[String])=>Future[Result]]
      val formData=MultipartFormData[TemporaryFile](dataParts = Map("nom"->Seq("unNom")), files = Seq(), badParts = Seq(), missingFileParts = Seq())
      val localisation=mock[Localisation]

      verif.apply(any[LocalisationForm]) returns Json.obj("nom"->"unNom")
      f.localisationDaoMock.findOne(org.mockito.Matchers.eq(Json.obj("nom"->"unNom")))(any[ExecutionContext]) returns future{Some(localisation)}

      val req=FakeRequest(POST,"/campaigns/localisations/localisation",FakeHeaders(),formData).withSession("user" -> """{"login":"test"}""")
      val r=f.controller.submitForm(route)(verif)(func)(req)

      status(r) must equalTo(BAD_REQUEST)
      contentAsString(r) must contain("<div class=\"alert alert-danger\" role=\"alert\">Cette localisation existe déjà</div>")

      there was one(f.localisationDaoMock).findOne(org.mockito.Matchers.eq(Json.obj("nom"->"unNom")))(any[ExecutionContext])
      there was one(verif).apply(any[LocalisationForm])
      there was no(func).apply(any[LocalisationForm],any[List[String]])
    }

    "execute dedicated function if localisation not found" in new WithApplication{
      val f=fixture
      val route=routes.LocalisationManager.listLocalisation()
      val verif=mock[LocalisationForm=>JsObject]
      val func=mock[(LocalisationForm,List[String])=>Future[Result]]
      val formData=MultipartFormData[TemporaryFile](dataParts = Map("nom"->Seq("unNom")), files = Seq(), badParts = Seq(), missingFileParts = Seq())

      func.apply(any[LocalisationForm],any[List[String]]) returns future{Results.Ok("exec func")}
      verif.apply(any[LocalisationForm]) returns Json.obj("nom"->"unNom")
      f.localisationDaoMock.findOne(org.mockito.Matchers.eq(Json.obj("nom"->"unNom")))(any[ExecutionContext]) returns future{None}

      val req=FakeRequest(POST,"/campaigns/localisations/localisation",FakeHeaders(),formData).withSession("user" -> """{"login":"test"}""")
      val r=f.controller.submitForm(route)(verif)(func)(req)

      status(r) must equalTo(OK)
      contentAsString(r) must equalTo("exec func")

      there was one(f.localisationDaoMock).findOne(org.mockito.Matchers.eq(Json.obj("nom"->"unNom")))(any[ExecutionContext])
      there was one(verif).apply(any[LocalisationForm])
      there was one(func).apply(any[LocalisationForm],any[List[String]])
    }
  }
}
