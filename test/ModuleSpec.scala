import java.util.Date

import com.fasterxml.jackson.databind.JsonMappingException
import models.Module
import org.junit.runner.RunWith
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner
import reactivemongo.bson.BSONObjectID

@RunWith(classOf[JUnitRunner])
class ModuleSpec extends Specification{

  "When method toStrings was called, Module" should {
    "return string represent a module" in {
      val bson_card1=BSONObjectID.generate
      val bson_card2=BSONObjectID.generate
      val bson_sensor1=BSONObjectID.generate
      val bson_sensor2=BSONObjectID.generate
      val date=new Date
      val mod=Module(id="id",types="type",dateAssemblage=date,cartes=List(bson_card1,bson_card2),capteurs=List(bson_sensor1,bson_sensor2),commentaire=Some("un com"))
      Module.toStrings(mod) must equalTo("{\"_id\":{\"$oid\":\""+mod._id.stringify+"\"},\"id\":\"id\",\"types\":\"type\",\"dateAssemblage\":"+date.getTime+",\"cartes\":[{\"$oid\":\""+bson_card1.stringify+"\"},{\"$oid\":\""+bson_card2.stringify+"\"}],\"capteurs\":[{\"$oid\":\""+bson_sensor1.stringify+"\"},{\"$oid\":\""+bson_sensor2.stringify+"\"}],\"commentaire\":\"un com\",\"configuration\":[],\"delete\":false}")
    }

    "return string represent an empty module" in {
      val mod=Module.toStrings(Module(id="",types="",dateAssemblage = new Date,cartes=List(),capteurs=List(),commentaire=None))
      mod must contain("\"id\":\"\",\"types\":\"\"")
      mod must contain("\"cartes\":[],\"capteurs\":[],\"configuration\":[],\"delete\":false")
    }
  }

  "When method toModule was called, Module" should {
    "return Module represent by a string" in {
      val bson=BSONObjectID.generate
      val bson_card1=BSONObjectID.generate
      val bson_card2=BSONObjectID.generate
      val bson_sensor1=BSONObjectID.generate
      val bson_sensor2=BSONObjectID.generate
      val date=new Date
      val mod=Module(_id=bson,id="id",types="type",dateAssemblage=date,cartes=List(bson_card1,bson_card2),capteurs=List(bson_sensor1,bson_sensor2),commentaire=Some("un com"))

      val mod_string="{\"_id\":{\"$oid\":\""+bson.stringify+"\"},\"id\":\"id\",\"types\":\"type\",\"dateAssemblage\":"+date.getTime+",\"cartes\":[{\"$oid\":\""+bson_card1.stringify+"\"},{\"$oid\":\""+bson_card2.stringify+"\"}],\"capteurs\":[{\"$oid\":\""+bson_sensor1.stringify+"\"},{\"$oid\":\""+bson_sensor2.stringify+"\"}],\"commentaire\":\"un com\",\"configuration\":[],\"delete\":false}"
      Module.toModule(Some(mod_string)) must equalTo(mod)
    }

    "return Module represent by an empty String" in {
      val bson=BSONObjectID.generate

      val mod=Module.toModule(None)

      mod.id must equalTo("")
      mod.cartes must equalTo(List[BSONObjectID]())
      mod.capteurs must equalTo(List[BSONObjectID]())
    }

    "throw an exception if invald string" in {
      Module.toModule(Some("")) must throwA[JsonMappingException]
    }

    "throw an exception if invald string" in {
      val bson=BSONObjectID.generate
      val mod_string="{\"_ID\":{\"$oid\":\""+bson.stringify+"\"},\"id\":\"id\",\"cartes\":[],\"capteurs\":[],\"delete\":false}"

      val mod=Module.toModule(Some(mod_string))
      mod.id must equalTo("")
      mod.types must equalTo("")
      mod.cartes must equalTo(List[BSONObjectID]())
      mod.capteurs must equalTo(List[BSONObjectID]())
      mod.commentaire must beNone
    }
  }
}