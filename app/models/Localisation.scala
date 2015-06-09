package models

import play.api.libs.json.{Json, Format}
import play.modules.reactivemongo.ReactiveMongoPlugin
import reactivemongo.bson.BSONObjectID
import reactivemongo.extensions.json.dao.JsonDao
import play.modules.reactivemongo.json.BSONFormats.BSONObjectIDFormat

import play.api.Play.current
import scala.concurrent.ExecutionContext.Implicits.global

case class Localisation(_id:BSONObjectID=BSONObjectID.generate,nom:String,lat:Option[Float],lon:Option[Float],commentaire:Option[String],photo:List[String])

object Localisation{
  implicit val localisationFormat:Format[Localisation]=Json.format[Localisation]
}

abstract class LocalisationDao extends JsonDao[Localisation, BSONObjectID](ReactiveMongoPlugin.db, "localisation"){
  this:JsonDao[Localisation, BSONObjectID]=>

}

object LocalisationDaoObj extends LocalisationDao