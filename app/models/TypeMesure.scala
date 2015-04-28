package models

import play.api.libs.json.{ Format, Json}
import play.modules.reactivemongo.ReactiveMongoPlugin
import reactivemongo.bson.{BSONDocument, BSONObjectID}

import play.modules.reactivemongo.json.BSONFormats.BSONObjectIDFormat
import play.api.Play.current

import reactivemongo.extensions.json.dao.JsonDao
import scala.concurrent.ExecutionContext.Implicits.global

case class TypeMesure(_id:BSONObjectID=BSONObjectID.generate,nom:String,unite:String)

object TypeMesure{
  implicit val typeFormat:Format[TypeMesure]=Json.format[TypeMesure]
}

abstract class TypeMesureDao extends JsonDao[TypeMesure, BSONObjectID](ReactiveMongoPlugin.db, "typeMesure"){
  this:JsonDao[TypeMesure, BSONObjectID]=>


}

object TypeMesureDaoObj extends TypeMesureDao
