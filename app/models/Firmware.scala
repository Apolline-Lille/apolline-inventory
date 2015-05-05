package models

import play.api.libs.json.{Json, Format}
import play.modules.reactivemongo.ReactiveMongoPlugin
import reactivemongo.bson.BSONObjectID
import reactivemongo.extensions.json.dao.JsonDao
import play.modules.reactivemongo.json.BSONFormats.BSONObjectIDFormat
import play.api.Play.current
import scala.concurrent.ExecutionContext.Implicits.global

case class Firmware(_id:BSONObjectID,nom:String,version:String)

object Firmware{
  implicit val firmwareFormat:Format[Firmware]=Json.format[Firmware]
}

abstract class FirmwareDao extends JsonDao[Firmware, BSONObjectID](ReactiveMongoPlugin.db, "firmware"){
  this:JsonDao[Firmware, BSONObjectID]=>

}

object FirmwareDaoObj extends FirmwareDao
