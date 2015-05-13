package models

import play.api.libs.json.{Json, Format}
import play.modules.reactivemongo.ReactiveMongoPlugin
import reactivemongo.bson.{BSONDocument, BSONObjectID}
import reactivemongo.core.commands.{SumValue, GroupField, Match, Aggregate}
import reactivemongo.extensions.json.dao.JsonDao
import play.modules.reactivemongo.json.BSONFormats.BSONObjectIDFormat
import play.api.Play.current
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

case class Firmware(_id:BSONObjectID=BSONObjectID.generate,nom:String,version:String)

object Firmware{
  implicit val firmwareFormat:Format[Firmware]=Json.format[Firmware]
}

abstract class FirmwareDao extends JsonDao[Firmware, BSONObjectID](ReactiveMongoPlugin.db, "firmware"){
  this:JsonDao[Firmware, BSONObjectID]=>

  def findFirmware():Future[Stream[BSONDocument]]=collection.db.command(Aggregate(collection.name, Seq(GroupField("nom")("count" -> SumValue(1)))))
  def findVersionFirmware():Future[Stream[BSONDocument]]=collection.db.command(Aggregate(collection.name, Seq(GroupField("version")("count" -> SumValue(1)))))
}

object FirmwareDaoObj extends FirmwareDao
