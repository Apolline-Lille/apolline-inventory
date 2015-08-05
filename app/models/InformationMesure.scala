package models

import play.api.libs.json.{Format, Json}
import play.modules.reactivemongo.ReactiveMongoPlugin
import reactivemongo.bson.BSONObjectID
import reactivemongo.extensions.json.dao.JsonDao
import play.modules.reactivemongo.json.BSONFormats.BSONObjectIDFormat
import play.api.Play.current
import scala.concurrent.ExecutionContext.Implicits.global


case class InformationMesure(_id:BSONObjectID=BSONObjectID.generate,index:Int,id:String,sensor:BSONObjectID,mesure:BSONObjectID)

object InformationMesure{
  implicit val infoMesureFormat:Format[InformationMesure]=Json.format[InformationMesure]
}

abstract class InformationMesureDao extends JsonDao[InformationMesure, BSONObjectID](ReactiveMongoPlugin.db, "informationMesure"){
  this:JsonDao[InformationMesure, BSONObjectID]=>

}

object InformationMesureDaoObj extends InformationMesureDao
