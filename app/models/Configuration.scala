package models

import play.api.libs.json.{Json, Format}
import play.modules.reactivemongo.ReactiveMongoPlugin
import reactivemongo.bson.BSONObjectID
import reactivemongo.extensions.json.dao.JsonDao
import play.modules.reactivemongo.json.BSONFormats.BSONObjectIDFormat
import play.api.Play.current
import scala.concurrent.ExecutionContext.Implicits.global


case class Configuration(_id:BSONObjectID=BSONObjectID.generate,port:String,timeout:Int=10000,baud:Int=9600,bits:Int=8,stopBits:Int=1,parity:Int=0,timeFilter:Int=1000,types:String,infoMesure:List[BSONObjectID])

object Configuration{
  implicit val configFormat:Format[Configuration]=Json.format[Configuration]
}

abstract class ConfigurationDao extends JsonDao[Configuration, BSONObjectID](ReactiveMongoPlugin.db, "configuration"){
  this:JsonDao[Configuration, BSONObjectID]=>

}

object ConfigurationDaoObj extends ConfigurationDao
