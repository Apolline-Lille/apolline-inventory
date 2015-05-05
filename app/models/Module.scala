package models

import java.util.Date

import play.api.libs.json.{Json, Format}
import play.modules.reactivemongo.ReactiveMongoPlugin
import reactivemongo.bson.BSONObjectID
import reactivemongo.extensions.json.dao.JsonDao

case class Module(_id:BSONObjectID=BSONObjectID.generate,id:String,types:BSONObjectID,firmware:BSONObjectID,acquisition:Date,firstUse:Option[Date],agregateur:Boolean,apolline:Option[String],hs:String,commentaire:Option[String],delete:Boolean=false)

object Module{
  implicit val moduleFormat:Format[Module]=Json.format[Module]
}

abstract class ModuleDao extends JsonDao[Module, BSONObjectID](ReactiveMongoPlugin.db, "module"){
  this:JsonDao[Module, BSONObjectID]=>

}

object ModuleDaoObj extends ModuleDao