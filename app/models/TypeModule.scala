package models

import play.api.libs.json.{Json, Format}
import play.modules.reactivemongo.ReactiveMongoPlugin
import reactivemongo.bson.BSONObjectID
import reactivemongo.extensions.json.dao.JsonDao


case class TypeModule(_id:BSONObjectID=BSONObjectID.generate,modele:String,types:String,delete:Boolean=false)

object TypeModule{
  implicit val typeModuleFormat:Format[TypeModule]=Json.format[TypeModule]
}

abstract class TypeModuleDao extends JsonDao[TypeModule, BSONObjectID](ReactiveMongoPlugin.db, "typeModule"){
  this:JsonDao[TypeModule, BSONObjectID]=>

}

object TypeModuleDaoObj extends TypeModuleDao