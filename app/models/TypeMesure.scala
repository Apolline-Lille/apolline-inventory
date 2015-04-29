package models

import play.api.libs.json.{ Format, Json}
import play.modules.reactivemongo.ReactiveMongoPlugin
import reactivemongo.bson.{BSONDocument, BSONObjectID}

import play.modules.reactivemongo.json.BSONFormats.BSONObjectIDFormat
import play.api.Play.current
import reactivemongo.core.commands.{SumValue, GroupField, Match, Aggregate}

import reactivemongo.extensions.json.dao.JsonDao
import scala.concurrent.ExecutionContext.Implicits.global

case class TypeMesure(_id:BSONObjectID=BSONObjectID.generate,nom:String,unite:String)

object TypeMesure{
  implicit val typeFormat:Format[TypeMesure]=Json.format[TypeMesure]
}

abstract class TypeMesureDao extends JsonDao[TypeMesure, BSONObjectID](ReactiveMongoPlugin.db, "typeMesure"){
  this:JsonDao[TypeMesure, BSONObjectID]=>

  def findListMesure(key:String)=collection.db.command(Aggregate(collection.name, Seq(GroupField(key)("count" -> SumValue(1)))))
  def findListUnite(key:String)=collection.db.command(Aggregate(collection.name, Seq(GroupField(key)("count" -> SumValue(1)))))
}

object TypeMesureDaoObj extends TypeMesureDao
