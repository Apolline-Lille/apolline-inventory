package models

import play.api.libs.json.{Json, Format}
import play.modules.reactivemongo.ReactiveMongoPlugin
import reactivemongo.bson.{BSONDocument, BSONObjectID}
import reactivemongo.core.commands.{Aggregate, Match, GroupField, SumValue}
import reactivemongo.extensions.json.dao.JsonDao
import play.modules.reactivemongo.json.BSONFormats.BSONObjectIDFormat
import play.api.Play.current
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future


case class TypeModule(_id:BSONObjectID=BSONObjectID.generate,modele:String,types:String,delete:Boolean=false)

object TypeModule{
  implicit val typeModuleFormat:Format[TypeModule]=Json.format[TypeModule]
}

abstract class TypeModuleDao extends JsonDao[TypeModule, BSONObjectID](ReactiveMongoPlugin.db, "typeModule"){
  this:JsonDao[TypeModule, BSONObjectID]=>

  def findListModele():Future[Stream[BSONDocument]]=collection.db.command(Aggregate(collection.name, Seq(Match(BSONDocument("delete"->false)),GroupField("modele")("count" -> SumValue(1)))))
  def findListType():Future[Stream[BSONDocument]]=collection.db.command(Aggregate(collection.name, Seq(Match(BSONDocument("delete"->false)),GroupField("types")("count" -> SumValue(1)))))
}

object TypeModuleDaoObj extends TypeModuleDao