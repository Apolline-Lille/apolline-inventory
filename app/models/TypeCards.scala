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


case class TypeCards(_id:BSONObjectID=BSONObjectID.generate,modele:String,types:String,delete:Boolean=false)

object TypeCards{
  implicit val typeCardsFormat:Format[TypeCards]=Json.format[TypeCards]
}

abstract class TypeCardsDao extends JsonDao[TypeCards, BSONObjectID](ReactiveMongoPlugin.db, "typeCards"){
  this:JsonDao[TypeCards, BSONObjectID]=>

  def findListModele():Future[Stream[BSONDocument]]=collection.db.command(Aggregate(collection.name, Seq(Match(BSONDocument("delete"->false)),GroupField("modele")("count" -> SumValue(1)))))
  def findListType(selector:BSONDocument):Future[Stream[BSONDocument]]=collection.db.command(Aggregate(collection.name, Seq(Match(selector),GroupField("types")("count" -> SumValue(1)))))
}

object TypeCardsDaoObj extends TypeCardsDao