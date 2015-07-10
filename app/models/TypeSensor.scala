package models

import scala.concurrent.Future
import play.modules.reactivemongo.ReactiveMongoPlugin
import reactivemongo.bson.{BSONDocument, BSONObjectID}
import reactivemongo.core.commands._
import reactivemongo.extensions.json.dao.JsonDao
import play.modules.reactivemongo.json.BSONFormats.BSONObjectIDFormat
import play.api.Play.current
import play.api.libs.json._
import scala.concurrent.ExecutionContext.Implicits.global

case class TypeSensor(
     _id:BSONObjectID=BSONObjectID.generate,
     nomType:String,
     modele:String,
     mesure:BSONObjectID,
     fabricant:String,
     nbSignaux:Int,
     espece:List[String],
     min:Float,
     max:Float,
     delete:Boolean=false
)

object TypeSensor{
  implicit val typeFormat:Format[TypeSensor]=Json.format[TypeSensor]
}

abstract class TypeSensorDao extends JsonDao[TypeSensor, BSONObjectID](ReactiveMongoPlugin.db, "typeSensors"){
  this:JsonDao[TypeSensor, BSONObjectID]=>

  def findListModele(key:String):Future[Stream[BSONDocument]]=collection.db.command(Aggregate(collection.name, Seq(Match(BSONDocument("delete"->false)),GroupField(key)("count" -> SumValue(1)))))
  def findListEspece(key:String):Future[Stream[BSONDocument]]=collection.db.command(Aggregate(collection.name, Seq(Match(BSONDocument("delete"->false)),Unwind(key),GroupField(key)("count" -> SumValue(1)))))
  def findListFabricant(key:String):Future[Stream[BSONDocument]]=collection.db.command(Aggregate(collection.name, Seq(Match(BSONDocument("delete"->false)),GroupField(key)("count" -> SumValue(1)))))
  def findListType(key:String):Future[Stream[BSONDocument]]=collection.db.command(Aggregate(collection.name, Seq(Match(BSONDocument("delete"->false)),GroupField(key)("count" -> SumValue(1)))))
  def findAllType(selector:BSONDocument):Future[Stream[BSONDocument]]=collection.db.command(Aggregate(collection.name, Seq(Match(selector),GroupField("nomType")("count" -> SumValue(1)))))
}

object TypeSensorDaoObj extends TypeSensorDao