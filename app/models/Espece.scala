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

case class Espece(
     _id:BSONObjectID=BSONObjectID.generate,
     espece:String,
     mesure:BSONObjectID,
     min:Float,
     max:Float
)

object Espece{
  implicit val especeFormat:Format[Espece]=Json.format[Espece]
}

abstract class EspeceDao extends JsonDao[Espece, BSONObjectID](ReactiveMongoPlugin.db, "espece"){
  this:JsonDao[Espece, BSONObjectID]=>

  def findListEspece():Future[Stream[BSONDocument]]=collection.db.command(Aggregate(collection.name, Seq(Match(BSONDocument("delete"->false)),Unwind("espece"),GroupField("espece")("count" -> SumValue(1)))))
}

object EspeceDaoObj extends EspeceDao