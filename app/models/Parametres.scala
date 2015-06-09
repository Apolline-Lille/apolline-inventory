package models

import play.api.libs.json.{Json, Format}
import play.modules.reactivemongo.ReactiveMongoPlugin
import reactivemongo.bson.BSONObjectID
import reactivemongo.extensions.json.dao.JsonDao
import play.modules.reactivemongo.json.BSONFormats.BSONObjectIDFormat

import play.api.Play.current
import scala.concurrent.ExecutionContext.Implicits.global

case class Parametres(_id:BSONObjectID=BSONObjectID.generate,cle:String,valeur:String)

object Parametres{
  implicit val parametresFormat:Format[Parametres]=Json.format[Parametres]
}

abstract class ParametresDao extends JsonDao[Parametres, BSONObjectID](ReactiveMongoPlugin.db, "parametres"){
  this:JsonDao[Parametres, BSONObjectID]=>

}

object ParametresDaoObj extends ParametresDao