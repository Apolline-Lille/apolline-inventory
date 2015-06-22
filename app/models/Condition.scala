package models

import java.util.Date

import play.api.libs.json.{JsObject, JsValue, Json, Format}
import play.modules.reactivemongo.ReactiveMongoPlugin
import reactivemongo.bson.BSONObjectID
import reactivemongo.extensions.json.dao.JsonDao
import play.modules.reactivemongo.json.BSONFormats.BSONObjectIDFormat
import play.api.Play.current
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.parsing.json.JSONObject

case class Condition(_id:BSONObjectID=BSONObjectID.generate,dateDebut:Date,dateFin:Option[Date],commentaire:Option[String],modules:BSONObjectID)

/**
 * Object used for condition class
 */
object Condition{

  /**
   * Value for format Condition to JSON and JSON to Condition
   */
  implicit val conditionFormat:Format[Condition]=Json.format[Condition]

}

/**
 * Abstract class for represent DAO for condition
 */
abstract class ConditionDao extends JsonDao[Condition, BSONObjectID](ReactiveMongoPlugin.db, "conditions"){
  this:JsonDao[Condition, BSONObjectID]=>

}

/**
 * This object represent the DAO for condition
 */
object ConditionDaoObj extends ConditionDao