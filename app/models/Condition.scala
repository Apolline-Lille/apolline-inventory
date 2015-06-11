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

abstract class Condition(_id:BSONObjectID,dateDebut:Date,dateFin:Option[Date],commentaire:Option[String],modules:BSONObjectID)
case class Test(_id:BSONObjectID=BSONObjectID.generate,dateDebut:Date,dateFin:Option[Date],commentaire:Option[String],modules:BSONObjectID) extends Condition(_id,dateDebut,dateFin,commentaire,modules)
case class Calibration(_id:BSONObjectID=BSONObjectID.generate,dateDebut:Date,dateFin:Option[Date],commentaire:Option[String],modules:BSONObjectID,params:List[BSONObjectID]) extends Condition(_id,dateDebut,dateFin,commentaire,modules)
case class Terrain(_id:BSONObjectID=BSONObjectID.generate,dateDebut:Date,dateFin:Option[Date],commentaire:Option[String],modules:BSONObjectID,localisation:BSONObjectID) extends Condition(_id,dateDebut,dateFin,commentaire,modules)

/**
 * Object used for condition class
 */
object Condition{

  def unapply(cond: Condition): Option[(String, JsValue)] = {
    val (prod: Product, sub) = cond match {
      case b: Test => (b, Json.toJson(b)(testFormat))
      case b: Calibration => (b, Json.toJson(b)(calibrationFormat))
      case b: Terrain => (b, Json.toJson(b)(terrainFormat))
    }
    Some(prod.productPrefix -> sub)
  }

  def apply(`class`: String, data: JsValue): Condition = {
    (`class` match {
      case "Test" => Json.fromJson[Test](data)(testFormat)
      case "Calibration" => Json.fromJson[Calibration](data)(calibrationFormat)
      case "Terrain" => Json.fromJson[Terrain](data)(terrainFormat)
    }).get
  }

  /**
   * Value for format Condition to JSON and JSON to Condition
   */
  implicit val conditionFormat:Format[Condition]=Json.format[Condition]

  /**
   * Value for format Test to JSON and JSON to Test
   */
  implicit val testFormat:Format[Test]=Json.format[Test]

  /**
   * Value for format Calibration to JSON and JSON to Calibration
   */
  implicit val calibrationFormat:Format[Calibration]=Json.format[Calibration]

  /**
   * Value for format Terrain to JSON and JSON to Terrain
   */
  implicit val terrainFormat:Format[Terrain]=Json.format[Terrain]
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