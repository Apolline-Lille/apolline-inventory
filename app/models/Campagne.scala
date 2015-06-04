package models

import play.api.libs.json.{Json, Format}
import play.modules.reactivemongo.ReactiveMongoPlugin
import reactivemongo.bson.BSONObjectID
import reactivemongo.extensions.json.dao.JsonDao
import play.modules.reactivemongo.json.BSONFormats.BSONObjectIDFormat

import play.api.Play.current
import scala.concurrent.ExecutionContext.Implicits.global

/**
 * This class represent a campaign
 * @param _id Uniqu id on mongDB
 * @param nom Name of the campaign
 * @param conditions List of campaign conditions
 * @param delete Flag to indicate if the campaign was delete
 */
case class Campagne(_id:BSONObjectID=BSONObjectID.generate,nom:String,conditions:List[BSONObjectID],delete:Boolean=false)

/**
 * Object used for campaign class
 */
object Campagne{
  /**
   * Value for format campaign to JSON and JSON to campaign
   */
  implicit val campagneFormat:Format[Campagne]=Json.format[Campagne]
}

/**
 * Abstract class for represent DAO for campaign
 */
abstract class CampagneDao extends JsonDao[Campagne, BSONObjectID](ReactiveMongoPlugin.db, "campagne"){
  this:JsonDao[Campagne, BSONObjectID]=>

}

/**
 * This object represent the DAO for campaign
 */
object CampagneDaoObj extends CampagneDao
