package models

import java.util.Date

import play.api.libs.json._
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

  /**
   * This method find modules state
   * @param modules List of modules
   * @return A future with the list of modules the state associat
   */
  def findModulesState(modules:List[BSONObjectID])= {
    val query = Json.obj("modules" -> Json.obj("$in" -> modules),"$or"->JsArray(Seq(
      Json.obj("dateFin"->Json.obj("$exists"->false)),
      Json.obj("dateDebut"->Json.obj("$lt"->new Date),"dateFin"->Json.obj("$gt"->new Date))
    )))
    //Find conditions associat to modules
    fold(query, Json.obj(), Map[BSONObjectID, BSONObjectID]()) { (maps, cond) => maps + (cond._id -> cond.modules) }.flatMap(
      modules => {

        //Get the list of conditions
        val conditions = modules.keys.toList

        //Associat condition to module
        CampagneDaoObj.fold(Json.obj("conditions" -> Json.obj("$in" -> conditions)), Json.obj(), Map[BSONObjectID, String]()) { (maps, camp) => camp.conditions.intersect(conditions) map (c => modules(c) -> camp.types) toMap }
      }
    )
  }
}

/**
 * This object represent the DAO for condition
 */
object ConditionDaoObj extends ConditionDao