package models

import java.util.Date
import play.modules.reactivemongo.ReactiveMongoPlugin
import reactivemongo.bson.{BSONDocument, BSONObjectID}
import reactivemongo.core.commands._
import reactivemongo.extensions.json.dao.JsonDao
import play.modules.reactivemongo.json.BSONFormats.BSONObjectIDFormat
import play.api.Play.current
import play.api.libs.json._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent._
import scala.concurrent.duration.Duration

abstract class SensorInfo {
  def id: String
  def firstUse: Option[Date]
  def acquisition: Date
  def expiration: Option[Date]
  def hs: Boolean
  def commentaire: Option[String]
}

case class Sensor (
     _id:BSONObjectID=BSONObjectID.generate,
     id:String,
     types:BSONObjectID,
     firstUse:Option[Date],
     acquisition:Date,
     expiration:Option[Date],
     hs:Boolean,
     commentaire:Option[String],
     delete:Boolean=false
) extends SensorInfo

object Sensor{
  implicit val sensorFormat:Format[Sensor]=Json.format[Sensor]
}

abstract class SensorDao extends JsonDao[Sensor, BSONObjectID](ReactiveMongoPlugin.db, "sensors"){
     this:JsonDao[Sensor, BSONObjectID]=>

     def countByType():Future[Stream[BSONDocument]]={
          collection.db.command(Aggregate(collection.name, Seq(Match(BSONDocument("delete"->false)) ,GroupField("types")("count" -> SumValue(1)))))
     }

  def countUsedSensors(typeSensor:List[TypeSensor]):Future[List[(BSONObjectID,Int)]]={
    future {
      typeSensor.mapConserve(types => {
        val futureRes = fold(Json.obj("delete" -> false,"types"->types._id), Json.obj(), List[BSONObjectID]())((list, sensor) => list :+ sensor._id).flatMap(
          list => ModuleDaoObj.collection.db.command(Aggregate(ModuleDaoObj.collection.name, Seq(Match(BSONDocument("delete" -> false)), Unwind("capteurs"), Match(BSONDocument("capteurs" -> BSONDocument("$in" -> list.toSeq))))))
        )
        val res = Await.result(futureRes, Duration.Inf)
        (types._id, res.size)
      }
      ).asInstanceOf[List[(BSONObjectID,Int)]]
    }
  }
}

object SensorDaoObj extends SensorDao