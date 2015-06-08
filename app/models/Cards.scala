package models

import java.util.Date

import play.api.libs.json.{Json, Format}
import play.modules.reactivemongo.ReactiveMongoPlugin
import reactivemongo.bson.{BSONDocument, BSONObjectID}
import reactivemongo.core.commands._
import reactivemongo.extensions.json.dao.JsonDao
import play.modules.reactivemongo.json.BSONFormats.BSONObjectIDFormat
import play.api.Play.current
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent._
import scala.concurrent.duration.Duration

case class Cards(_id:BSONObjectID=BSONObjectID.generate,id:String,types:BSONObjectID,firmware:BSONObjectID,acquisition:Date,firstUse:Option[Date],agregateur:Boolean,apolline:Option[String],hs:Boolean,commentaire:Option[String],delete:Boolean=false)

object Cards{
  implicit val moduleFormat:Format[Cards]=Json.format[Cards]
}

abstract class CardsDao extends JsonDao[Cards, BSONObjectID](ReactiveMongoPlugin.db, "cards"){
  this:JsonDao[Cards, BSONObjectID]=>

  def countCards():Future[Stream[BSONDocument]]=collection.db.command(Aggregate(collection.name, Seq(Match(BSONDocument("delete"->false)),GroupField("types")("count" -> SumValue(1)))))
  def findApolline():Future[Stream[BSONDocument]]=collection.db.command(Aggregate(collection.name, Seq(Match(BSONDocument("delete"->false)),GroupField("apolline")("count" -> SumValue(1)))))
  def countUsedCards(typeCards:List[TypeCards]):Future[List[(BSONObjectID,Int)]]={
    future {
      typeCards.mapConserve(types => {
        val futureRes = fold(Json.obj("delete" -> false,"types"->types._id), Json.obj(), List[BSONObjectID]())((list, card) => list :+ card._id).flatMap(
          list => ModuleDaoObj.collection.db.command(Aggregate(ModuleDaoObj.collection.name, Seq(Match(BSONDocument("delete" -> false)), Unwind("cartes"), Match(BSONDocument("cartes" -> BSONDocument("$in" -> list.toSeq))))))
        )
        val res = Await.result(futureRes, Duration.Inf)
        (types._id, res.size)
      }
      ).asInstanceOf[List[(BSONObjectID,Int)]]
    }
  }
}

object CardsDaoObj extends CardsDao