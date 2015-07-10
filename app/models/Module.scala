package models

import java.util.Date

import play.api.libs.json.{Json, Format}
import play.modules.reactivemongo.ReactiveMongoPlugin
import reactivemongo.bson.{BSONDocument, BSONObjectID}
import reactivemongo.core.commands.{SumValue, GroupField, Match, Aggregate}
import reactivemongo.extensions.json.dao.JsonDao
import play.modules.reactivemongo.json.BSONFormats.BSONObjectIDFormat
import play.api.Play.current
import scala.concurrent.ExecutionContext.Implicits.global

/**
 * This class represent a module
 * @param _id Uniqu id on mongoDB
 * @param id Module id
 * @param types Module type
 * @param dateAssemblage Date when the module was assembly
 * @param cartes List of cards associated to the module
 * @param capteurs List of sensors associated to the module
 * @param commentaire Comment about the module
 * @param delete Flag to indicate if the module was delete
 */
case class Module(_id:BSONObjectID=BSONObjectID.generate,id:String,types:String,dateAssemblage:Date,cartes:List[BSONObjectID],capteurs:List[BSONObjectID],commentaire:Option[String],delete:Boolean=false) extends Serializable

/**
 * Object used for module class
 */
object Module{
  /**
   * Value for format Module to JSON and JSON to Module
   */
  implicit val moduleFormat:Format[Module]=Json.format[Module]

  /**
   * Method for serialize a module to a string
   * @param module Module serialize
   * @return Return a string represent the module
   */
  def toStrings(module:Module)=module match{
    case Module(_,_,_,_,_,_,_,_) =>Json.stringify(Json.toJson(module))
    case _ => Json.stringify(Json.toJson(Module(id="",types="",dateAssemblage=new Date,cartes=List(),capteurs=List(),commentaire=None)))
  }

  /**
   * Method for unserialize string to a module
   * @param module Module unserialize
   * @return Return a module
   */
  def toModule(module:Option[String])=module match{
    case None => Module(id="",types="",dateAssemblage=new Date,cartes=List(),capteurs=List(),commentaire=None)
    case Some(json) => Module.moduleFormat.reads(Json.parse(json)).getOrElse(Module(id="",types="",dateAssemblage=new Date,cartes=List(),capteurs=List(),commentaire=None))
  }
}

/**
 * Abstract class for represent DAO for module
 */
abstract class ModuleDao extends JsonDao[Module, BSONObjectID](ReactiveMongoPlugin.db, "modules"){
  this:JsonDao[Module, BSONObjectID]=>

  /**
   * This method return the list of module type
   * @return Return the list of module type
   */
  def findListType()=collection.db.command(Aggregate(collection.name, Seq(Match(BSONDocument("delete"->false)) ,GroupField("types")("count" -> SumValue(1)))))

  def findSensorState(sensors:List[BSONObjectID])=fold(Json.obj("delete"->false,"capteurs"->Json.obj("$in"->sensors)),Json.obj(),Map[BSONObjectID,List[BSONObjectID]]()){
    (maps,mod)=>maps + (mod._id->mod.capteurs.intersect(sensors))
  }.flatMap(
      sensors=>ConditionDaoObj.findModulesState(sensors.keys.toList).map(
        data=>{
          data.keys.foldLeft(Map[BSONObjectID,String]()){(maps,k) => maps ++ (sensors(k) map (s=> s->data(k) )) }
        }
      )
    )

  def findCardState(cards:List[BSONObjectID])=fold(Json.obj("delete"->false,"cartes"->Json.obj("$in"->cards)),Json.obj(),Map[BSONObjectID,List[BSONObjectID]]()){
    (maps,mod)=>maps + (mod._id->mod.cartes.intersect(cards))
  }.flatMap(
      cards=>ConditionDaoObj.findModulesState(cards.keys.toList).map(
        data=>{
          data.keys.foldLeft(Map[BSONObjectID,String]()){(maps,k) => maps ++ (cards(k) map (s=> s->data(k) )) }
        }
      )
    )
}

/**
 * This object represent the DAO for module
 */
object ModuleDaoObj extends ModuleDao


