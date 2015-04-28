package models

import reactivemongo.api.DB
import reactivemongo.bson.BSONObjectID
import reactivemongo.core.commands.LastError
import reactivemongo.extensions.json.dao.JsonDao
import play.modules.reactivemongo.json.BSONFormats.BSONObjectIDFormat
import play.modules.reactivemongo.ReactiveMongoPlugin
import play.api.Play.current
import play.api.libs.json._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent._
import scala.util.{Failure,Success}

case class User(
     _id: BSONObjectID = BSONObjectID.generate,
     username : String,
     password: String,
     salt : String,
     modules:List[BSONObjectID]=List[BSONObjectID]()
 )

object User {
    implicit val userFormat:Format[User] = Json.format[User]
}

class UserDao(db:DB) extends JsonDao[User, BSONObjectID](db, "users"){
    def addModuleToUser(_idUser:BSONObjectID,_idModule:BSONObjectID):Future[LastError]={
        val p=promise[LastError]
        findById(_idUser).onComplete({
            case Failure(e) => p failure(e)
            case Success(data) => {
                val user=data.get
                val modules=user.modules :+ _idModule
                updateById(_idUser,User(_idUser,user.username,user.password,user.salt,modules)).onComplete({
                    case Failure(e) => p failure(e)
                    case Success(lastError) => p success(lastError)
                })
            }
        })
        p future
    }

    def deleteModuleOfUser(_idUser:BSONObjectID,_idModule:BSONObjectID):Future[LastError]={
        val p=promise[LastError]
        findById(_idUser).onComplete({
            case Failure(e) => p failure(e)
            case Success(data) => {
                val user=data.get
                val modules=user.modules.filter(id=> !id.equals(_idModule))
                updateById(_idUser,User(_idUser,user.username,user.password,user.salt,modules)).onComplete({
                    case Failure(e) => p failure(e)
                    case Success(lastError) => p success(lastError)
                })
            }
        })
        p future
    }
}