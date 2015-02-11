package models

import reactivemongo.api._

case class User(username : String,
                password: String)

object User {
}