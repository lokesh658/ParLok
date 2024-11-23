package models

import com.google.inject.{Inject, Singleton}
import play.api._
import play.api.Configuration
import org.mongodb.scala.{MongoClient, MongoCollection, MongoDatabase, classTagToClassOf}
import org.mongodb.scala.bson.codecs.Macros._
import org.bson.codecs.configuration.CodecRegistries.{fromProviders, fromRegistries}
import org.mongodb.scala.MongoClient.DEFAULT_CODEC_REGISTRY
import org.mongodb.scala.bson.ObjectId

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Success, Using}


case class Address(street:String, city: String, pincode: String, state:String, country: String)

case class User(_id:ObjectId,firstName: String, lastName: String, email: String, password: String, phoneNumber: Option[String], address:Option[Address])

case class Product(name:String, description: String, price: Double, stock_quantity:Int)

@Singleton
class model @Inject() (config: Configuration) {
  val codecRegistry = fromRegistries( fromProviders(classOf[User], classOf[Product], classOf[Address]),DEFAULT_CODEC_REGISTRY)
  val url: String = config.get[String]("Database.url")

  val mongoClient: Future[MongoClient] = Future{MongoClient(url)}
  val db: Future[MongoDatabase] = mongoClient.map(_.getDatabase("parlok").withCodecRegistry(codecRegistry))
  val user: Future[MongoCollection[User]] = db.map(_.getCollection("user"))

  def insertUser(user1: User): Future[String] = {
    user.flatMap{userCollection =>userCollection.insertOne(user1).toFuture().map(result =>result.getInsertedId.toString)}
  }
  def findUserId(email: String): Future[Option[ObjectId]]={
    user.flatMap{userCollection => {
      userCollection.find(org.mongodb.scala.bson.BsonDocument("email" -> email)).headOption().map{
        case Some(tempUser) => Some(tempUser._id)
        case None => None
      }
    }
    }
  }
}