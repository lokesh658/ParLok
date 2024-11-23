package models

import com.google.inject.{Inject, Singleton}
import play.api._
import play.api.Configuration
import org.mongodb.scala.{MongoClient, MongoCollection, MongoDatabase, classTagToClassOf}
import org.mongodb.scala.bson.codecs.Macros._
import org.bson.codecs.configuration.CodecRegistries.{fromProviders, fromRegistries}
import org.mongodb.scala.MongoClient.DEFAULT_CODEC_REGISTRY
import org.mongodb.scala.bson.ObjectId

import java.util.Locale.Category
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Success, Using}


case class Address(addressLine1:String, addressLine2: String, city: String, pincode: String, state:String, country: String)

case class User(_id: ObjectId, firstName: String, lastName: String, email: String, password: String, phoneNumber: Option[String], address:Option[Address])

case class Product(_id: ObjectId, name:String, description: String, price: Double, stockQuantity: Int, category: String, imageURL: String)

case class Cart(_id: ObjectId, userId: ObjectId, productId: ObjectId, quantity: Int)

case class OrderItem(_id: ObjectId, orderId: ObjectId, productId: ObjectId, quantity: Int)

case class Order(_id: ObjectId, userId: ObjectId, orderItems: List[OrderItem], shippingAddress: Address, date: String)

@Singleton
class model @Inject() (config: Configuration) {
  val codecRegistry = fromRegistries( fromProviders(classOf[User], classOf[Product], classOf[Address], classOf[Cart], classOf[OrderItem], classOf[Order]),DEFAULT_CODEC_REGISTRY)
  val url: String = config.get[String]("Database.url")

  val mongoClient: Future[MongoClient] = Future{MongoClient(url)}
  val db: Future[MongoDatabase] = mongoClient.map(_.getDatabase("parlok").withCodecRegistry(codecRegistry))
  val user: Future[MongoCollection[User]] = db.map(_.getCollection("user"))
  val product: Future[MongoCollection[Product]] = db.map(_.getCollection("product"))

  def insertUser(user1: User): Future[String] = {
    user.flatMap{userCollection =>userCollection.insertOne(user1).toFuture().map(result =>result.getInsertedId.toString)}
  }
  def findUserId(email: String, password: String): Future[Option[ObjectId]] = {
    user.flatMap{userCollection => {
      userCollection.find(org.mongodb.scala.bson.BsonDocument("email" -> email, "password" -> password)).headOption().map{
        case Some(tempUser) => Some(tempUser._id)
        case None => None
      }
    }
    }
  }
  def getAllProducts(): Future[List[Product]] = {
    product.flatMap{productCollection =>
      productCollection.find().toFuture().map(_.toList)
    }
  }


}